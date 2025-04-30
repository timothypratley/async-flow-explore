(ns async-flow-explore
  (:require [clojure.core.async :as async]
            [clojure.core.async.flow :as flow]
            [clojure.core.async.flow-monitor :as monitor]
            [clojure.datafy :as datafy]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [scicloj.kindly.v4.kind :as kind]
            [stats]
            [dorothy.core :as dorothy]
            [dorothy.jvm :as jvm]))

;; # Visualizing Async Flows

;; Async flows are directed graphs, presumably acyclic

;; Alex made an example (https://github.com/puredanger/flow-example/blob/main/src/stats.clj)

(def f (stats/create-flow))

(datafy/datafy f)

;; recently Flow Monitor was released, can we visualize another way?

(defn dd [g]
  (kind/html (jvm/render (dorothy/dot g) {:format :svg})))

(defn id-for [x]
  (cond (keyword? x) (str (symbol x))
        (vector? x) (str/join " " (map id-for x))
        (string? x) x
        :else (str x)))

(id-for [:foo :bar])

(defn flow-digraph [flow title]
  (let [{:keys [conns procs]} (datafy/datafy flow)
        all-proc-chans (set (mapcat identity conns))]
    (dorothy/digraph "G"
      (concat
        [{:label title}]
        ;; nodes
        ;; proc nodes are a subgraph containing chan nodes
        (for [[proc-key proc-chans] (group-by first all-proc-chans)]
          (let [{:keys [args proc]} (get procs proc-key)
                {:keys [desc]} proc
                {:keys [params ins outs]} desc]
            (dorothy/subgraph (id-for proc-key)
                              (cons {:cluster "true"
                                     :label   (str (name proc-key)
                                                   \newline \newline
                                                   (str/join \newline
                                                             (for [[k param] params]
                                                               (str (name k) " (" (get args k) ") " param))))}
                                    (for [[_ chan :as proc-chan] proc-chans]
                                      [(id-for proc-chan) {:shape "box"
                                                           :label (str (name chan)
                                                                       \newline \newline
                                                                       (or (get outs chan)
                                                                           (get ins chan)))}])))))
        ;; edges between the chans of the procs
        (for [[from to] conns]
          [(id-for from) (id-for to)])))))

(def h (flow-digraph f "Alex's example core.async.flow"))
(dd h)
(dorothy/dot* h)
