(ns async-flow-explore
  (:require [clojure.datafy :as datafy]
            [clojure.string :as str]
            [scicloj.kindly.v4.kind :as kind]
            [stats]
            [dorothy.core :as dorothy]
            [dorothy.jvm :as jvm]
            [elk :as elk]
            [svg :as svg]))

(def g
  {:id            "G"
   :layoutOptions {:algorithm :layered}
   :children      [{:id     "n1"
                    :width  10
                    :height 10}
                   {:id     "n2"
                    :width  10
                    :height 10}]
   :edges         [
                   {:id      "e1"
                    :sources ["n1"]
                    :targets ["n2"]}]})

(def g'
  (elk/layout g))

(svg/render-graph g')

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

(defn nodes [flow]
  (let [{:keys [conns procs]} (datafy/datafy flow)
        all-proc-chans (set (mapcat identity conns))]
    (for [[proc-key proc-chans] (group-by first all-proc-chans)]
      (let [{:keys [args proc]} (get procs proc-key)
            {:keys [desc]} proc
            {:keys [params ins outs]} desc]
        {:id    (id-for proc-key)
         :label (str (name proc-key)
                     \newline \newline
                     (str/join \newline
                               (for [[k param] params]
                                 (str (name k) " (" (get args k) ") " param))))}
        ;; children? ports?
        (for [[_ chan :as proc-chan] proc-chans]
          {:id     (id-for proc-chan)
           :parent (id-for proc-key)
           :label  (str (name chan)
                        \newline \newline
                        (or (get outs chan)
                            (get ins chan)))})))))

(defn flow-digraph [flow title]
  (let [{:keys [conns procs]} (datafy/datafy flow)
        all-proc-chans (set (mapcat identity conns))]
    (dorothy/digraph "G"
                     (concat
                       [{:label    title
                         :compound "true"}]
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

(def e
  {:id            "G"
   :layoutOptions {:algorithm :layered}
   :children      [{:id     "n1"
                    :width  10
                    :height 10}
                   {:id     "n2"
                    :width  10
                    :height 10}]
   :edges         [
                   {:id      "e1"
                    :sources ["n1"]
                    :targets ["n2"]}]})

(defn elkg [flow]
  (let [{:keys [conns procs]} (datafy/datafy flow)
        all-proc-chans (into #{} cat conns)]
    {:id            "G"
     :layoutOptions {:algorithm :layered}
     :children
     (for [[proc-key proc-chans] (group-by first all-proc-chans)]
       (let [{:keys [args proc]} (get procs proc-key)
             {:keys [desc]} proc
             {:keys [params ins outs]} desc]
         {:id     (id-for proc-key)
          :width  100
          :height 100
          :label  (str (name proc-key)
                       \newline \newline
                       (str/join \newline
                                 (for [[k param] params]
                                   (str (name k) " (" (get args k) ") " param))))
          :children
          (for [[_ chan :as proc-chan] proc-chans]
            {:id     (id-for proc-chan)
             :width  10
             :height 10
             :label  (str (name chan)
                          \newline \newline
                          (or (get outs chan)
                              (get ins chan)))})}))
     :edges
     (for [[from to] conns]
       {:id      (id-for [from to])
        :sources [(id-for from)]
        :targets [(id-for to)]})}))

(svg/render-graph
  (elkg f))
