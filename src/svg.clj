(ns svg
  (:require [clojure.string :as str]
            [scicloj.kindly.v4.kind :as kind]))

(def default-styles
  {:edge-shape-stroke      "black"
   :edge-shape-fill        "none"
   :node-shape-stroke      "black"
   :node-shape-fill        "white"
   :node-label-stroke      "none"
   :node-label-fill        "black"
   :node-label-font-size   "12px"
   :node-label-font-family "sans-serif"
   :port-shape-stroke      "black"
   :port-shape-fill        "white"})

(defn edge-path [{:keys [sections]}]
  (let [[a & more] (for [{:keys [startPoint bendPoints endPoint]} sections
                         {:keys [x y]} (concat [startPoint] bendPoints [endPoint])]
                     (str x "," y))]
    (str "M" a "L" (str/join " " more))))

(defn edge [edge-d]
  [:path {:d          (edge-path edge-d)
          :stroke     (:edge-shape-stroke default-styles)
          :fill       (:edge-shape-fill default-styles)
          :marker-end "url(#edgeShapeMarker)"}])

(defn edge-defs []
  [:marker {:id           "edgeShapeMarker"
            :markerWidth  10
            :markerHeight 10
            :refX         6
            :refY         3
            :orient       "auto"
            :markerUnits  "strokeWidth"}
   [:path {:d    "M0,0 L0,6 L6,3 z"
           :fill (:edge-shape-stroke default-styles)}]])

(defn node-shape [{:keys [x y width height]}]
  [:rect {:x      x
          :y      y
          :width  width
          :height height
          :stroke (:node-shape-stroke default-styles)
          :fill   (:node-shape-fill default-styles)}])

(defn node-label [label n]
  [:text {:x                  (+ (:x n) (:x label))
          :y                  (+ (:y n) (:y label))
          :stroke             (:node-label-stroke default-styles)
          :fill               (:node-label-fill default-styles)
          :style              {:font (str (:node-label-font-size default-styles) " "
                                          (:node-label-font-family default-styles))}
          :text-anchor        "left"
          :alignment-baseline "hanging"}
   (:text label)])

(defn node-port [{:as port :keys [x y width height]} node]
  [:rect {:x      (+ (:x node) x)
          :y      (+ (:y node) y)
          :width  width
          :height height
          :stroke (:port-shape-stroke default-styles)
          :fill   (:port-shape-fill default-styles)}])

(defn node [{:as n :keys [labels ports children edges x y]} parent]
  [:g {:style {:transform (str "translate(" (:x parent) ", " (:y parent) ")")}}
   [:g (node-shape n)]
   [:g (for [l labels]
         (node-label l n))]
   [:g (for [p ports]
         (node-port p n))]
   [:g (for [c children]
         (node c n))]
   [:g {:style {:transform (str "translate(" x ", " y ")")}}
    (for [e edges]
      (edge e))]])

(defn render-graph [{:as g :keys [x y width height children edges]}]
  (kind/hiccup
    [:div {:style {:overflow-x :scroll}}
     [:svg {:view-box (str (or x 0) " " (or y 0) " " width " " height)
            :width    "100%"
            :height   "100%"}
      [:defs (edge-defs)]
      [:g (for [c children]
            (node c g))]
      [:g (for [e edges]
            (edge e))]]]))
