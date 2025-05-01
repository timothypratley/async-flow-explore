(ns elk
  (:require [clojure.data.json :as json])
  (:import (org.eclipse.elk.core RecursiveGraphLayoutEngine)
           (org.eclipse.elk.core.util BasicProgressMonitor)
           (org.eclipse.elk.graph ElkNode)
           (org.eclipse.elk.graph.json ElkGraphJson)))

(defn ^ElkNode elk [g]
  (-> (json/write-str g)
      (ElkGraphJson/forGraph)
      (.toElk)))

(defn unelk [^ElkNode g]
  (-> (ElkGraphJson/forGraph g)
      (.toJson)
      (json/read-str {:key-fn keyword})))

(defn layout [g]
  (let [g (elk g)]
    (.layout (RecursiveGraphLayoutEngine.) g (BasicProgressMonitor.))
    (unelk g)))
