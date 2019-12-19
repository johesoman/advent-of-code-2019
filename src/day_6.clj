(ns day-6
  (:require [clojure.data.priority-map :refer [priority-map]]))

;; +++++++++
;; + Graph +
;; +++++++++

(defn ->graph [adjs]
  (->> (concat adjs (map (comp vec reverse) adjs))
       (map (fn [[k v]] {k [v]}))
       (apply merge-with concat)))

(def neighbors get)

;; ++++++++++++++++++++++++
;; + Dijkstra's algorithm +
;; ++++++++++++++++++++++++

(defn minimum [a b]
  (if (and a b) (min a b) (or a b)))

(defn dijkstra [graph start]
  (loop [dists {}
         visited #{start}
         q (priority-map start 0)]
    (if (empty? q)
      dists
      (let [[u d] (peek q)
            vs (remove visited
                       (neighbors graph u))
            ds (map #(minimum (+ d 1) (get dists %1))
                    vs)
            vds (map vector vs ds)]
        (recur (into dists vds)
               (into visited vs)
               (into (pop q) vds))))))

;; ++++++++++++
;; + Solution +
;; ++++++++++++

(def graph
  (->> "input/day_6.txt"
       clojure.java.io/reader
       line-seq
       (map #(clojure.string/split %1 #"\)"))
       ->graph))

(defn part-1 []
  (->> (dijkstra graph "COM")
       vals
       (apply +)))

(defn part-2 []
  (- (get (dijkstra graph "YOU") "SAN")
     2))
