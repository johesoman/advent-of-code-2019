(ns day-1)

(defn day-1-part-1 []
  (->> (line-seq (clojure.java.io/reader "input/day_1.txt"))
       (map #(- (long (/ (Integer/parseInt %1) 3)) 2))
       (apply +)))

(defn fuel-requirements [mass]
  (->> mass
       (iterate #(- (long (/ %1 3)) 2))
       (drop 1)
       (take-while #(< 0 %1))
       (apply +)))

(defn day-1-part-2 []
  (->> (line-seq (clojure.java.io/reader "input/day_1.txt"))
       (map #(Integer/parseInt %1))
       (map fuel-requirements)
       (apply +)))
