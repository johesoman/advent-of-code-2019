(ns day-10)

(def input-lines
  (line-seq (clojure.java.io/reader "input/day_10.txt")))

(defn parse-asteroids [lines]
  (vec
   (for [[i cs] (map-indexed vector lines)
         [j c] (map-indexed vector cs)
         :when (not= c \.)]
     [j i])))

(defn angle-between [[x y] [z w]]
  (- Math/PI
     (Math/atan2  (- z x) (- w y))))

(defn distance-to [[x y] [z w]]
  (Math/sqrt (+ (* (- z x) (- z x))
                (* (- w y) (- w y)))))

(defn count-visible [p asteroids]
  (->> (map #(angle-between p %1) asteroids)
       set
       count))

(defn find-asteroid-with-most-visible [asteroids]
  (apply max-key second (for [p asteroids]
                          [p (count-visible p asteroids)])))

(defn part-1 [lines]
  (-> (parse-asteroids lines)
      find-asteroid-with-most-visible))

(defn partition-by-angle-between [p asteroids]
  (->> (filter #(not= %1 p) asteroids)
       (sort-by #(distance-to p %1))
       (sort-by #(angle-between p %1))
       (partition-by #(angle-between p %1))))

(defn columns
  ([xss]
   (let [xss (mapv vec xss)]
     (columns xss (count xss) 0 0 0)))
  ([xss n i j nils]
   (cond
     (= nils n) nil
     (= i n) (columns xss n 0 (inc j) nils)
     :else (if-let [x (get-in xss [i j])]
             (lazy-seq (cons x (columns xss n (inc i) j nils)))
             (lazy-seq (cons nil (columns xss n (inc i) j (inc nils))))))))

(defn simulate-laser [p asteroids]
  (->> (partition-by-angle-between p asteroids)
       columns
       (remove nil?)))

(defn find-asteroid-and-simulate-laser [lines]
  (let [asteroids (parse-asteroids lines)
        p (find-asteroid-with-most-visible asteroids)]
    (simulate-laser (first p) asteroids)))

(defn part-2 [lines]
  (->> (find-asteroid-and-simulate-laser lines)
       (take 200)
       last))


