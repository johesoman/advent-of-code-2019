(ns day-3)

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [k (f v)])
                m)))

(def directions
  {"U" [0 1]
   "D" [0 -1]
   "L" [-1 0]
   "R" [1 0]})

(defn expand-move [s]
  (repeat (Integer/parseInt (subs s 1))
          (get directions (subs s 0 1))))

(defn do-move [{:keys [steps pos wire]} dir]
  (let [[x1 y1] pos
        [x2 y2] dir
        pos [(+ x1 x2) (+ y1 y2)]]
    {:pos pos
     :steps (inc steps)
     :wire (assoc wire pos steps)}))

(defn string->wire [s]
  (->> (clojure.string/split s #",")
       (mapcat expand-move)
       (reduce do-move {:steps 1 :pos [0 0] :wire {}})
       :wire
       (map-keys vector)))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn file->line-intersections [path]
  (->> (line-seq (clojure.java.io/reader path))
       (map string->wire)
       (apply merge-with concat)
       (filter (fn [[_ v]] (< 1 (count v))))))

(defn closest-by [f m]
  (->> (map (fn [[k v]] [k (f k v)]) m)
       (sort-by second)
       first))

(defn day-3-part-1 []
  (->> (file->line-intersections "input/day_3.txt")
       (closest-by (fn [pos & _] (manhattan-distance pos)))))

(defn day-3-part-2 []
  (->> (file->line-intersections "input/day_3.txt")
       (closest-by #(apply + %2))))

