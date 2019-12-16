(ns core)

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

(defn non-decreasing-digits [n]
  (letfn [(go [start n]
            (if (< n 1)
              [[]]
              (for [x (range (int start) (+ (int \9) 1))
                    xs (go x (dec n))]
                (cons (char x) xs))))]
    (->> (go \0 n)
         (map #(apply str %1)))))

(defn adj-groups [group-size xs]
  (->> (partition group-size 1 xs)
       (filter #(apply = %1))
       (map first)
       set))

(defn count-codes [f]
  (->> (non-decreasing-digits 6)
       (filter f)
       (map #(Integer/parseInt %1))
       (filter #(<= 387638 %1 919123))
       count))

(defn has-two-group? [xs]
  (adj-groups 2 xs))

(defn has-two-group-but-not-three-group? [xs]
  (let [two-groups (adj-groups 2 xs)
        three-groups (adj-groups 3 xs)]
    (->> (filter #(not (get three-groups %1)) two-groups)
         first)))

(defn day4-part1 []
  (count-codes has-two-group?))

(defn day4-part2 []
  (count-codes has-two-group-but-not-three-group?))
