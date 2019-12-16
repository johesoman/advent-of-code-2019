(ns day-2)

(defn string->program [s]
  (->> (clojure.string/split s #",")
       (map #(Integer/parseInt %1))
       vec))

(defn do-op [program counter]
  (let [[op i j k] (->> program
                        (drop (* counter 4))
                        (take 4))
        x (get program i)
        y (get program j)]
    (condp = op
      1 (assoc program k (+ x y))
      2 (assoc program k (* x y))
      nil)))

(defn run-program [program]
  (loop [counter 0
         program program]
    (if-let [program (do-op program counter)]
      (recur (inc counter) program)
      (get program 0))))

(defn day-2-part-1 []
  (-> (line-seq (clojure.java.io/reader "input/day_2.txt"))
      first
      string->program
      (assoc 1 12)
      (assoc 2 2)
      run-program))

(defn cartesian-product [[i j] [k l]]
  (for [x (range i j) y (range k l)]
      [x y]))

(defn run-program-with [program noun verb]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)
      run-program))

(defn day-2-part-2 []
  (let [program (-> (line-seq (clojure.java.io/reader "input/day_2.txt"))
                    first
                    string->program)]
    (->> (cartesian-product [0 100] [0 100])
         (map (fn [[x y]] [x y (run-program-with program x y)]))
         (filter (fn [[_ _ z]] (= z 19690720)))
         first)))

