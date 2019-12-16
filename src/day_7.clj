(ns day-7)

;; +++++++++++
;; + program +
;; +++++++++++

(defn char->int [c] (- (int c) 48))

(defn get-op [{:keys [program counter]}]
  (rem (first (subvec program counter)) 100))

(defn get-params [{:keys [program counter]}]
  (take 3 (rest (subvec program counter))))

(defn get-modes [{:keys [program counter]}]
  (let [op-int (first (subvec program counter))
        xs (drop 2 (reverse (str op-int)))
        xs (concat  xs (repeat \0))]
    (take 2 (map char->int xs))))

(defn load-val [{:keys [program]} val mode]
  (if (= mode 0)
    (get program val)
    val))

(def write-val #(update %1 :program assoc %2 %3))

(def inc-counter #(update %1 :counter + %2))

(def set-counter #(assoc %1 :counter %2))

(defn read-input [{:keys [input]}]
  (if (seq input)
    (first input)
    (Integer/parseInt (read-line))))

(def write-output #(update %1 :output concat [%2]))

(def flush-input #(update %1 :input rest))

(defmulti do-op (fn [_ op-map] (:type op-map)))

(defn do-next-op [state]
  (let [op (get-op state)
        [a b c] (get-params state)
        [a-mode b-mode] (get-modes state)]
    (do-op state
           {:a a
            :b b
            :c c
            :type op
            :a-val (load-val state a a-mode)
            :b-val (load-val state b b-mode)})))

(defn run-program
  ([program] (run-program program nil))
  ([program input]
   (loop [state {:counter 0
                 :input input
                 :program program}]
     (if-let [state (do-next-op state)]
       (recur state)
       (:output state)))))

;; ++++++
;; + op +
;; ++++++

(defmethod do-op 1 [state {:keys [c a-val b-val]}]
  (-> (write-val state c (+ a-val b-val))
      (inc-counter 4)))

(defmethod do-op 2 [state {:keys [c a-val b-val]}]
  (-> (write-val state c (* a-val b-val))
      (inc-counter 4)))

(defmethod do-op 3 [state {:keys [a]}]
  (let [x (read-input state)]
    (-> (flush-input state)
        (write-val a x)
        (inc-counter 2))))

(defmethod do-op 4 [state {:keys [a-val]}]
  (-> (write-output state a-val)
      (inc-counter 2)))

(defmethod do-op 5 [state {:keys [a-val b-val]}]
  (if (not= a-val 0)
    (set-counter state b-val)
    (inc-counter state 3)))

(defmethod do-op 6 [state {:keys [a-val b-val]}]
  (if (= a-val 0)
    (set-counter state b-val)
    (inc-counter state 3)))

(defmethod do-op 7 [state {:keys [c a-val b-val]}]
  (let [x (if (< a-val b-val) 1 0)]
    (-> (write-val state c x)
        (inc-counter 4))))

(defmethod do-op 8 [state {:keys [c a-val b-val]}]
  (let [x (if (= a-val b-val) 1 0)]
    (-> (write-val state c x)
        (inc-counter 4))))

(defmethod do-op 99 [_ _] nil)

;; ++++++++
;; + main +
;; ++++++++

(defn simulate-amplifiers [program phase-settings]
  (first (reduce (fn [output phase-setting]
                   (-> (run-program program
                                    (concat [phase-setting]
                                            output))))
                 [0]
                 phase-settings)))

(defn all-n-seqs [[range-lo range-hi] n]
  (letfn [(go [n]
            (if (<= n 0)
              [[]]
              (for [x (range range-lo range-hi)
                    xs (go (dec n))]
                (cons x xs))))]
    (go n)))

(defn string->program [s]
  (->> (clojure.string/split s #",")
       (map #(Integer/parseInt %1))
       vec))

(def program
  (-> (line-seq (clojure.java.io/reader "input/day_7.txt"))
      first
      string->program))

(defn find-phase-settings [range-vec]
  (->> (all-n-seqs range-vec 5)
       (filter #(= #{0 1 2 3 4} (set %1)))
       (map (fn[xs] [xs (simulate-amplifiers program xs)]))
       (sort-by #(- (second %1)))
       first))

(defn day-7-part-1 [] (find-phase-settings [0 5]))
(defn day-7-part-2 [] (find-phase-settings [5 10]))
