(def input [0 2 7 0])


(defn get-largest-block
  [memory]
  (first (filter (fn [{:keys [index value]}] (= value (apply max memory)))
                 (map-indexed (fn [index value] {:index index :value value})
                              memory))))


(defn get-next-index
  [vector index]
  (if (contains? vector (inc index)) (inc index) 0))


(defn reallocate
  [memory]
  (let [largest-block (get-largest-block memory)]
    (loop [blocks (largest-block :value)
           index (get-next-index memory (largest-block :index))
           memory (assoc memory (largest-block :index) 0)]
      (if (zero? blocks)
        memory
        (recur (dec blocks)
               (get-next-index memory index)
               (assoc memory index (inc (memory index))))))))


(defn loop-until-deja-vu
  [initial-memory]
  (loop [seen #{} curr-memory initial-memory]
    (if (contains? seen curr-memory)
      {:loops (count seen) :memory curr-memory}
      (recur (conj seen curr-memory)
             (reallocate curr-memory)))))


(defn find-loop-size
  [memory]
  (inc (count (take-while (fn [curr-memory] (not (= curr-memory memory)))
                          (rest (iterate reallocate memory))))))


; part one
(println (:loops (loop-until-deja-vu input)))

; part two
(println (find-loop-size (:memory (loop-until-deja-vu input))))
