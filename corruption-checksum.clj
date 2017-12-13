(require '[clojure.string :as str])


(def input "
  5 9 2 8
  9 4 7 3
  3 8 6 5
  ")


(defn split-and-trim
  [string regex]
  (filter (complement empty?)
          (map str/trim (str/split string regex))))


(defn parse-input
  [input]
  (map (fn [x] (map #(Integer/parseInt %)
                    (split-and-trim x #"\s")))
       (split-and-trim input #"\n")))


(defn calc-checksum
  [matrix]
  (reduce + 0
          (map (fn [line] (- (apply max line) (apply min line)))
               matrix)))


(defn combinations
  "itertools.combinations(numbers, 2)"
  [numbers]
  (loop [i (first numbers) the-rest (rest numbers) combinations []]
    (if (empty? the-rest)
      combinations
      (recur (first the-rest) (rest the-rest)
             (into combinations (map (fn [x] [i x]) the-rest))))))


(defn find-quotient
  [numbers]
  (apply / (first (drop-while
                    (fn [[x y]] (not (= (mod x y) 0)))
                    (combinations (reverse (sort numbers)))))))


; part one
(println (calc-checksum (parse-input input)))

; part two
(println (reduce (fn [res line] (+ res (find-quotient line))) 0 (parse-input input)))
