(def input "192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12")


(defn parse-input
  [input]
  (map (fn [x] (clojure.edn/read-string (clojure.string/trim x)))
       (clojure.string/split input #",")))


(defn string->ascii
  [string]
  (into [] (map int string)))


(defn rotate
  "rotate a circular list so that i is the new 0"
  [knot i]
  (into (vec (drop i knot))
        (vec (take i knot))))


(defn rotate-back
  "rotate a circular list back so that 0 is at i"
  [knot i]
  (rotate knot (- (count knot) i)))


(defn twist
  [knot start-pos length]
  (let [wip-knot (rotate knot start-pos)]
    (rotate-back (into
                   (vec (reverse (take length wip-knot)))
                   (vec (drop length wip-knot))) start-pos)))


(defn knit
  "returns a {:knot :current-pos :skip-size} dict"
  [initial-knot lengths start-pos initial-skip-size]
  (loop [knot initial-knot
         current-pos start-pos skip-size initial-skip-size
         length (first lengths) rem-lengths (rest lengths)]
    (if (empty? rem-lengths)
      {:knot (twist knot current-pos length)
       :current-pos (mod (+ current-pos length skip-size) (count knot))
       :skip-size (inc skip-size)}
      (recur (twist knot current-pos length)
             (mod (+ current-pos length skip-size) (count knot))
             (inc skip-size)
             (first rem-lengths) (rest rem-lengths)))))


(defn multi-knit
  [initial-knot lengths]
  (:knot (reduce (fn [{:keys [knot current-pos skip-size]} round]
                   (knit knot lengths current-pos skip-size))
                 {:knot initial-knot :current-pos 0 :skip-size 0}
                 (range 64))))


(defn condense
  [knot]
  (loop [block (take 16 knot) remaining (drop 16 knot) output []]
    (if (empty? remaining)
      (conj output (apply bit-xor block))
      (recur (take 16 remaining) (drop 16 remaining)
             (conj output (apply bit-xor block))))))


(defn hexadecimate
  [numbers]
  (apply str (map (fn [x] (let [s (Integer/toString x 16)]
                            (if (= 1 (count s)) (str "0" s) s)))
                  numbers)))


(defn knot-hash
  [input]
  (hexadecimate (condense (multi-knit (range 256)
                                      (into (string->ascii input) [17 31 73 47 23])))))


; part one
(println (apply * (take 2 (:knot (knit (range 256)
                                       (parse-input input)
                                       0 0)))))

; part two
(println (knot-hash input))
