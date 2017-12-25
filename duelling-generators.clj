(def generators {:a {:factor 16807 :divisor 2147483647 :multiple 4}
                 :b {:factor 48271 :divisor 2147483647 :multiple 8}})

(def starting-values {:a 65 :b 8921})


(defn zfill
  [string width]
  (loop [string string]
    (if (>= (count string) width)
      string
      (recur (str "0" string)))))


(defn rightmost-bits
  [number bits]
  (let [string (zfill (Integer/toString number 2) bits)]
    (subs string (- (count string) bits))))


(defn is-match?
  [values]
  (apply = (map (fn [value] (rightmost-bits value 16))
                (vals values))))


(defn generate
  [generator old-value]
  (rem (* old-value (generator :factor)) (generator :divisor)))


(defn generate-pickily
  [generator old-value]
  (let [generate (partial generate generator)]
    (first (drop-while (fn [value] (not (zero? (rem value (generator :multiple)))))
                       (rest (iterate generate old-value))))))


(defn duel
  [generators generator-func num-pairs starting-values]
  (loop [i 0 matches 0 values starting-values]
    (if (= i num-pairs)
      matches
      (let [values (reduce (fn [new-values [generator-name generator]]
                             (assoc new-values
                                    generator-name
                                    (generator-func generator (values generator-name))))
                           values
                           generators)]
        (recur (inc i)
               (if (is-match? values) (inc matches) matches)
               values)))))


; part one
(println (duel generators generate 40000000 starting-values))

; part two
(println (duel generators generate-pickily 5000000 starting-values))
