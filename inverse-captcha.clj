(def input "1122")


(defn get-counterpart
  [string index distance]
  (str (get string (mod (+ index distance) (count string)))))


(defn calc-captcha [string distance]
  (reduce + 0
          (map-indexed
            (fn [index item]
              (if (= (str item) (get-counterpart string index distance))
                (Integer/parseInt (str item))
                0))
            string)))


; part one
(println (calc-captcha input 1))

; part two
(println (calc-captcha input (/ (count input) 2)))
