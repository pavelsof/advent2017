(def input "
  0: 3
  1: 2
  4: 4
  6: 4
  ")


(defn split-and-trim
  [string regex]
  (filter (complement empty?)
          (map clojure.string/trim (clojure.string/split string regex))))


(defn parse-input
  [string]
  (reduce (fn [dict line]
            (let [[key value] (split-and-trim line #":")]
              (into dict
                    {(clojure.edn/read-string key) (clojure.edn/read-string value)})))
          {}
          (split-and-trim string #"\n")))


(defn is-caught
  [firewall layer step]
  (and (contains? firewall layer) (zero? (mod step (* 2 (dec (firewall layer)))))))


(defn calc-layer-severity
  [firewall layer step]
  (if (is-caught firewall layer step)
    (* layer (firewall layer))
    0))


(defn calc-firewall-severity
  [firewall initial-step]
  (loop [step initial-step layer 0 severity 0]
    (if (> layer (apply max (keys firewall)))
      severity
      (recur (inc step) (inc layer)
             (+ severity (calc-layer-severity firewall layer step))))))


(defn can-pass-through
  [firewall initial-step]
  (loop [step initial-step layer 0]
    (if (> layer (apply max (keys firewall)))
      true
      (if (is-caught firewall layer step)
        false
        (recur (inc step) (inc layer))))))


(defn find-smallest-delay
  [firewall]
  (loop [i 0]
    (if (can-pass-through firewall i)
      i
      (recur (inc i)))))


; part one
(println (calc-firewall-severity (parse-input input) 0))

; part two
(println (find-smallest-delay (parse-input input)))
