(def input-file "inputs/maze-of-twisty-trampolines")


(defn read-input
  [file]
  (mapv clojure.edn/read-string (clojure.string/split (slurp file) #"\n")))


(defn jump
  [{:keys [maze pos]}]
  (let [value (maze pos)]
    {:maze (assoc maze pos (inc value))
     :pos (+ pos value)}))


(defn count-to-exit
  [maze jump-func]
  (count (take-while (fn [{:keys [maze pos]}] (contains? maze pos))
                     (iterate jump-func {:maze maze :pos 0}))))


(defn fancy-jump
  [{:keys [maze pos]}]
  (let [value (maze pos)]
    {:maze (assoc maze pos (if (< value 3) (inc value) (dec value)))
     :pos (+ pos value)}))


; part one
(println (count-to-exit (read-input input-file) jump))

; part two
(println (count-to-exit (read-input input-file) fancy-jump))
