(def input 361527)


(defn calc-distance
  [number]
  (let [p (int (Math/floor (Math/sqrt (inc number))))]
    (let [p (if (zero? (mod p 2)) (dec p) p)
          q (/ (inc p) 2)]
      (+ q
         (mod (Math/abs (- number (+ (* p p) q))) (inc p))))))


(defn get-next-head
  "determine the next coords of a spiral's head"
  [spiral]
  (let [{:keys [x y]} (spiral :head)]
    (if (contains? spiral {:x x :y (dec y)})  ; if down
      (if (contains? spiral {:x (dec x) :y y})  ; if left
        {:x x :y (inc y)}
        {:x (dec x) :y y})
      (if (contains? spiral {:x (inc x) :y y})  ; if right
        {:x x :y (dec y)}
        (if (contains? spiral {:x x :y (inc y)})  ; if up
          {:x (inc x) :y y}
          {:x x :y (inc y)})))))


(defn get-square
  "return the set of 9 positions forming a square centred around the given one"
  [{:keys [x y]}]
  (reduce (fn [res x] (into res (reduce (fn [res y] (conj res {:x x :y y}))
                                        #{}
                                        [(dec y) y (inc y)])))
          #{}
          [(dec x) x (inc x)]))


(defn calc-pos-value
  "sum the values of the position's neighbours"
  [spiral pos]
  (reduce (fn [res pos] (+ res (get spiral pos 0)))
          0
          (get-square pos)))


(defn grow-spiral
  "grow a spiral by one cell"
  [spiral]
  (let [spiral (assoc spiral :head (get-next-head spiral))]
    (assoc spiral (spiral :head) (calc-pos-value spiral (spiral :head)))))


(defn grow-new-spiral-until-reaching
  "grow a new spiral until its head reaches a value"
  [value]
  (first (drop-while (fn [spiral] (<= (spiral (spiral :head)) value))
                     (iterate grow-spiral {{:x 0 :y 0} 1
                                           {:x 1 :y 0} 1
                                           :head {:x 1 :y 0}}))))


; part one
(println (calc-distance input))

; part two
(let [spiral (grow-new-spiral-until-reaching input)]
  (println (spiral (spiral :head))))
