(clojure.main/load-script "knot-hash.clj")


(def input "flqrgnkx")


(defn hex-digit->bits
  "convert a hex digit (0-f) to a seq of bits"
  [hex]
  (loop [power 3 decimal (clojure.edn/read-string (str "0x" hex)) bits []]
    (if (< power 0)
      bits
      (let [base (Math/pow 2 power)]
        (recur (dec power)
               (rem decimal base)
               (conj bits (int (quot decimal base))))))))


(defn hex->bits
  "convert a hexadecimal string to a seq of bits"
  [hex]
  (reduce (fn [bits digit] (into bits (hex-digit->bits digit)))
          [] hex))


(defn make-grid
  "make the 128x128 grid as a [] of [] of 1s and 0s"
  [input]
  (reduce (fn [grid line] (conj grid (hex->bits (knot-hash line))))
          []
          (map #(str input "-" %) (range 128))))


(defn count-ones
  "count the number of 1s in a grid"
  [grid]
  (reduce (fn [res row] (+ res (count (filter #(= 1 %) row)))) 0 grid))


(defn merge-regions
  "re-assign all cells of region B to region A"
  [regions a b]
  (reduce (fn [res [cell region]] (assoc res cell
                                         (if (= region b) a region)))
          {} regions))


(defn assign-region
  "assign the region of a cell looking at its top and left neighbours"
  [regions row col]
  (let [top [(dec row) col] left [row (dec col)]]
    (if (and (contains? regions top) (contains? regions left))
      (let [regions (merge-regions regions (regions top) (regions left))]
        (assoc regions [row col] (regions top)))
      (assoc regions [row col]
             (cond (contains? regions top) (regions top)
                   (contains? regions left) (regions left)
                   :else (gensym))))))


(defn make-regions
  "compile mapping from [row col] coords to region identifiers"
  [grid]
  (reduce (fn [regions [row values]]
            (reduce (fn [regions [col value]]
                      (if (zero? value)
                        regions
                        (assign-region regions row col)))
                    regions (map-indexed vector values)))
          {} (map-indexed vector grid)))


(let [grid (make-grid input)]
  (println (count-ones grid))
  (println (count (into #{} (vals (make-regions grid))))))
