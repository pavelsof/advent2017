(def input "
  0 <-> 2
  1 <-> 1
  2 <-> 0, 3, 4
  3 <-> 2, 4
  4 <-> 2, 3, 6
  5 <-> 6
  6 <-> 4, 5
  ")


(defn split-and-trim
  [string regex]
  (map clojure.string/trim (clojure.string/split string regex)))


(defn parse-input
  [string]
  (reduce (fn [d line]
            (let [[left right] (split-and-trim line #" <-> ")]
              (into d (hash-map left (vec (split-and-trim right #","))))))
          {}
          (filter (complement empty?) (split-and-trim string #"\n"))))


(defn find-group
  "recursively find the set of houses reachable from the one specified"
  [village house already-visited]
  (reduce (fn [visited pipe] (if (contains? visited pipe)
                               visited
                               (find-group village pipe visited)))
          (into already-visited #{house}) (get village house)))


(defn remove-keys
  [dict keys-to-remove]
  (reduce (fn [new-dict [key value]] (if (contains? keys-to-remove key)
                                       new-dict
                                       (into new-dict {key value})))
          {} dict))


(defn count-groups
  "count how many groups are there"
  [village]
  (loop [total 0 sub-village village]
    (if (= (count sub-village) 0)
      total
      (let [group (find-group sub-village (first (first sub-village)) #{})]
        (recur (inc total) (remove-keys sub-village group))))))


; part one
(println (count (find-group (parse-input input) "0" #{})))

; part two
(println (count-groups (parse-input input)))
