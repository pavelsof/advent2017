(def input-file "inputs/high-entropy-passphrases")


(defn read-input
  [file]
  (map (fn [line] (clojure.string/split (clojure.string/trim line) #"\s"))
       (clojure.string/split (slurp file) #"\n")))


(defn has-duplicates?
  [words]
  (not (= (count (apply hash-set words))
          (count words))))


(defn count-duplicate-free
  [lines]
  (reduce + 0
          (map (fn [line] (if (has-duplicates? line) 0 1))
               lines)))


(defn has-anagrams?
  [words]
  (not (= (count (apply hash-set (map #(apply hash-set %) words)))
          (count words))))


(defn count-anagram-free
  [lines]
  (apply + (map (fn [line] (if (has-anagrams? line) 0 1))
                lines)))


; part one
(println (count-duplicate-free (read-input input-file)))

; part two
(println (count-anagram-free (read-input input-file)))
