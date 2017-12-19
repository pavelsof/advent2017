(def input-file "inputs/heard-you-like-registers")


(defn read-input
  "produce seq of {:var :op :num :cond-var :cond-op :cond-num} dicts"
  [file]
  (let [regex #"([a-z]+) ([a-z]+) ([-0-9]+) if ([a-z]+) ([><!=]+) ([-0-9]+)"]
    (map (fn [line] (zipmap [:var :op :num :cond-var :cond-op :cond-num]
                            (map clojure.edn/read-string
                                 (rest (re-find regex line)))))
         (clojure.string/split-lines (slurp file)))))


(defn condition-holds?
  [{:keys [cond-var cond-op cond-num]} registers]
  (let [func (if (= cond-op (symbol "!=")) not= (resolve cond-op))]
    (func (get registers cond-var 0) cond-num)))


(defn update-registers
  [line registers]
  (let [func (if (= (line :op) (symbol "inc")) + -)
        last-value (get registers (line :var) 0)]
    (assoc registers (line :var) (func last-value (line :num)))))


(let [all-lines (read-input input-file)]
  (loop [line (first all-lines) lines (rest all-lines)
         registers {} max-values []]
    (if (nil? line)
      (do (println (last max-values))
          (println (apply max max-values)))
      (let [new-registers (if (condition-holds? line registers)
                            (update-registers line registers)
                            registers)]
        (recur (first lines)
               (rest lines)
               new-registers
               (conj max-values
                     (if (empty? registers)
                       0
                       (apply max (vals new-registers)))))))))
