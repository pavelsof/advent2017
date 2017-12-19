(def input-file "inputs/stream-processing")


(defn remove-ignored
  [stream]
  (clojure.string/replace stream #"!." ""))


(defn remove-garbage
  [stream]
  (clojure.string/replace stream #"<.*?>" ""))


(defn size-of-garbage
  [stream]
  (apply + (map (fn [match] (- (count match) 2))
                (re-seq #"<.*?>" stream))))


(defn calc-score
  [whole-stream]
  (loop [ch (first whole-stream) stream (rest whole-stream) level 0 score 0]
    (if (nil? ch)
      score
      (recur (first stream)
             (rest stream)
             (cond (= ch \{) (inc level) (= ch \}) (dec level) :else level)
             (if (= ch \}) (+ score level) score)))))


(loop [lines (clojure.string/split-lines (slurp input-file))]
  (when-not (empty? lines)
    (let [stream (remove-ignored (first lines))]
      (println (calc-score (remove-garbage stream))
               (size-of-garbage stream))
      (recur (rest lines)))))
