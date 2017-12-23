(def input-file "inputs/hex-ed")


(def compass {:n [0 1]
              :ne [1 0]
              :se [1 -1]
              :s [0 -1]
              :sw [-1 0]
              :nw [-1 1]})


(defn read-input
  [file]
  (clojure.string/split (clojure.string/trim (slurp file)) #","))


(defn make-step
  [pos dir]
  (into [] (map + pos (compass (keyword dir)))))


(defn dist-from-origin
  [pos]
  (let [ne (first pos) n (second pos)
        abs-ne (Math/abs (first pos)) abs-n (Math/abs (second pos))]
    (if (< (* ne n) 0)
      (max abs-ne abs-n)
      (+ abs-ne abs-n))))


(loop [curr-pos [0 0] path (read-input input-file)
       curr-dist 0 max-dist 0]
  (if (empty? path)
    (println curr-dist max-dist)
    (let [curr-pos (make-step curr-pos (first path))
          curr-dist (dist-from-origin curr-pos)]
      (recur curr-pos (rest path) curr-dist (max curr-dist max-dist)))))
