(def input-file "inputs/recursive-circus")


(defn read-input
  [file]

  (defn parse-name-and-weight
    [string]
    (let [matches (rest (re-find #"([a-z]+) \((\d+)\)" string))]
      {:name (first matches)
       :weight (clojure.edn/read-string (second matches))}))

  (defn parse-holding
    [string]
    {:holding (if (nil? string) #{}
                (into #{} (clojure.string/split string #", ")))})

  (defn parse-line
    [line]
    (let [parts (clojure.string/split line #" -> ")]
      (into (parse-name-and-weight (first parts))
            (parse-holding (second parts)))))

  (reduce (fn [{:keys [forest weights]} {:keys [name weight holding]}]
            {:forest (assoc forest name holding)
             :weights (assoc weights name weight)})
          {:forest {} :weights {}}
          (map parse-line (clojure.string/split-lines (slurp file)))))


(defn find-ungrafted
  "find path to an ungrafted node or return nil"
  [forest path]
  (let [node (get-in forest path)]
    (if-not (empty? node)
      (if (set? node)
        path
        (first (drop-while nil?
                           (map (fn [[k v]] (find-ungrafted forest
                                                            (conj path k)))
                                node)))))))


(defn graft
  "graft first-tier trees onto the subtree defined by path"
  [forest path]
  (assoc-in (apply dissoc forest (get-in forest path))
            path
            (into {} (map (fn [name] [name (forest name)])
                          (get-in forest path)))))


(defn build-tree
  "recursively turn a forest into a single tree"
  [initial-forest]
  (loop [forest initial-forest]
    (if (= (count forest) 1)
      forest
      (recur (graft forest (find-ungrafted forest []))))))


(defn find-paths
  "return seq of all paths (vectors of names), deepest first"
  ([root]
   (let [root-name (first (keys root))]
     (find-paths root-name (root root-name))))
  ([node-name children]
   (if (map? children)
     (map (fn [path] (into [node-name] path))
          (reduce (fn [res [k v]] (into res (find-paths k v))) [[]] children))
     (list [node-name]))))


; solution
(let [{:keys [forest weights]} (read-input input-file)]
  (let [tree (build-tree forest)]
    (println (first (keys tree)))
    (find-paths tree)))
