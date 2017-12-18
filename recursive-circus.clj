(def input-file "inputs/recursive-circus")


(defn read-input
  "return 2 dicts mapping names, one to sets of other names, another to weights"
  [file]
  (reduce (fn [{:keys [forest weights]} line]
            (let [matches (rest (re-find #"([a-z]+) \((\d+)\)( -> ([a-z, ]+))?" line))]
              {:forest (assoc forest
                              (first matches)
                              (if (nil? (last matches))
                                #{}
                                (into #{}
                                      (clojure.string/split (last matches)
                                                            #", "))))
               :weights (assoc weights
                               (first matches)
                               (clojure.edn/read-string (second matches)))}))
          {:forest {} :weights {}}
          (clojure.string/split-lines (slurp file))))


(defn find-ungrafted
  "find path to an ungrafted node or return nil"
  [forest path]
  (let [node (get-in forest path)]
    (if-not (empty? node)
      (if (set? node)
        path
        (first (drop-while nil?
                           (map (fn [k] (find-ungrafted forest (conj path k)))
                                (keys node))))))))


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
     (concat (map (fn [path] (into [node-name] path))
                  (reduce (fn [res [k v]] (into res (find-paths k v)))
                          [] children))
             (list [node-name]))
     (list [node-name]))))


(defn non-conforming-key
  "given a dict that has all values equal but for one, get the key to that one"
  [dict]
  (let [sorted (sort-by val dict)]
    (if (= (first (vals sorted)) (second (vals sorted)))
      (last (keys sorted))
      (first (keys sorted)))))


(defn find-should-be-weight
  "find the solution to part 2"
  [tree weights]
  (loop [total-weights {} paths (find-paths tree)]
    (let [path (first paths) node-name (last path) children (get-in tree path)]
      (if (and (not (empty? children))
               (not (apply = (map (fn [k] (total-weights k)) (keys children)))))
        (let [problem-node (non-conforming-key (into {}
                                                     (map (fn [x] [x (total-weights x)])
                                                          (keys children))))]
          (+ (weights problem-node)
             (- (total-weights (first (filter (fn [k] (not (= k problem-node)))
                                              (keys children))))
                (total-weights problem-node))))
        (recur (assoc total-weights
                      node-name
                      (reduce (fn [weight child] (+ weight (total-weights child)))
                              (weights node-name) (keys children)))
               (rest paths))))))


(let [{:keys [forest weights]} (read-input input-file)]
  (let [tree (build-tree forest)]
    (println (first (keys tree)))
    (println (find-should-be-weight tree weights))))
