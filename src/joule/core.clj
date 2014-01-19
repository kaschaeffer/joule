(ns joule.core)

; (declare find-best-split)
; (declare best-split-value)
(declare evaluate-split)

; How does the tree stop splitting?
;
; 1. Node is pure (available from data)
; 2. Sample size of node is too small (available from data)
; 3. Infogain from new feature is too small in absolute terms (should be availabel from find-best-split)
; 4. Tree hits maximum depth (TODO info is not easily available in current implementation)
;
; Scikit-learn has: max_depth,
; min_samples_split,
; min_samples_leaf - TODO understand this criterion.  According to scikit-learn documentation this is "minimum number of samples required to be at a leaf node" 
; min_density, 
; max_features - which features to consider when making split (will be important
;   for random forest implementation (can be # or function of total features i.e. 
;   "log", "sqrt", etc...

; structure of tree
; [[:feature-name split-value] left-node right-node]

; (defn build-tree [data]
;   (if-let [[best-split-name best-split-value] (find-best-split data)]
;     (let [left-node-data right-node-data] (make-split data best-split-name best-split-value)
;       [[best-split-name best-split-value] (build-tree left-node-data) (build-tree right-node-data)])
;     data))


; TODO refactor to use multimethods below

(defn get-feature-type [feature-data]
  (get (meta feature-data) :type :numerical))

(defmulti find-best-split
  (fn [feature-data label-data] (get-feature-type feature-data)))

(defmethod find-best-split :categorical
  [feature-data label-data]
  )



(defmethod find-best-split :numerical
  [feature-data label-data]

  ; first sort the data
  ; then step through all possible splits
  (let [sorted-features (sort feature-data)
        features-with-labels (map list feature-data label-data)]
    (apply min (map (fn [split-value] (evaluate-split split-value features-with-labels criterion)) features-with-labels)))
  )

(split-with #(> % 2) (reverse (sort (list 9 1 2 3 100))))

(split-with #(<= (first %) 2) (list [1 :z] [2 :z] [10 :b] [12 :c]))

(apply map list (list [1 :a] [2 :b] [3 :c] [4 :d]))

(defn evaluate-split [split-value data criterion]
  (->> data
       (split-with #(>= split-value (first %)))
       (map #(apply map list %))
       (map second)
       (map criterion)
       (reduce +)
       ))

(apply min (list 1 9 -100 8123))

; (defmethod find-best-split :default
;   [feature-data label-data]
;   (throw UnsupportedOperationException. 
;          (format "feature type not supported %s" (get-feature-type feature-data))))

; (defn find-best-split [feature_data]
;   "For a given feature, finds the best possible split.
  
;   Deals correctly with both numerical and categorical data - the type
;   is specified in the metadata of the list."

;   ; first get the metadata in a let expression
;   ; then use case to check
;   (let [feature_type (get (meta feature_data) :type :numerical)]
;     (if (nil? metadata)
;       (find-best-split-numerical data_list)
;       (let [metadata_type (metadata :type)]
;       (cond 
;         (= feature_type :categorical) (find-best-split-categorical data_list)
;         (= feature_type :numerical) (find-best-split-numerical data_list)
;         :else
;           (throw UnsupportedOperationException. 
;                  (format "feature type not supported %s" feature_type)))))
  
;   )
; )

; what format should feature_data take:
; Possibilities:
; 1) [feature_list, label_list]


(defn find-best-split-categorical [feature_data label_data]
  "Find the best possible split for a categorical variable.
  
  For computational reasons, at this point we only consider two types of splits:
  1. split into each category
  2. split into two groups (and consider all ways of splitting groups)")

; TODO use group-by command to split things by feature_dat
; (better approach than frequencies!! which doesn't really use the labels)
; see http://clojuredocs.org/clojure_core/1.2.0/clojure.core/group-by
;
;
; TODO should I just be passing frequencies instead??
;
; (let [category-groups (seq (group-by first (map vector feature-data label-data)))]
;      []
;   ([category-names (map first category-groups)]
;        [category-data (map #(map first  (second %)) category-groups)])
;     (min ()))

(defn find-best-feature [data]
  "Find the best possible split of the data.
  
  Should:
  
  -- always perform binary split on numerical data
  -- should intelligently deal with categorical data (this may mean performing a n-way split)
  
  -- TODO decide on a good way of representing categorical data
  "
  ; {:feature x
  ;  :list of ranges
  ;  :list of chunks of data})
)

 (defn build-tree [data options]
   "Recursively builds a decision tree using the original data.
   
   options is a map with keys:
    :min-sample-split   --> any node with this or fewer examples will stop splitting
    :criterion          --> criterion for finding best split (currently, either gini or entropy)
    
   TODO
   :max-depth
   :feature-importance (is this on a tree-level or a forest-level?)
   "
   
   (if (<= (count data) (options :min-sample-split))
     {:feature nil
      :splits nil
      :node_data data}
     (let [{:keys [feature splits node_data]} (find-best-split data)]
       {:feature feature
        :splits splits
        :node_data (map #(build-tree % options) node_data)}
       )
     )
   )

(defn gini [leaf-labels]
  "Takes a list <leaf-labels>.  Each node is a list of items
  along with their the class they belong to.
 
  Returns the Gini index as a double"
  
  (let [prob (map #(/ (double (val %)) 
                      (count leaf-labels)) 
                  (frequencies leaf-labels))]
    (reduce + (map #(* % (- 1 %)) prob))
    )
  )

(defn entropy [leaf-labels]
  "Takes a list <leaf-labels>.  Each node is a list of items
  along with their the class they belong to.
 
  Returns the entropy as a double"
  
  (let [prob (map #(/ (double (val %)) 
                      (count leaf-labels)) 
                  (frequencies leaf-labels))]
    (reduce + (map #(* -1 % (Math/log %)) prob))
    )
  )

