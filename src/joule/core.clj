(ns joule.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

; TODO refactor to use multimethods below
(defn find-best-split [feature_data]
  "For a given feature, finds the best possible split.
  
  Deals correctly with both numerical and categorical data - the type
  is specified in the metadata of the list."

  ; first get the metadata in a let expression
  ; then use case to check
  (let [feature_type (get (meta feature_data) :type :numerical)]
    (if (nil? metadata)
      (find-best-split-numerical data_list)
      (let [metadata_type (metadata :type)])
      (cond 
        (= feature_type :categorical) (find-best-split-categorical data_list)
        (= feature_type :numerical) (find-best-split-numerical data_list)
        :else
          (throw UnsupportedOperationException. 
                 (format "feature type not supported %s" feature_type))))
  
  )
)

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
(let [category-groups (seq (group-by first (map vector feature-data label-data)))]
     []
  ([category-names (map first category-groups)]
       [category-data (map #(map first  (second %)) category-groups)])
    (min ()))

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

