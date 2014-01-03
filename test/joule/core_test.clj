(ns joule.core-test
  (:require [clojure.test :refer :all]
            [joule.core :refer :all]))

; TODO refactor to use a built in method rather than
; this custom approx-equal
(defn approx-equal 
  ([a b] (approx-equal a b 0.00001))
  ([a b tolerance]
   "approximate equality for float types"
   (< (Math/abs (- a b)) tolerance))
  ) 

(deftest test-gini
  (let [leaf-vals (list 1 5 1 1)]
    (is (approx-equal (gini leaf-vals) (double (/ 3 8))))))

(deftest test-entropy
  (let [leaf-vals (list 1 5 1 1)]
    (is (approx-equal (entropy leaf-vals) (+ (* -0.25 (Math/log 0.25)) 
                                             (* -0.75 (Math/log 0.75)))))) 
  )
