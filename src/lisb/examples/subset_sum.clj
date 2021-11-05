(ns lisb.examples.subset-sum
  (:require [lisb.core :refer [eval-ir-formula]]
            [lisb.translation.lisb2ir :refer [defpred]]))

(defpred subset-sum-p [coins target sol]
  (and (member? sol (--> coins natural-set))
       (= target (sigma [:x] (member? :x coins) (* (fn-call sol :x) :x)))))

(defn subset-sum
  ([coins target]
   (eval-ir-formula (subset-sum-p coins target :stolen)))
  ([]
   (let [coins #{16 17 23 24 39 40}
         target 100]
     (subset-sum coins target))))
