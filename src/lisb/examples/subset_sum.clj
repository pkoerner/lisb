(ns lisb.examples.subset-sum
  (:require [lisb.core :refer [eval-ir-as-predicate]]
            [lisb.translation.lisb2ir :refer :all]
            [lisb.translation.ir2ast :refer [ir->predicate-ast]]))

(defpred subset-sum-p [coins target sol]
  (and (member? sol (--> coins bnatural-set))
       (= target (sigma [:x] (member? :x coins) (* (apply sol :x) :x)))))

(defn subset-sum
  ([coins target]
   (eval-ir-as-predicate (subset-sum-p coins target :stolen)))
  ([]
   (let [coins #{16 17 23 24 39 40}
         target 100]
     (subset-sum coins target))))
