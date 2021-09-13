(ns lisb.examples.subset-sum
  (:require [lisb.core :refer [eval-formula empty-state-space]])
  (:require [lisb.translation.representation :refer :all])
  (:require [lisb.translation.translation :refer [b->predicate-ast]]))

(defpred subset-sum-p [coins target sol]
  (and (member? sol (--> coins bnatural-set))
       (= target (sigma [:x] (member? :x coins) (* (apply sol :x) :x)))))

(defn subset-sum
  ([coins target ss]
   (eval-formula ss (b->predicate-ast (subset-sum-p coins target :stolen))))
  ([]
   (defonce ss (empty-state-space))
   (let [coins #{16 17 23 24 39 40}
         target 100]
     (subset-sum coins target ss))))
