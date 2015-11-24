(ns lisb.examples.subset-sum
  (:require [lisb.core :refer [eval state-space]])
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]]))

(defpred subset-sum-p [coins target sol]
  (and (member? sol (--> coins (bnatural-set)))
       (= target (sigma [:x] (member? :x coins) (* (apply sol :x) :x)))))

(defn subset-sum
  ([coins target ss]
   (eval ss (to-ast (subset-sum-p coins target :stolen))))
  ([]
   (defonce ss (state-space))
   (let [coins #{16 17 23 24 39 40}
         target 100]
     (subset-sum coins target ss))))
