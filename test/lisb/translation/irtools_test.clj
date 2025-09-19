(ns lisb.translation.irtools-test
  (:require [lisb.translation.irtools :refer [find-identifiers]]
            [lisb.translation.lisb2ir :refer :all]
            [clojure.test :refer :all]))



(deftest find-identifiers-test
  (testing "extracting open identifiers from an IR snippet works"
    (are [ir ids] (= (find-identifiers ir) ids)
         :x #{:x}
         1 #{}
         #{1 2 :x} #{:x}
         (b+ :x :y) #{:x :y}
         (b+ :x (b+ :y :z)) #{:x :y :z}))
  (testing "local identifiers are not extracted"
    (are [ir ids] (= (find-identifiers ir) ids)
         (bfor-all [:x] (b= :x :y)) #{:y}
         (bexists [:x] (b= :x :y)) #{:y}
         (bcomprehension-set [:x] (bmember? :x (binterval 1 :y))) #{:y}
         (bunion-pe [:z] (bcontains? bnat-set :z) :y) #{:y}
         (bintersection-pe [:z] (bcontains? bnat-set :z) :y) #{:y}
         (bpi [:z] (bcontains? bnat-set :z) :y) #{:y}
         (bsigma [:z] (bcontains? bnat-set :z) :y) #{:y}
         (bvar [:x] :y) #{:y}  ;; invalid substituion but who cares
         (bany [:x] (b< :x :y) bskip) #{:y}))
  (testing "shadowed variables which are also used are extracted"
    (are [ir ids] (= (find-identifiers ir) ids)
         (band (b= :x 42) (bfor-all [:x] (b= :x :y))) #{:x :y}))
  (testing "nested local variables work as intended"
    (are [ir ids] (= (find-identifiers ir) ids)
         (band (b= :x 42) 
               (bfor-all [:x :y] 
                         (bexists [:z] (b< :x :y :z)))) #{:x})
    (are [ir ids] (= (find-identifiers ir) ids)
         (band (b= :x 42) 
               (bfor-all [:x :y] 
                         (bexists [:x] (b< :x :y :z)))) #{:x :z})))
