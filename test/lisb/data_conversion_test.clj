(ns lisb.data-conversion-test
  (:require [clojure.test :refer :all]
            [lisb.representation :refer :all]
            [lisb.data-conversion :refer :all]))


(deftest base-conversion
  (testing "conversion of non-nested sets"
    (is (= (convert #{1 2 3} :set [])
           (apply bset-enum #{1 2 3})))) ; ordering may differ otherwise


  (testing "conversion of non-nested vectors"
    (is (= (convert [4 2] :tuple [])
           (btuple 4 2)))

    (is (= (convert [4 2] :set [])
           (bset-enum 4 2)))
    
    (is (= (convert [4 2] :sequence [])
           (bsequence 4 2))))

    
  (testing "conversion of non-nested maps"
    ;; this should work for any art of relation/function
    (is (= (convert {1 2} :fn [])
           (bset-enum (btuple 1 2))))
    
    (is (= (convert {:x 1} :record [])
           (brecord :x 1)))))
