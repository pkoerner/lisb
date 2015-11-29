(ns lisb.data-conversion-test
  (:require [clojure.test :refer :all]
            [lisb.representation :refer :all]
            [lisb.data-conversion :refer :all]))


(deftest base-conversion
  (testing "conversion of non-nested sets"
    (is (= (convert #{1} :set :int)
           (bset-enum 1))))


  (testing "conversion of non-nested vectors"
    (is (= (convert [4 2] :tuple [:int :int])
           (btuple 4 2)))

    (is (= (convert [4 2] :set :int)
           (bset-enum 4 2)))
    
    (is (= (convert [4 2] :sequence :int)
           (bsequence 4 2))))

    
  (testing "conversion of non-nested maps"
    ;; this should work for any art of relation/function
    (is (= (convert {1 2} :fn [:int :int])
           (bset-enum (btuple 1 2))))
    
    (is (= (convert {:x 1} :record [{:x :int}])
           (brecord :x 1)))
  
    (is (= (convert {:x 1 :y 2} :record [{:x :int}])
           (brecord :x 1)))))



(deftest nested-conversion
  (testing "conversion of sets of something else"
    (is (= (convert #{[2 2]} :set [:tuple [:int :int]])
           (bset-enum (btuple 2 2)))))
  
  (testing "conversion of nested vectors"
    (is (= (convert [#{true} 1] :tuple [[:set :bool] :int])
           (btuple (bset-enum true) 1)))

    (is (= (convert [true #{1}] :tuple [:bool [:set :int]])
           (btuple true (bset-enum 1))))

    (is (= (convert [#{true} #{1}] :tuple [[:set :bool] [:set :int]])
           (btuple (bset-enum true) (bset-enum 1))))

    (is (= (convert [#{1} #{2} #{3}] :sequence [:set :int])
           (bsequence (bset-enum 1) (bset-enum 2) (bset-enum 3))))

    (is (= (convert [#{1}] :set [:set :int])
           (bset-enum (bset-enum 1)))))
  
  (testing "conversion of nested maps"
    (is (= (convert {#{1} #{2}} :fn [[:set :int] [:set :int]])
           (bset-enum (btuple (bset-enum 1) (bset-enum 2)))))
    
    (is (= (convert {:x #{1}} :record [{:x [:set :int]}])
           (brecord :x (bset-enum 1))))

    (is (= (convert {:x #{1} :y 2} :record [{:x [:set :int]}])
           (brecord :x (bset-enum 1))))))
