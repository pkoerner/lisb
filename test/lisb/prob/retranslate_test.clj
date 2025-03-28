(ns lisb.prob.retranslate-test
  (:require [clojure.test :refer :all]
            [lisb.translation.types :refer [->Tuple]]
            [lisb.prob.retranslate :refer [retranslate]])
  (:import (de.hhu.stups.prob.translator BValue)))


;;; value types

(deftest atom-retranslation
  (testing "retranslating an atom results in a string"
    (is (= "a"
           (retranslate (BValue/atom "a")))))
  (testing "retranslating an atom yields a native type"
    (is (= (type "a")
           (type (retranslate (BValue/atom "a")))))))


(deftest bool-retranslation
  (testing "retranslating a boolean results in the same value"
    (is (= true
           (retranslate (BValue/bool true))))
    (is (= false
           (retranslate (BValue/bool false)))))
  (testing "retranslating a boolean yields a native type"
    (is (= (type true)
           (type (retranslate (BValue/bool true)))))
    (is (= (type false)
           (type (retranslate (BValue/bool false))))))
  (testing "the retranslating of a boolean true is truthy"
    (is (retranslate (BValue/bool true))))
  (testing "the retranslating of a boolean false is falsey"
    (is (not (retranslate (BValue/bool false))))))


(deftest number-retranslation
  (testing "retranslating an integer results in the same value"
    (is (= 5
           (retranslate (BValue/number "5")))))
  (testing "retranslating an integer yields a native type"
    (is (= (type 5)
           (type (retranslate (BValue/number "5")))))))


(deftest string-retranslation
  (testing "retranslating a string results in the same value"
    (is (= "str"
           (retranslate (BValue/string "str")))))
  (testing "retranslating a string yields a native type"
    (is (= (type "str")
           (type (retranslate (BValue/string "str")))))))


;;; collection types

(deftest tuple-retranslation
  (let [e1 (BValue/number "1")
        e2 (BValue/number "2")]
    (testing "retranslating a tuple retranslates its members and results in a vector"
      (is (= (->Tuple [1 2])
             (retranslate (BValue/tuple e1 e2)))))
    (testing "retranslating a tuple results in a native data type"
      (is (= (type (->Tuple [1 2]))
             (type (retranslate (BValue/tuple e1 e2))))))))


(deftest record-retranslation
  (testing "retranslating an empty record results in an empty map"
    (is (= {}
           (retranslate (BValue/record {})))))
  (testing "retranslating an empty record results in a native type"
    (is (= (type {})
           (type (retranslate (BValue/record {}))))))
  (testing "retranslating a record retranslates its members and results in a map"
    (is (= {:a 1, :b 2} (retranslate (BValue/record {"a" (BValue/number "1"), "b" (BValue/number "2")})))))
  (testing "retranslating a record results in a native data type"
    (is (= (type {:a 1, :b 2})
           (type (retranslate (BValue/record {"a" (BValue/number "1"), "b" (BValue/number "2")})))))))


(deftest set-retranslation
  (testing "retranslating an empty set results in an empty set"
    (is (= #{}
           (retranslate (BValue/set #{})))))
  (testing "retranslating an empty set results in a native type"
    (is (= (type #{})
           (type (retranslate (BValue/set #{}))))))
  (testing "retranslating a set retranslates its members and results in a set"
    (is (= #{5} (retranslate (BValue/set #{(BValue/number "5")})))))
  (testing "retranslating a set results in a native data type"
    (is (= (type #{5})
           (type (retranslate (BValue/set #{(BValue/number "5")})))))))


;;; interpreted collection types of set

(deftest function-retranslation
  (testing "retranslating an empty function results in an empty map"
    (is (= {}
           (retranslate (BValue/function {})))))
  (testing "retranslating an empty function results in a native type"
    (is (= (type {})
           (type (retranslate (BValue/function {}))))))
  (testing "retranslating a function retranslates its members and results in a map"
    (is (= {1 4, 2 12}
           (retranslate (BValue/function {(BValue/number "1") (BValue/number "4"), (BValue/number "2") (BValue/number "12")})))))
  (testing "retranslating a function results in a native data type"
    (is (= (type {1 4, 2 12})
           (type (retranslate (BValue/function {(BValue/number "1") (BValue/number "4"), (BValue/number "2") (BValue/number "12")})))))))


(deftest relation-retranslation
  (testing "retranslating an empty relation results in an empty map"
    (is (= {}
           (retranslate (BValue/relationFromTuples #{})))))
  (testing "retranslating an empty relation results in a native type"
    (is (= (type {})
           (type (retranslate (BValue/relationFromTuples #{}))))))
  (testing "retranslating a relation retranslates its members and results in a map with vectors as values"
    (is (= {1 [4 5], 2 [12]}
           (retranslate (BValue/relationFromTuples #{(BValue/tuple (BValue/number "1") (BValue/number "4")) (BValue/tuple (BValue/number "1") (BValue/number "5")) (BValue/tuple (BValue/number "2") (BValue/number "12"))})))))
  (testing "retranslating a relation results in a native data type"
    (is (= (type {1 [4 5], 2 [12]})
           (type (retranslate (BValue/relationFromTuples #{(BValue/tuple (BValue/number "1") (BValue/number "4")) (BValue/tuple (BValue/number "1") (BValue/number "5")) (BValue/tuple (BValue/number "2") (BValue/number "12"))})))))))


(deftest sequence-retranslation
  (testing "retranslating an empty sequence results in an empty vector"
    (is (= []
           (retranslate (BValue/sequence [])))))
  (testing "retranslating an empty sequence results in a native type"
    (is (= (type [])
           (type (retranslate (BValue/sequence []))))))
  (testing "retranslating a set retranslates its members and results in a vector"
    (is (= [4 12]
           (retranslate (BValue/sequenceFromTuples #{(BValue/tuple (BValue/number "1") (BValue/number "4")) (BValue/tuple (BValue/number "2") (BValue/number "12"))})))))
  (testing "retranslating a sequence results in a native data type"
    (is (= (type [4 12])
           (type (retranslate (BValue/sequenceFromTuples #{(BValue/tuple (BValue/number "1") (BValue/number "4")) (BValue/tuple (BValue/number "2") (BValue/number "12"))})))))))

(deftest small-big-number-retranslation
  (testing "small and big numbers are retranslated properly"
    (is (= 1 
          (retranslate (BValue/number "1"))))
    (is (= 1 
          (retranslate (BValue/number 1))))
    (is (= 123745624398705623487
          (retranslate (BValue/number "123745624398705623487"))))))
