(ns lisb.prob.retranslate-test
  (:require [clojure.test :refer :all]
            [lisb.translation.types :refer [->Tuple]]
            [lisb.prob.retranslate :refer [retranslate]])
  (:import (de.hhu.stups.prob.translator BAtom BBoolean BNumber BString BSet BTuple BRecord)
           (de.hhu.stups.prob.translator.interpretations BRelation BFunction BSequence)))


;;; value types

(deftest atom-retranslation
  (testing "retranslating an atom results in a string"
    (is (= "a"
           (retranslate (BAtom. "a")))))
  (testing "retranslating an atom yields a native type"
    (is (= (type "a")
           (type (retranslate (BAtom. "a")))))))


(deftest bool-retranslation
  (testing "retranslating a boolean results in the same value"
    (is (= true
           (retranslate (BBoolean. true))))
    (is (= false
           (retranslate (BBoolean. false)))))
  (testing "retranlatinga boolean yields a native type"
    (is (= (type true)
           (type (retranslate (BBoolean. true)))))
    (is (= (type false)
           (type (retranslate (BBoolean. false))))))
  (testing "the retranslating of a boolean true is truthy"
    (is (retranslate (BBoolean. true))))
  (testing "the retranslating of a boolean false is falsey"
    (is (not (retranslate (BBoolean. false))))))


(deftest number-retranslation
  (testing "retranslating an integer results in the same value"
    (is (= 5
           (retranslate (BNumber. "5")))))
  (testing "retranslating an integer yields a native type"
    (is (= (type 5)
           (type (retranslate (BNumber. "5")))))))


(deftest string-retranslation
  (testing "retranslating a string results in the same value"
    (is (= "str"
           (retranslate (BString. "str")))))
  (testing "retranslating a string yields a native type"
    (is (= (type "str")
           (type (retranslate (BString. "str")))))))


;;; collection types

(deftest tuple-retranslation
  (let [e1 (BNumber. "1")
        e2 (BNumber. "2")]
    (testing "retranslating a tuple retranslates its members and results in a vector"
      (is (= (->Tuple [1 2])
             (retranslate (BTuple. e1 e2)))))
    (testing "retranslating a tuple results in a native data type"
      (is (= (type (->Tuple [1 2]))
             (type (retranslate (BTuple. e1 e2))))))))


(deftest record-retranslation
  (testing "retranslating an empty record results in an empty map"
    (is (= {}
           (retranslate (BRecord. {})))))
  (testing "retranslating an empty record results in a native type"
    (is (= (type {})
           (type (retranslate (BRecord. {}))))))
  (testing "retranslating a record retranslates its members and results in a map"
    (is (= {"a" 1, "b" 2} (retranslate (BRecord. {(BString. "a") (BNumber. "1"), (BString. "b") (BNumber. "2")})))))
  (testing "retranslating a record results in a native data type"
    (is (= (type {"a" 1, "b" 2})
           (type (retranslate (BRecord. {(BString. "a") (BNumber. "1"), (BString. "b") (BNumber. "2")})))))))


(deftest set-retranslation
  (testing "retranslating an empty set results in an empty set"
    (is (= #{}
           (retranslate (BSet. #{})))))
  (testing "retranslating an empty set results in a native type"
    (is (= (type #{})
           (type (retranslate (BSet. #{}))))))
  (testing "retranslating a set retranslates its members and results in a set"
    (is (= #{5} (retranslate (BSet. #{(BNumber. "5")})))))
  (testing "retranslating a set results in a native data type"
    (is (= (type #{5})
           (type (retranslate (BSet. #{(BNumber. "5")})))))))


;;; interpreted collection types of set

(deftest function-retranslation
  (testing "retranslating an empty function results in an empty map"
    (is (= {}
           (retranslate (BFunction. #{})))))
  (testing "retranslating an empty function results in a native type"
    (is (= (type {})
           (type (retranslate (BFunction. #{}))))))
  (testing "retranslating a function retranslates its members and results in a map"
    (is (= {1 4, 2 12}
           (retranslate (BFunction. #{(BTuple. (BNumber. "1") (BNumber. "4")) (BTuple. (BNumber. "2") (BNumber. "12"))})))))
  (testing "retranslating a function results in a native data type"
    (is (= (type {1 4, 2 12})
           (type (retranslate (BFunction. #{(BTuple. (BNumber. "1") (BNumber. "4")) (BTuple. (BNumber. "2") (BNumber. "12"))})))))))


(deftest relation-retranslation
  (testing "retranslating an empty relation results in an empty map"
    (is (= {}
           (retranslate (BRelation. #{})))))
  (testing "retranslating an empty relation results in a native type"
    (is (= (type {})
           (type (retranslate (BRelation. #{}))))))
  (testing "retranslating a relation retranslates its members and results in a map with vectors as values"
    (is (= {1 [4 5], 2 [12]}
           (retranslate (BRelation. #{(BTuple. (BNumber. "1") (BNumber. "4")) (BTuple. (BNumber. "1") (BNumber. "5")) (BTuple. (BNumber. "2") (BNumber. "12"))})))))
  (testing "retranslating a relation results in a native data type"
    (is (= (type {1 [4 5], 2 [12]})
           (type (retranslate (BRelation. #{(BTuple. (BNumber. "1") (BNumber. "4")) (BTuple. (BNumber. "1") (BNumber. "5")) (BTuple. (BNumber. "2") (BNumber. "12"))})))))))


(deftest sequence-retranslation
  (testing "retranslating an empty sequence results in an empty vector"
    (is (= []
           (retranslate (BSequence. #{})))))
  (testing "retranslating an empty sequence results in a native type"
    (is (= (type [])
           (type (retranslate (BSequence. #{}))))))
  (testing "retranslating a set retranslates its members and results in a vector"
    (is (= [4 12]
           (retranslate (BSequence. #{(BTuple. (BNumber. "1") (BNumber. "4")) (BTuple. (BNumber. "2") (BNumber. "12"))})))))
  (testing "retranslating a sequence results in a native data type"
    (is (= (type [4 12])
           (type (retranslate (BSequence. #{(BTuple. (BNumber. "1") (BNumber. "4")) (BTuple. (BNumber. "2") (BNumber. "12"))})))))))
