(ns lisb.retranslate-test
  (:require [clojure.test :refer :all]
            [lisb.retranslate :refer :all]))


(deftest int-retranslation
  (testing "retranslates an integer results in the same value"
    (is (= (retranslate (de.prob.translator.types.BigInteger/build "5"))
           5)))
  (testing "retranslating an integer yields a native type"
    (is (= (type (retranslate (de.prob.translator.types.BigInteger/build "5")))
           (type 5)))))

(deftest bool-retranslation
  (testing "retranslating a boolean results in the same value"
    (is (= (retranslate (de.prob.translator.types.Boolean. true))
           true))
    (is (= (retranslate (de.prob.translator.types.Boolean. false))
           false)))
  (testing "retranlatinga boolean yields a native type"
    (is (= (type (retranslate (de.prob.translator.types.Boolean. true)))
           (type true))))
  (testing "the retranslating of a boolean true is truthy"
    (is (retranslate (de.prob.translator.types.Boolean. true))))
  (testing "the retranslating of a boolean false is falsey"
    (is (not (retranslate (de.prob.translator.types.Boolean. false))))))


(deftest set-retranslation
  (testing "retranslating an empty set results in an empty set"
    (is (= (retranslate (de.prob.translator.types.Set. #{}))
           #{})))
  (testing "retranslating an empty set results in a native type"
    (is (= (type (retranslate (de.prob.translator.types.Set. #{})))
           (type #{}))))
  (testing "retranslating a set retranslates its members"
    (let [m (de.prob.translator.types.BigInteger/build "5")
          s (de.prob.translator.types.Set. #{m})
          r (retranslate s)]
      (is (= (count r) 1))
      (is (= (type (first s)) de.prob.translator.types.BigInteger))
      (is (not= (type (first r)) de.prob.translator.types.BigInteger)))))


(deftest tuple-retranslation
  (let [e1 (de.prob.translator.types.BigInteger/build "1")
        e2 (de.prob.translator.types.BigInteger/build "2")]
    (testing "retranslating a tuple results in a vector"
      (is (= (retranslate (de.prob.translator.types.Tuple. e1 e2))
             [1 2])))
    (testing "retranslating a tuple results in a native data type"
      (is (= (type (retranslate (de.prob.translator.types.Tuple. e1 e2)))
             (type [1 2]))))
    (testing "retranslating a tuple retranslates its elements"
      (let [[a b] (retranslate (de.prob.translator.types.Tuple. e1 e2))]
        (is (not= (type a) (type e1)))
        (is (not= (type b) (type e2)))
        (is (= a e1))
        (is (= b e2))))))


(deftest sequence-retranslation
  (testing "retranslating an empty sequence results in an empty set"
    (is (= (retranslate (de.prob.translator.types.Sequence. []))
           #{})))
  (testing "retranslating an empty sequence results in a native type"
    (is (= (type (retranslate (de.prob.translator.types.Sequence. [])))
           (type #{}))))
  (testing "retranslating a sequence results in a set of tuples"
    (let [e1 (de.prob.translator.types.BigInteger/build "4")
          e2 (de.prob.translator.types.BigInteger/build "12")]
      (is (= (retranslate (de.prob.translator.types.Sequence. [e1 e2]))
             #{[1 4] [2 12]}))))
  (testing "retranslating a sequence results translated tuples"
    (let [e1 (de.prob.translator.types.BigInteger/build "4")
          e2 (de.prob.translator.types.BigInteger/build "12")]
      (is (= (not-any? (partial instance? de.prob.translator.types.Tuple)
                       (retranslate (de.prob.translator.types.Sequence. [e1 e2]))))))))
