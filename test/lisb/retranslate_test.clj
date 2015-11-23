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
