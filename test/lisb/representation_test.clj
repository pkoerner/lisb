(ns lisb.representation-test
  (:require [clojure.test :refer :all]
            [lisb.representation :refer :all]))


(deftest less-test
  (testing "less works with two arguments"
    (is (= {:tag :less
            :children [:a :b]}
           (b< :a :b))))
  (testing "less works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :less :children [:a :b]}
                       {:tag :less :children [:b :c]}]}
           (b< :a :b :c)))))

(deftest plus-test
  (testing "plus works with two arguments"
    (is (= {:tag :plus
            :children [:a :b]}
           (b+ :a :b))))
  (testing "plus works with more than two arguments"
    (is (= {:tag :plus
            :children [{:tag :plus :children [:a :b]}
                       :c]}
           (b+ :a :b :c)))))


(deftest and-test
  (testing "and works with two arguments"
    (is (= {:tag :and
            :children [:a :b]}
           (band :a :b))))
  (testing "and works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :and :children [:a :b]}
                       :c]}
           (band :a :b :c)))))

(deftest minus-test
  (testing "minus is special and must have a unary version"
    (is (= {:tag :unaryminus
            :children [:a]}
           (b- :a))))
  (testing "minus also works with two arguments"
    (is (= {:tag :minus
            :children [:a :b]}
           (b- :a :b))))
  (testing "minus also works as expected with more than two arguments"
    (is (= {:tag :minus
            :children [{:tag :minus :children [:a :b]}
                       :c]}
           (b- :a :b :c)))))

(deftest equals-test
  (testing "equals works with two arguments"
    (is (= {:tag :equals
            :children [:a :b]}
           (b= :a :b))))
  (testing "equals works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :equals :children [:a :b]}
                       {:tag :equals :children [:b :c]}]}
           (b= :a :b :c)))))


(deftest equivalence-test
  (testing "equals works with two arguments"
    (is (= {:tag :equivalence
            :children [:a :b]}
           (b<=> :a :b))))
  (testing "equals works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :equivalence :children [:a :b]}
                       {:tag :equivalence :children [:b :c]}]}
           (b<=> :a :b :c)))))



(deftest or-test
  (testing "or works with two arguments"
    (is (= {:tag :or
            :children [:a :b]}
           (bor :a :b))))
  (testing "or works with more than two arguments"
    (is (= {:tag :or
            :children [{:tag :or :children [:a :b]}
                       :c]}
           (bor :a :b :c)))))

(deftest not-test
  (testing "not is unary"
    (is (= {:tag :not
            :children [:a]}
           (bnot :a)))))

(deftest not-equals-test
  (testing "not-equals works with two arguments"
    (is (= {:tag :not-equals
            :children [:a :b]}
           (bnot= :a :b))))
  (testing "not-equals works with more than two arguments"
    (is (= {:tag :and,
            :children [{:tag :and, :children [{:tag :not-equals, :children [:a :b]}
                                              {:tag :not-equals, :children [:a :c]}]}
                       {:tag :not-equals :children [:b :c]}]}
           (bnot= :a :b :c))))
  (testing "not-equals generates data in a set-like semantics"
    (is (= {:tag :and
            :children [{:tag :not-equals :children [:a :a]}
                       {:tag :not-equals :children [:a :b]}]}))))

