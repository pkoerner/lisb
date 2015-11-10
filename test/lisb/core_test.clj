(ns lisb.core-test
  (:require [clojure.test :refer :all]
            [lisb.core :refer :all]))

(deftest conjunct-test
  (testing "conjunction of two values works"
    (is (= {:tag :and
            :children [:a :b]}
           (conjunct :a :b)))))


(deftest chaining-test
  (testing "chaining generates conjunctions"
    (is (= {:tag :and
            :children [[:a :b] [:b :c]]}
           (chain vector [:a :b :c])))))

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

