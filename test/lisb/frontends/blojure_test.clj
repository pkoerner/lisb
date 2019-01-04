(ns lisb.frontends.blojure-test
  (:require [clojure.test :refer :all]
            [lisb.frontends.blojure :refer :all]))

(deftest node-test
  (testing "node works as intended"
    (is (= {:tag :foo
            :children [:bar :baz 1 #{} {:k :v}]}
           (node :foo :bar :baz 1 #{} {:k :v})))))

(deftest minus-test
  (testing "minus is special and must have a unary version"
    (is (= {:tag :unaryminus
            :children [:a]}
           (b- :a))))
  (testing "minus also works with more arguments"
    (is (= {:tag :minus
            :children [:a :b]}
           (b- :a :b)))
    (is (= {:tag :minus
            :children [:a :b :c]}
           (b- :a :b :c)))))



(deftest max-test
  (testing "max with a set"
    (is (= {:tag :max :children [#{1 2 3}]}
           (bmax #{1 2 3}))))
  (testing "max with more arguments"
    (is (= {:tag :max :children [#{1 2}]}
           (bmax 1 2)))
    (is (= {:tag :max :children [#{1 2 3}]}
           (bmax 1 2 3)))))

(deftest min-test
  (testing "min with a set"
    (is (= {:tag :min :children [#{1 2 3}]}
           (bmin #{1 2 3}))))
  (testing "min with more arguments"
    (is (= {:tag :min :children [#{1 2}]}
           (bmin 1 2)))
    (is (= {:tag :min :children [#{1 2 3}]}
           (bmin 1 2 3)))))


(deftest forall-test
  (testing "universal quantification representation"
    (is (= {:tag :forall
            :children [{:tag :list
                        :children [:x]}
                       {:tag :implication
                        :children [:a :b]}]}
           (bforall [:x] (b=> :a :b))
           (bforall [:x] :a :b)))))

(deftest let-tests
  (testing "one binding and predicate"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo]}
                       {:tag :equals :children [:foo 1]}
                       {:tag :less :children [:foo 2]}]}
           (blet-pred [:foo 1] (b< :foo 2)))))
  (testing "more bindings and a predicate"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo :bar]}
                       {:tag :and :children [{:tag :equals :children [:foo 1]}
                                             {:tag :equals :children [:bar 2]}]}
                       {:tag :less :children [:foo :bar]}]}
           (blet-pred [:foo 1 :bar 2] (b< :foo :bar)))))
  (testing "one binding and expression"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo]}
                       {:tag :equals :children [:foo 1]}
                       {:tag :plus :children [:foo 2]}]}
           (blet-pred [:foo 1] (b+ :foo 2)))))
  (testing "more bindings and an expression"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo :bar]}
                       {:tag :and :children [{:tag :equals :children [:foo 1]}
                                             {:tag :equals :children [:bar 2]}]}
                       {:tag :plus :children [:foo :bar]}]}
           (blet-pred [:foo 1 :bar 2] (b+ :foo :bar))))))
