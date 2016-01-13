(ns lisb.representation-test
  (:require [clojure.test :refer :all]
            [lisb.representation :refer :all]))

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
  


(deftest member-test
  (testing "member representation with two arguments"
    (is (= {:tag :member
            :children [1 #{1}]}
           (bmember 1 #{1}))))
  (testing "member representation with more than two arguments"
    (is (= {:tag :and :children [{:tag :member :children [1 #{1}]}
                                 {:tag :member :children [1 #{2}]}]}
           (bmember 1 #{1} #{2})))))


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

(deftest pred-test
  (testing "the pred macro allows to write b-macrofied expressions
            and returns a function"
    (is (fn? (pred [] (+ 1 2)))))
  
  (testing "the pred macro has a parameter list just like fn"
    (is (fn? (pred [x y] (+ 1 2)))))

  (testing "the resulting function generates a representation
            which replaces the parameter symbols with the values provided"
    (is (= ((pred [x y] (< x y)) 1 2)
           {:tag :less :children [1 2]})))
  
  (testing "the pred macro flattens sets properly"
    (is (= (count ((pred [] #{:x :y})))
            2)))) 


(deftest let-tests
  (testing "one binding and predicate"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo]}
                       {:tag :list :children [1]}
                       {:tag :less :children [:foo 2]}]}
           (blet-pred [:foo 1] (b< :foo 2)))))
  (testing "more bindings and a predicate"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo :bar]}
                       {:tag :list :children [1 2]}
                       {:tag :less :children [:foo :bar]}]}
           (blet-pred [:foo 1 :bar 2] (b< :foo :bar)))))
  (testing "one binding and expression"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo]}
                       {:tag :list :children [1]}
                       {:tag :plus :children [:foo 2]}]}
           (blet-pred [:foo 1] (b+ :foo 2)))))
  (testing "more bindings and an expression"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo :bar]}
                       {:tag :list :children [1 2]}
                       {:tag :plus :children [:foo :bar]}]}
           (blet-pred [:foo 1 :bar 2] (b+ :foo :bar))))))
