(ns lisb.representation-test
  (:require [clojure.test :refer :all]
            [lisb.representation :refer :all]))

(deftest equality-predicates-test
  (testing " equality-predicates"
    (are [node-repr lisb] (= node-repr (b lisb))
                          {:tag :equal
                           :left true
                           :right false}
                          (= true false))))

(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [node-repr lisb] (= node-repr (b lisb))
                          {:tag        :and
                           :predicates [{:tag :equal, :left 1, :right 1}
                                        {:tag :equal, :left 2, :right 2}
                                        {:tag :equal, :left 3, :right 3}]}
                          (and (= 1 1) (= 2 2) (= 3 3)))))


(deftest minus-test
  (testing "minus is special and must have a unary version"
    (is (= {:tag    :unary-minus
            :number :a}
           (b- :a))))
  (testing "minus also works with more arguments"
    (is (= {:tag :minus
            :numbers [:a :b]}
           (b- :a :b)))
    (is (= {:tag :minus
            :numbers [:a :b :c]}
           (b- :a :b :c)))))


(deftest max-test
  (testing "max with a set"
    (is (= {:tag :max :set #{1 2 3}}
           (bmax #{1 2 3}))))
  (testing "max with more arguments"
    (is (= {:tag :max :set #{1 2}}
           (bmax 1 2)))
    (is (= {:tag :max :set #{1 2 3}}
           (bmax 1 2 3)))))

(deftest min-test
  (testing "min with a set"
    (is (= {:tag :min :set #{1 2 3}}
           (bmin #{1 2 3}))))
  (testing "min with more arguments"
    (is (= {:tag :min :set #{1 2}}
           (bmin 1 2)))
    (is (= {:tag :min :set #{1 2 3}}
           (bmin 1 2 3)))))


(deftest for-all-test
  (testing "universal quantification representation"
    (is (= {:tag         :for-all
            :identifiers [:x]
            :implication {:tag        :implication
                          :predicates [:a :b]}}
           (bfor-all [:x] (b=> :a :b))
           (bfor-all [:x] :a :b)))))

(deftest pred-test
  (testing "the pred macro allows to write b-macrofied expressions
            and returns a function"
    (is (fn? (pred [] (+ 1 2)))))
  
  (testing "the pred macro has a parameter list just like fn"
    (is (fn? (pred [x y] (+ 1 2)))))

  (testing "the resulting function generates a representation
            which replaces the parameter symbols with the values provided"
    (is (= ((pred [x y] (< x y)) 1 2)
           {:tag :less :numbers [1 2]})))
  
  (testing "the pred macro flattens sets properly"
    (is (= (count ((pred [] #{:x :y})))
            2)))) 


(deftest let-tests
  (testing "one binding and predicate"
    (is (= {:tag :let-pred
            :identifiers [:foo]
            :assignment {:tag :equal, :left :foo, :right 1}
            :predicate {:tag :less, :numbers [:foo 2]}}
           (blet-pred [:foo 1] (b< :foo 2)))))
  (testing "more bindings and a predicate"
    (is (= {:tag         :let-pred
            :identifiers [:foo :bar]
            :assignment  {:tag        :and
                          :predicates [{:tag :equal, :left :foo, :right 1}
                                       {:tag :equal, :left :bar, :right 2}]}
            :predicate   {:tag :less, :numbers [:foo :bar]}}
           (blet-pred [:foo 1 :bar 2] (b< :foo :bar)))))
  (testing "one binding and expression"
    (is (= {:tag :let-pred
           :identifiers [:foo]
           :assignment {:tag :equal, :left :foo, :right 1}
           :predicate {:tag :plus, :numbers [:foo 2]}}
           (blet-pred [:foo 1] (b+ :foo 2)))))
  (testing "more bindings and an expression"
    (is (= {:tag         :let-pred
            :identifiers [:foo :bar]
            :assignment  {:tag        :and
                          :predicates [{:tag :equal, :left :foo, :right 1}
                                       {:tag :equal, :left :bar, :right 2}]}
            :predicate   {:tag :plus, :numbers [:foo :bar]}}
           (blet-pred [:foo 1 :bar 2] (b+ :foo :bar))))))
