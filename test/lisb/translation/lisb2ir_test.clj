(ns lisb.translation.lisb2ir-test
  (:require [clojure.test :refer :all]
            [lisb.translation.util :refer [lisb->ir b pred]]
    [lisb.examples.simple :as simple]
    [lisb.examples.marriages :as marriages]
    [lisb.examples.sebastian :as sebastian]
            ))


(deftest examples-sebastian-test
  (testing "examples-sebastian"
    (are [ir lisb-name] (= ir (lisb->ir (read-string (slurp (clojure.java.io/resource (str "machines/lisb/sebastian/" lisb-name ".edn"))))))
                        sebastian/generic-timer-mc "GenericTimersMC"
                        sebastian/traffic-light2 "TrafficLight2"
                        sebastian/traffic-light-time-ref "TrafficLightTime_Ref")))


(deftest examples-marriages-test
  (testing "examples-marriages"
    (are [ir lisb-name] (= ir (lisb->ir (read-string (slurp (clojure.java.io/resource (str "machines/lisb/marriages/" lisb-name ".edn"))))))
                        marriages/life "Life"
                        marriages/marriage "Marriage"
                        marriages/registrar "Registrar")))

(deftest simple-machines-test
  (testing "simple-machines"
    (are [ir lisb-name] (= ir (lisb->ir (read-string (slurp (clojure.java.io/resource (str "machines/lisb/simple/" lisb-name ".edn"))))))
                simple/lift "Lift"
                simple/a-counter "ACounter"
                simple/gcd "GCD"
                simple/knights-knaves "KnightsKnaves"
                simple/bakery0 "Bakery0"
                simple/bakery1 "Bakery1"
                )))

(deftest machine-clauses-test
  (testing "machine-clauses"
    (is (= {:tag    :operations,
            :values [{:tag :op, :name :inc, :args (), :body {:tag :assignment, :id-vals [:x {:tag :add, :nums [:x 1]}]}}
                     {:tag :op, :name :dec, :args (), :body {:tag :assignment, :id-vals [:x {:tag :sub, :nums [:x 1]}]}}]}
           (b (operations (:inc () (assign :x (+ :x 1))) (:dec () (assign :x (- :x 1)))))))
    (is (= {:tag    :sets,
            :values [{:tag :deferred-set, :id :E}
                     {:tag :enumerated-set, :id :F, :elems [:y :z :x]}]}
           (b (sets :E :F #{:x :y :z}))))
    (is (= {:tag    :operations
            :values [{:tag  :op
                      :name :inc
                      :args [:x]
                      :body {:tag     :assignment
                             :id-vals [:x {:tag :successor, :num :x}]}}]}
           (b (operations (:inc [:x] (assign :x (inc :x)))))))
    (is (= {:tag    :operations
            :values [{:tag     :op
                      :returns [:a]
                      :name    :inc
                      :args    [:x]
                      :body    {:tag     :assignment
                                :id-vals [:x {:tag :successor, :num :x}]}}]}
           (b (operations (<-- [:a] (:inc [:x] (assign :x (inc :x))))))))))

(deftest equality-predicates-test
  (testing "equality-predicates"
    (is (= {:tag :=
            :left true
            :right false}
           (b (= true false))))))

(deftest logical-predicates-test
  (testing "logical-predicates"
    (is (= {:tag :and
           :preds [{:tag :=, :left 1, :right 1}
                        {:tag :=, :left 2, :right 2}
                        {:tag :=, :left 3, :right 3}]}
           (b (and (= 1 1) (= 2 2) (= 3 3)))))))


(deftest minus-test
  (testing "minus is special and must have a unary version"
    (is (= {:tag    :unary-minus
            :num :a}
           (b (- :a)))))
  (testing "minus also works with more arguments"
    (is (= {:tag :sub
           :nums [:a :b]}
           (b (- :a :b))))
    (is (= {:tag :sub
            :nums [:a :b :c]}
           (b (- :a :b :c))))))


(deftest max-test
  (testing "max with a set"
    (is (= {:tag :max :set #{1 2 3}}
           (b (max #{1 2 3})))))
  (testing "max with more arguments"
    (is (= {:tag :max :set #{1 2}}
           (b (max 1 2))))
    (is (= {:tag :max :set #{1 2 3}}
           (b (max 1 2 3))))))

(deftest min-test
  (testing "min with a set"
    (is (= {:tag :min :set #{1 2 3}}
           (b (min #{1 2 3})))))
  (testing "min with more arguments"
    (is (= {:tag :min :set #{1 2}}
           (b (min 1 2))))
    (is (= {:tag :min :set #{1 2 3}}
           (b (min 1 2 3))))))


(deftest for-all-test
  (testing "universal quantification representation"
    (is (= {:tag         :for-all,
            :ids         [:x],
            :implication {:tag :implication, :preds [{:tag :member, :elem :x, :set {:tag :nat-set}}
                                                     {:tag :less-equals, :nums [:x 0]}]}}
           (b (for-all [:x] (member? :x nat-set) (<= :x 0)))))))

(deftest pred-test
  (testing "the pred macro allows to write b-macrofied expressions
            and returns a function"
    (is (fn? (pred [] (+ 1 2)))))
  
  (testing "the pred macro has a parameter list just like fn"
    (is (fn? (pred [x y] (+ 1 2)))))

  (testing "the resulting function generates a representation
            which replaces the parameter symbols with the values provided"
    (is (= {:tag :less :nums [1 2]}
           ((pred [x y] (< x y)) 1 2))))
  
  (testing "the pred macro flattens sets properly"
    (is (= (count ((pred [] #{:x :y})))
            2)))) 


(deftest let-tests
  (testing "one binding and predicate"
    (is (= {:tag         :let
            :id-vals [:foo 1 ]
            :expr-or-pred   {:tag :less, :nums [:foo 2]}}
           (b (let [:foo 1] (< :foo 2))))))
  (testing "more bindings and a predicate"
    (is (= {:tag         :let
            :id-vals [:foo 1 :bar 2]
            :expr-or-pred   {:tag :less, :nums [:foo :bar]}}
           (b (let [:foo 1 :bar 2] (< :foo :bar))))))
  (testing "one binding and expression"
    (is (= {:tag :let
            :id-vals [:foo 1]
            :expr-or-pred {:tag :add, :nums [:foo 2]}}
           (b (let [:foo 1] (+ :foo 2))))))
  (testing "more bindings and an expression"
    (is (= {:tag         :let
            :id-vals [:foo 1 :bar 2]
            :expr-or-pred   {:tag :add, :nums [:foo :bar]}}
           (b (let [:foo 1 :bar 2] (+ :foo :bar)))))))
