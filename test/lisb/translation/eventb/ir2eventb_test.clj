(ns lisb.translation.eventb.ir2eventb-test
  (:require [lisb.translation.util :refer [b]]
            [lisb.translation.eventb.ir2eventb :refer :all]
            [clojure.test :refer [are deftest is run-tests testing]]
            ))

(deftest expr-test
  (testing "Expressions"
    (are [eventb ir] (= eventb (ir-expr->str ir))
      "x+E" (b (+ :x :E))
      "2*(x+E)" (b (* 2 (+ :x :E)))
      "1/3" (b (/ 1 3))
      "{}" (b #{})
      "{1}" (b #{1})
      "{1,x}" (b #{:x 1}) ;;enumerated sets are not orderd!
      )))

(deftest pred-test
  (testing "Predicates"
    (are [eventb ir] (= eventb (ir-pred->str ir))
      "x<10&y>0" (b (and (< :x 10) (> :y 0)))
      "a<<:b&b<<:c" (b (strict-subset? :a :b :c))
      "1=x or 1=1" (b (or (= 1 :x) (= 1 1)))
      )))

(deftest action-test
  (are [actions ir] (= actions (ir-sub->actions-codes ir))
    ["x := 1"] (b (assign :x 1))
    ["f(x) := 1"] (b (assign (fn-call :f :x) 1))
    ["x,y := 1,x"] (b (assign :x 1 :y :x))
    ["x := 1" "y := TRUE"] (b (|| (assign :x 1) (assign :y true)))
    ["z := 3" "w := x" "x := 1" "y := 2"] (b (|| (|| (assign :z 3) (assign :w :x)) (assign :x 1) (assign :y 2)))
    ))

(defn get-actions [event-name events]
  (->> events
       (filter (fn [x] (= event-name (.getName x))))
       first
       .getActions
       (map (fn [x] (.getCode (.getCode x))))))

(defn get-guards [event-name events]
  (->> events
       (filter (fn [x] (= event-name (.getName x))))
       first
       .getGuards
       (map (fn [x] (.getCode (.getPredicate x))))
       ))

(deftest prob-machine-test
  (let [ir (b (machine :hello-world
                       (variables :x :y :hello)
                       (invariants
                        (in :hello bool-set)
                        (<= :x 10)
                        (in :y nat-set))
                       (init
                        (assign :x 0 :y 50)
                        (assign :hello true))
                       (operations
                        (:inc [] (pre (< :x 10) (assign :x (+ :x 1))))
                        (:hello [] (assign :hello false)))))
        machine (ir->prob-machine ir)
        events (.getEvents machine)
        invariants (.getInvariants machine)]
    (is (= ["x" "y" "hello"] (map (fn [x] (.getName x)) (.getVariables machine))))
    (is (= ["hello:BOOL" "x<=10" "y:NAT"] (map (fn [x] (.getCode (.getPredicate x))) invariants)))
    (is (= ["INITIALISATION" "inc" "hello"] (map (fn [x] (.getName x)) events)))
    (is (= ["x,y := 0,50" "hello := TRUE"] (get-actions "INITIALISATION" events)))
    (is (= ["x<10"] (get-guards "inc" events)))
    (is (= ["x := x+1"] (get-actions "inc" events)))
    (is (= [] (get-guards "hello" events)))
    (is (= ["hello := FALSE"] (get-actions "hello" events)))
    ))

