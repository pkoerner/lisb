(ns lisb.translation.eventb.ir2eventb-test
  (:require [lisb.translation.eventb.dsl :refer [eventb]]
            [lisb.translation.eventb.ir2eventb :refer [ir->prob ir-expr->str ir-pred->str ir-sub->strs]]
            [clojure.test :refer :all]
            ))


(deftest expr-test
  (testing "Expressions"
    (are [eventb ir] (= eventb (ir-expr->str ir))
      "x+E" (eventb (+ :x :E))
      "2*(x+E)" (eventb (* 2 (+ :x :E)))
      "1/3" (eventb (/ 1 3))
      "{}" (eventb #{})
      "{1}" (eventb #{1})
      "{1,x}" (eventb #{:x 1}) ;;enumerated sets are not orderd!
      "DOMAIN-->RANGE" (eventb (--> :DOMAIN :RANGE))
      "(%(x|->y)|->z.(x:0..5&y:0..5&z:0..5)|x+y+z)" (eventb (lambda [:x :y :z] (contains? (interval 0 5) :x :y :z) (+ :x :y :z)))
      )))

(deftest pred-test
  (testing "Predicates"
    (are [eventb ir] (= eventb (ir-pred->str ir))
      "(x<10&y>0)" (eventb (and (< :x 10) (> :y 0)))
      "(a<<:b&b<<:c)" (eventb (strict-subset? :a :b :c))
      "(1=x or 1=1)" (eventb (or (= 1 :x) (= 1 1)))
      "partition(X,{x},{y})" (eventb (partition :X #{:x} #{:y}))
      )))


(defn action-code [a] (-> a .getCode .getCode))
(defn guard-code [g] (-> g .getPredicate .getCode))

(deftest action-test
  (are [actions ir] (= actions (ir-sub->strs ir))
    ["x := 1"] (eventb (assign :x 1))
    ["f(x) := 1"] (eventb (assign (fn-call :f :x) 1))
    ["x,y := 1,x"] (eventb (assign :x 1 :y :x))
    ["x := 1" "y := TRUE"] (eventb (|| (assign :x 1) (assign :y true)))
    ["z := 3" "w := x" "x := 1" "y := 2"] (eventb (|| (|| (assign :z 3) (assign :w :x)) (assign :x 1) (assign :y 2)))
    ["x :: NAT"] (eventb (becomes-element-of [:x] nat-set))
    ["x,y :| (x'>2&y'>x')"] (eventb (becomes-such [:x :y] (and (> :x' 2) (> :y' :x'))))
    ))

(defn find-first-by-name [event-name events]
 (->> events
       (filter (fn [x] (= event-name (.getName x))))
       first))

(defn get-actions [event]
  (->> event
       .getActions
       (map action-code)))

(defn get-guards [event]
  (->> event
       .getGuards
       (map guard-code)
       ))

(deftest empty-machine-test
  (let [machine (ir->prob (eventb (machine :empty)))]
    (is (= () (.getEvents machine)))))

(deftest prob-machine-test
  (let [ir (eventb (machine :hello-world
                       (variables :x :y :hello)
                       (invariants
                        (in :hello bool-set)
                        (<= :x 10)
                        (in :y nat-set))
                       (init
                        (assign :x 0 :y 50)
                        #_(assign :hello true)) ; TODO: disabled because of parallel/sequential composition difference
                       (events
                        (event :inc (when (< :x 10)) (then (assign :x (+ :x 1))))
                        (event :hello (then (assign :hello false))))))
        machine (ir->prob ir)
        events (.getEvents machine)
        invariants (.getInvariants machine)]
    (is (= ["x" "y" "hello"] (map (fn [x] (.getName x)) (.getVariables machine))))
    (is (= ["hello:BOOL" "x<=10" "y:NAT"] (map (fn [x] (.getCode (.getPredicate x))) invariants)))
    (is (= ["INITIALISATION" "inc" "hello"] (map (fn [x] (.getName x)) events)))
    (is (= ["x,y := 0,50" #_"hello := TRUE"] (get-actions (find-first-by-name "INITIALISATION" events))))
    (is (= ["x<10"] (get-guards (find-first-by-name "inc" events))))
    (is (= ["x := x+1"] (get-actions (find-first-by-name "inc" events))))
    (is (= [] (get-guards (find-first-by-name "hello" events))))
    (is (= ["hello := FALSE"] (get-actions (find-first-by-name "hello" events))))
    ))


