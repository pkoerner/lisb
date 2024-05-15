(ns lisb.translation.core-logic-translation.lisb2ir-test
  (:require [clojure.test :refer :all]
            [lisb.translation.core-logic-translation.lisb2ir :refer :all]))

(deftest simple-add-lisb->ir
  (testing "translate simple lisb (add)"
    (is (= {:tag :add :nums [1 2 3]} (lisb->ir '(+ 1 2 3))))))

(deftest simple-add-ir->lisb
  (testing "translate simple ir (add)"
    (is (= '(+ 1 2 3) (ir->lisb {:tag :add :nums [1 2 3]})))))

(deftest simple-or-lisb->ir
  (testing "translate simple lisb (or)"
    (is (= {:tag :or :preds [true true false]} (lisb->ir '(or true true false))))))

(deftest simple-or-ir->lisb
  (testing "translate simple ir (or)"
    (is (= '(or true true false) (ir->lisb {:tag :or :preds [true true false]}))) ))

(deftest nat-set-lisb->ir
  (testing "translate nat-set to {:tag :nat-set}]"
    (is (= {:tag :nat-set} (lisb->ir 'nat-set)))))

(deftest nat-set-ir->lisb
  (testing "translato {:tag :nat-set} to nat-set"
    (is (= 'nat-set (ir->lisb {:tag :nat-set})))))


; nested expressions, one arg-type

(deftest nested-add-ir->lisb
  (testing "Übersetze nested Addition in IR zu nested lisb-Addition"
    (is (= '(+ 1 2 3 (+ 4 5)) (ir->lisb {:tag :add :nums [1 2 3 {:tag :add :nums [4 5]}]})))))

(deftest nested-add-lisb->ir
  (testing "Übersetze nested Addition in lisb zu nested IR-Addition"
    (is (= {:tag :add :nums [1 2 3 {:tag :add :nums [4 5]}]} (lisb->ir '(+ 1 2 3 (+ 4 5)))))))

; simple expression, multiple arg-types

(deftest simple-multiple-arg-types-lisb->ir
  (testing "Übersetze lisb mit mehreren Argument-Typen zu entsprechender IR"
    (is (= {:tag :comprehension-set :ids (list "id1" "id2" "id3") :pred {:tag :member, :elem :x, :set {:tag :nat-set}}}
           (lisb->ir '(comprehension-set ["id1" "id2" "id3"] (member? :x nat-set)))))))

(deftest simple-multiple-arg-types-ir->lisb
  (testing "Übersetze IR mit mehreren Argument-Typen zu entsprechendem lisb" 
    (is (= '(comprehension-set ("id1" "id2" "id3") (contains? nat-set :x)) (ir->lisb {:tag :comprehension-set :ids ["id1" "id2" "id3"] :pred {:tag :member :elem :x :set {:tag :nat-set}}})))))

; syntactic sugar

(deftest lisb->ir-syntactic-sugar 
  (testing "Sinngleiche Operatoren sollen gleich übersetzt werden"
    (is (= {:tag :implication :preds '(true true)} (lisb->ir '(=> true true))))
    (is (= (lisb->ir '(implication true true)) (lisb->ir '(=> true true))))))

; operator "overloading"

(deftest lisb->ir->overloaded-operator
  (testing "Gleicher Operator soll jeweils zu richtiger IR übersetzt werden"
    (is (= {:tag :if-sub :cond :some-cond :then :some-then} (lisb->ir '(if-sub :some-cond :some-then))))
    (is (= {:tag :if-sub :cond :some-cond :then :some-then :else :some-else} (lisb->ir '(if-sub :some-cond :some-then :some-else))))))

(deftest ir->lisb->overloaded-operator
  (testing "Gleicher Operator soll jeweils zu richtigem lisb übersetzt werden"
    (is (= '(if-sub :some-cond :some-then) (ir->lisb {:tag :if-sub :cond :some-cond :then :some-then})))
    (is (= '(if-sub :some-cond :some-then :some-else) (ir->lisb {:tag :if-sub :cond :some-cond :then :some-then :else :some-else})))))

; SPECIAL CASES

;; test taken from lisb2ir_test.clj in lisb

(deftest lisb->ir-for-all
  (testing "translate for-all sugar"
    (is (= {:tag :for-all :ids [:x],
            :implication {:tag :implication, :preds [{:tag :member, :elem :x, :set {:tag :nat-set}} {:tag :less-equals, :nums [:x 0]}]}}
           (lisb->ir '(for-all [:x] (member? :x nat-set) (<= :x 0)))))
    (is (= '(for-all [:x] (implication (contains? nat-set :x) (<= :x 0)))
           (ir->lisb {:tag :for-all :ids [:x],
                      :implication {:tag :implication, 
                                    :preds [{:tag :member, :elem :x, :set {:tag :nat-set}} {:tag :less-equals, :nums [:x 0]}]}})))))

(deftest logical-predicates-test
  (testing "logical-predicates"
    (is (= {:tag :and
            :preds [{:tag :equals, :left 1, :right 1}
                    {:tag :equals, :left 2, :right 2}
                    {:tag :equals, :left 3, :right 3}]}
           (lisb->ir '(and (= 1 1) (= 2 2) (= 3 3)))))))

;; " <-- " operator

(deftest op-lisb->ir
  (testing "translate '<--' to :op IR when given name as argument"
    (is (= {:tag :op :returns [ 1 2] :name :somename :args [] :body {:tag :less :nums [1 2]}}
           (op->ir '(<-- [1 2] (:somename [] (< 1 2))))))))

(deftest op-ir->lisb
  (testing "translate :op to <-- with name in body when :name in IR"
    (is (= '(<-- (1 2) (:somename [] (< 1 2)))
           (ir->op {:tag :op :returns [1 2] :name :somename :args [] :body {:tag :less :nums [1 2]}})))))


(deftest op-call-lisb->ir
  (testing "translate '<--' to :op-call IR when given op-call as argument"
    (is (= {:tag :op-call :returns [1 2] :op :someop :args []}
           (lisb->ir '(<-- [1 2] (op-call :someop [])))))))

;; 
(deftest op-call-ir->lisb
  (testing "translate :op-call to '<--' with op-call in body when :op in IR"
    (is (= '(<-- [:a :b] (op-call :someop []))
           (ir->lisb {:tag :op-call :returns [:a :b] :op :someop :args []})))))

(deftest name-op-lisb->ir
  (testing "translate op name from lisb to IR"
    (is (= {:tag :op :returns [] :name :opname :args [] :body {:tag :add, :nums [1 2]}}
           (op->ir '(:opname [] (+ 1 2)))))))

(deftest name-op-ir->lisb
  (testing "translate op name from IR to lisb"
    (is (= '(:opname [] (+ 1 2))
           (ir->op 
             {:tag :op :returns [] :name :opname :args [] :body {:tag :add, :nums [1 2]}})))))


(deftest refinement
  (testing "translate refinement with machine-ref correctly"
    (is (= '(refinement :somename :someref (invariants true))
           (ir->lisb {:tag :refinement 
                      :name :somename 
                      :abstract-machine-name {:tag :machine-reference, :name :someref} 
                      :machine-clauses [{:tag :invariants, :values [true]}]})))
    (is (= {:tag :refinement 
            :name :somename 
            :abstract-machine-name {:tag :machine-reference, :name :someref} 
            :machine-clauses [{:tag :invariants, :values [true]}]}
           (lisb->ir '(refinement :somename :someref (invariants true)))))))


(deftest misc-test
  (testing "a small number of test cases I used doing development"
    (are [x y] (= x y) 
         (lisb->ir #{1 2 3})
           #{1 2 3}
         (lisb->ir #{1 '(+ 1 1) 3})
           #{1 {:tag :add, :nums [1 1]} 3}
         (lisb->ir '(<-- [:a :b] (op-call :someop [:c :d]))) 
           {:tag :op-call, :returns [:a :b], :op :someop, :args [:c :d]}
         (lisb->ir '(<-- [:a :b] (op-call :someop []))) 
           {:tag :op-call, :returns [:a :b], :op :someop, :args []} 
         (ir->lisb {:tag :op-call, :returns :res, :op :someop, :args :bla}) 
           '(<-- :res (op-call :someop :bla))
         (op->ir '(<-- :res (:somename :args (< 1 2)))) 
           '{:tag :op, :returns :res, :name :somename, :args :args, :body {:tag :less, :nums (1 2)}}
         (ir->lisb '{:tag :op, :returns :res, :name :somename, :args :args, :body {:tag :less, :nums (1 2)}}) 
           '(op :res :somename :args (< 1 2))
         (lisb->ir '(op-call :res :someop :bla)) 
           {:tag :op-call, :returns :res, :op :someop, :args :bla}
         (lisb->ir '(=> (+ 1 2 3) :bar)) 
           '{:tag :implication, :preds ({:tag :add, :nums (1 2 3)} :bar)}
         (lisb->ir '(for-all [:x] (member? :x nat-set) (<= :x 0)))
           '{:tag :for-all, :ids [:x], :implication {:tag :implication, :preds ({:tag :member, :elem :x, :set {:tag :nat-set}} {:tag :less-equals, :nums (:x 0)})}}
         (lisb->ir '(=> :foo :bar))
           '{:tag :implication, :preds (:foo :bar)}
         (lisb->ir '(+ :foo :bar))
           '{:tag :add, :nums (:foo :bar)}
         (lisb->ir '(assign :foo 42 :bar 43))
           '{:tag :assignment, :id-vals (:foo 42 :bar 43)}
         (lisb->ir '(+ "a" "b"))
           '{:tag :add, :nums ("a" "b")}
         (lisb->ir '["a" "b"])
           '["a" "b"]
         (lisb->ir '(pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                         (assign (fn-call :curDeadlines :timer) :deadline)))
           '{:tag :precondition, :pred {:tag :and, :preds ({:tag :member, :set :TIMERS, :elem :timer}
                                                           {:tag :member, :set {:tag :natural-set}, :elem :deadline})}, 
                                 :subs ({:tag :assignment, :id-vals ({:tag :fn-call, :f :curDeadlines, :args :timer} :deadline)})}
         (lisb->ir '(and (contains? :TIMERS :timer) (contains? natural-set :deadline)))
           '{:tag :and, :preds ({:tag :member, :set :TIMERS, :elem :timer} {:tag :member, :set {:tag :natural-set}, :elem :deadline})}
         (lisb->ir '(contains? :TIMERS :timer))
           '{:tag :member, :set :TIMERS, :elem :timer}
         (lisb->ir '(member? :TIMERS :timer))
           '{:tag :member, :elem :TIMERS, :set :timer}
         (lisb->ir '(machine :foo
                             (constants :bar)
                             (operations (:AbsoluteSetDeadline [:timer :deadline]
                                                               (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                                                    (assign (fn-call :curDeadlines :timer) :deadline)))
                                         (<-- returns (:foo [] (assign :bla 42))))))
           '{:tag :machine, :name :foo, :machine-clauses ({:tag :constants, :values (:bar)} 
                                                          {:tag :operations, :values ({:tag :op, :name :AbsoluteSetDeadline, :returns [], :args [:timer :deadline], 
                                                                                       :body {:tag :precondition, :pred {:tag :and, :preds ({:tag :member, :set :TIMERS, :elem :timer} 
                                                                                                                                            {:tag :member, :set {:tag :natural-set}, :elem :deadline})},
                                                                                              :subs ({:tag :assignment, :id-vals ({:tag :fn-call, :f :curDeadlines, :args :timer} :deadline)})}}
                                                                                      {:tag :op, :returns returns, :name :foo, :args [], :body {:tag :assignment, :id-vals (:bla 42)}})})})))

(deftest cycle-test
  (testing "the bi-directional transformation works"
    (are [x] (= x (ir->lisb (lisb->ir x)))
         '(implication (implication :quux :foo) :bar) 
         'nat-set
         '(<-- [:a :b] (op-call :someop [:c :d]))
         '(<-- [:a :b] (op-call :someop []))
         '(<-- :res (op-call :someop :bla))
         '(implication (+ 1 2 3) :bar)
         '(for-all [:x] (implication (contains? nat-set :x) (<= :x 0)))
         '(implication (+ 1 2 3) (* (+ 1 4) 5) (* 6 7 8 ))
         '(implication :foo :bar)
         '(+ :foo :bar)
         '(assign :foo 42 :bar 43)
         '(+ "a" "b")
         '["a" "b"]
         #{1 2 3}
         '#{1 (+ 1 1) 3}
         '(pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
               (assign (fn-call :curDeadlines :timer) :deadline))
         '(and (contains? :TIMERS :timer) (contains? natural-set :deadline ))
         '(contains? :TIMERS :timer)
         '(machine :foo
                   (constants :bar)
                   (operations (:AbsoluteSetDeadline [:timer :deadline]
                                                     (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                                          (assign (fn-call :curDeadlines :timer) :deadline)))
                               (<-- returns (:foo [] (assign :bla 42)))))
         '(operations (:AbsoluteSetDeadline [:timer :deadline]
                                            (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                                 (assign (fn-call :curDeadlines :timer) :deadline)))
                      (<-- returns (:foo [] (assign :bla 42)))))))
