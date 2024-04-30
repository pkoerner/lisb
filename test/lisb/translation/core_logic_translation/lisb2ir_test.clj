(ns lisb.translation.core-logic-translation.lisb2ir-test
  (:require [clojure.test :refer :all]
            [lisb.translation.core-logic-translation.lisb2ir :refer :all]))

(deftest simple-add-lisb->ir
  (testing "translate simple lisb (add)"
    (is (= {:tag :add :nums [1 2 3]} (first (lisb->ir '(+ 1 2 3)))))))

(deftest simple-add-ir->lisb
  (testing "translate simple ir (add)"
    (is (= '(+ 1 2 3) (first (ir->lisb {:tag :add :nums [1 2 3]}))))))

(deftest simple-or-lisb->ir
  (testing "translate simple lisb (or)"
    (is (= {:tag :or :preds [true true false]} (first (lisb->ir '(or true true false)))))))

(deftest simple-or-ir->lisb
  (testing "translate simple ir (or)"
    (is (= '(or true true false) (first (ir->lisb {:tag :or :preds [true true false]})))) ))

(deftest nat-set-lisb->ir
  (testing "translate nat-set to {:tag :nat-set}]"
    (is (= {:tag :nat-set} (first (lisb->ir 'nat-set))))))

(deftest nat-set-ir->lisb
  (testing "translato {:tag :nat-set} to nat-set"
    (is (= 'nat-set (first (ir->lisb {:tag :nat-set}))))))


; nested expressions, one arg-type

(deftest nested-add-ir->lisb
  (testing "Übersetze nested Addition in IR zu nested lisb-Addition"
    (is (= '(+ 1 2 3 (+ 4 5)) (first (ir->lisb {:tag :add :nums [1 2 3 {:tag :add :nums [4 5]}]}))))))

(deftest nested-add-lisb->ir
  (testing "Übersetze nested Addition in lisb zu nested IR-Addition"
    (is (= {:tag :add :nums [1 2 3 {:tag :add :nums [4 5]}]} (first (lisb->ir '(+ 1 2 3 (+ 4 5))))))))
  
; simple expression, multiple arg-types

(deftest simple-multiple-arg-types-lisb->ir
  (testing "Übersetze lisb mit mehreren Argument-Typen zu entsprechender IR"
    (is (= {:tag :comprehension-set :ids (list "id1" "id2" "id3") :pred {:tag :member, :elem :x, :set {:tag :nat-set}}}
           (first (lisb->ir '(comprehension-set ["id1" "id2" "id3"] (member? :x nat-set))))))))
  
(deftest simple-multiple-arg-types-ir->lisb
  (testing "Übersetze IR mit mehreren Argument-Typen zu entsprechendem lisb" 
    (is (= '(comprehension-set ("id1" "id2" "id3") (member? :x nat-set)) (first (ir->lisb {:tag :comprehension-set :ids (list "id1" "id2" "id3") :pred {:tag :member :elem :x :set {:tag :nat-set}}}))))))

; syntactic sugar

(deftest lisb->ir-syntactic-sugar 
  (testing "Sinngleiche Operatoren sollen gleich übersetzt werden"
    (is (= {:tag :implication :preds '(true true)} (first (lisb->ir '(=> true true)))))
    (is (= (first (lisb->ir '(implication true true))) (first (lisb->ir '(=> true true)))))))

; operator "overloading"

(deftest lisb->ir->overloaded-operator
  (testing "Gleicher Operator soll jeweils zu richtiger IR übersetzt werden"
    (is (= {:tag :if-sub :cond :some-cond :then :some-then} (first (lisb->ir '(if-sub :some-cond :some-then)))))
    (is (= {:tag :if-sub :cond :some-cond :then :some-then :else :some-else} (first (lisb->ir '(if-sub :some-cond :some-then :some-else)))))))

(deftest ir->lisb->overloaded-operator
  (testing "Gleicher Operator soll jeweils zu richtigem lisb übersetzt werden"
    (is (= '(if-sub :some-cond :some-then) (first (ir->lisb {:tag :if-sub :cond :some-cond :then :some-then}))))
    (is (= '(if-sub :some-cond :some-then :some-else) (first (ir->lisb {:tag :if-sub :cond :some-cond :then :some-then :else :some-else}))))))

; SPECIAL CASES

;; test taken from lisb2ir_test.clj in lisb

(deftest lisb->ir-for-all
  (testing "translate for-all sugar"
    (is (= {:tag :for-all :ids [:x],
            :implication {:tag :implication, :preds [{:tag :member, :elem :x, :set {:tag :nat-set}} {:tag :less-equals, :nums [:x 0]}]}}
          (first (lisb->ir '(for-all [:x] (member? :x nat-set) (<= :x 0))))))
    (is (= '(for-all [:x] (implication (member? :x nat-set) (<= :x 0)))
            (first (ir->lisb {:tag :for-all :ids [:x],
                               :implication {:tag :implication, 
                                             :preds [{:tag :member, :elem :x, :set {:tag :nat-set}} {:tag :less-equals, :nums [:x 0]}]}}))))))

(deftest logical-predicates-test
  (testing "logical-predicates"
    (is (= {:tag :and
            :preds [{:tag :equals, :left 1, :right 1}
                    {:tag :equals, :left 2, :right 2}
                    {:tag :equals, :left 3, :right 3}]}
           (first (lisb->ir '(and (= 1 1) (= 2 2) (= 3 3))))))))

;; " <-- " operator

(deftest op-lisb->ir
  (testing "translate '<--' to :op IR when given name as argument"
    (is (= {:tag :op :returns [ 1 2] :name :somename :args [] :body {:tag :less :nums [1 2]}}
            (first (lisb->ir '(<-- [1 2] (:somename [] (< 1 2)))))))))

(deftest op-ir->lisb
  (testing "translate :op to <-- with name in body when :name in IR"
    (is (= '(<-- (1 2) (:somename [] (< 1 2)))
            (first (ir->lisb {:tag :op :returns [1 2] :name :somename :args [] :body {:tag :less :nums [1 2]}}))))))
            

(deftest op-call-lisb->ir
  (testing "translate '<--' to :op-call IR when given op-call as argument"
    (is (= {:tag :op-call :returns [1 2] :op :someop :args []}
            (first (lisb->ir '(<-- [1 2] (op-call :someop []))))))))

;; 
(deftest op-call-ir->lisb
  (testing "translate :op-call to '<--' with op-call in body when :op in IR"
    (is (= '(<-- [1 2] (op-call :someop []))
            (first (ir->lisb {:tag :op-call :returns [1 2] :op :someop :args []}))))))

(deftest name-op-lisb->ir
  (testing "translate op name from lisb to IR"
    (is (= {:tag :op :returns [] :name :opname :args [] :body {:tag :add, :nums [1 2]}}
            (first (lisb->ir '(:opname [] (+ 1 2))))))))

(deftest name-op-ir->lisb
  (testing "translate op name from IR to lisb"
    (is (= '(:opname [] (+ 1 2))
            (first (ir->lisb 
                    {:tag :op :returns [] :name :opname :args [] :body {:tag :add, :nums [1 2]}}))))))

 
(deftest refinement
  (testing "translate refinement with machine-ref correctly"
    (is (= '(refinement :somename :someref (+ 1 2 3))
           (first (ir->lisb {:tag :refinement 
                              :name :somename 
                              :abstract-machine-name {:tag :machine-reference, :name :someref} 
                              :machine-clauses [{:tag :add, :nums [1 2 3]}]}))))
    (is (= {:tag :refinement 
            :name :somename 
            :abstract-machine-name {:tag :machine-reference, :name :someref} 
            :machine-clauses {:tag :add, :nums [1 2 3]}}
           (first (lisb->ir '(refinement :somename :someref (+ 1 2 3))))))))

