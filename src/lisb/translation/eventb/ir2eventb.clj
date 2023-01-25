(ns lisb.translation.eventb.ir2eventb
  (:require [lisb.translation.util :refer [b]]
            [lisb.translation.ir2ast :refer [ir->ast id-vals->ids id-vals->vals ir->ast-node]])
  (:import
    de.prob.animator.domainobjects.EventB
    (de.prob.model.eventb
            EventBMachine
            EventBInvariant
            EventBAction
            EventBGuard
            Event
            Context)))

(defmulti ir-node->prob-node (fn [ir-node] (:tag ir-node)))

(defmethod ir-node->prob-node :assignment [ir-node]
  (s/assert (s/keys :req-un [::id-vals]) ir-node)
  (let [id-vals (:id-vals ir-node)
        ids (id-vals->ids id-vals)
        vals (id-vals->vals id-vals)]
    (AAssignSubstitution. ids vals)))

(deftest substitutions-test
  (testing "substitutions"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                "skip" (b skip)
                "x := E" (b (assign :x :E))
                "x,y := E,F" (b (assign :x :E :y :F))
                "f(x) := E" (b (assign (fn-call :f :x) :E))
                "x::S" (b (becomes-element-of [:x] :S))
                "x :(x>0) " (b (becomes-such [:x] (> :x 0)))
                "op(x)" (b (op-call :op :x))
                "a<--op(x)" (b (<-- [:a] (op-call :op :x)))
                "skip || skip" (b (parallel-sub skip skip))
                "skip || skip || skip" (b (parallel-sub skip skip skip))
                "skip ; skip" (b (sequential-sub skip skip))
                "skip ; skip ; skip" (b (sequential-sub skip skip skip))
                "ANY x WHERE x>0 THEN skip END " (b (any [:x] (> :x 0) skip))
                "ANY x WHERE x>0 THEN skip ; skip END " (b (any [:x] (> :x 0) skip skip))
                "LET x BE x=1 IN skip END " (b (let-sub [:x 1] skip))
                "LET x BE x=1 IN skip ; skip END " (b (let-sub [:x 1] skip skip))
                "VAR x IN skip END " (b (bvar #{:x} skip))
                "VAR x IN skip ; skip END " (b (bvar #{:x} skip skip))
                "PRE 1=2 THEN skip END " (b (pre (= 1 2) skip))
                "PRE 1=2 THEN skip ; skip END " (b (pre (= 1 2) skip skip))
                "ASSERT 1=2 THEN skip END " (b (assert (= 1 2) skip))
                "ASSERT 1=2 THEN skip ; skip END " (b (assert (= 1 2) skip skip))
                "CHOICE skip OR skip END " (b (choice skip skip))
                "IF 1=2 THEN skip END " (b (if-sub (= 1 2) skip))
                "IF 1=2 THEN skip ELSE skip END " (b (if-sub (= 1 2) skip skip))
                "IF 1=2 THEN skip ELSIF 1=3 THEN skip END " (b (cond (= 1 2) skip (= 1 3) skip))
                "IF 1=2 THEN skip ELSIF 1=3 THEN skip ELSE skip END " (b (cond (= 1 2) skip (= 1 3) skip skip))
                "SELECT 1=2 THEN skip END " (b (select (= 1 2) skip))
                "SELECT 1=2 THEN skip ELSE x := 1 END " (b (select (= 1 2) skip (assign :x 1)))
                "SELECT 1=1 THEN skip WHEN 2=2 THEN skip END " (b (select (= 1 1) skip (= 2 2) skip))
                "SELECT 1=1 THEN skip WHEN 2=2 THEN skip ELSE skip END " (b (select (= 1 1) skip (= 2 2) skip skip))
                "CASE 1+1 OF EITHER 1 THEN skip OR 2 THEN skip END END " (b (case (+ 1 1) 1 skip 2 skip))
                "CASE 1+1 OF EITHER 1 THEN skip OR 2 THEN skip ELSE skip END END " (b (case (+ 1 1) 1 skip 2 skip skip))
                "CASE 1+1 OF EITHER 1 THEN skip OR 2 THEN skip ELSE skip END END " (b (case (+ 1 1) 1 skip 2 skip skip))
                )))

(comment
  (EventB. "x,y:=1,2")
  (= (EventBAction. "act1" "x,y:=1,2" #{})
     (EventBAction. "act1" (EventB. "x,y:=1,2") ""))

  (EventBAction. "act2" "f(x):=E" #{})
  (.getAst (.getCode (EventBAction. "act1" "x:=E" #{})))
  (ir->ast-node (b (assign :x :E)))
  )
