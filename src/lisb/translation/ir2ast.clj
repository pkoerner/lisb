(ns lisb.translation.ir2ast
  (:require [lisb.translation.lisb2ir :refer [b= bnot= band bmember? bimplication]])
  (:import (de.be4.classicalb.core.parser.node Start
                                               EOF
                                               AAbstractMachineParseUnit
                                               AMachineMachineVariant
                                               AMachineHeader
                                               AVariablesMachineClause
                                               AInvariantMachineClause
                                               AInitialisationMachineClause
                                               ABlockSubstitution
                                               AAssignSubstitution
                                               AAddExpression
                                               AMinusExpression
                                               AMultOrCartExpression
                                               AMinusOrSetSubtractExpression
                                               ADivExpression
                                               AUnaryMinusExpression
                                               AIntegerExpression
                                               TIntegerLiteral
                                               ABooleanTrueExpression
                                               ABooleanFalseExpression
                                               AConvertBoolExpression
                                               AIdentifierExpression
                                               AEmptySetExpression
                                               ASetExtensionExpression
                                               AComprehensionSetExpression
                                               APowSubsetExpression
                                               APow1SubsetExpression
                                               AFinSubsetExpression
                                               AFin1SubsetExpression
                                               ACardExpression
                                               AUnionExpression
                                               AIntersectionExpression
                                               ASetSubtractionExpression
                                               AMemberPredicate
                                               ASubsetPredicate
                                               ASubsetStrictPredicate
                                               ABoolSetExpression
                                               ANaturalSetExpression
                                               ANatural1SetExpression
                                               AIntegerSetExpression
                                               AIntSetExpression
                                               ANatSetExpression
                                               ANat1SetExpression
                                               TIntegerLiteral
                                               TIdentifierLiteral
                                               AConjunctPredicate
                                               ADisjunctPredicate
                                               ANegationPredicate
                                               AEqualPredicate
                                               ANotEqualPredicate
                                               AEquivalencePredicate
                                               ALessPredicate
                                               AGreaterPredicate
                                               ALessEqualPredicate
                                               AGreaterEqualPredicate
                                               AMaxExpression
                                               AMinExpression
                                               AModuloExpression
                                               ACoupleExpression
                                               ARelationsExpression
                                               ADomainExpression
                                               ARangeExpression
                                               AIdentityExpression
                                               ADomainRestrictionExpression
                                               ADomainSubtractionExpression
                                               ARangeRestrictionExpression
                                               ARangeSubtractionExpression
                                               AReverseExpression
                                               AImageExpression
                                               AOverwriteExpression
                                               ADirectProductExpression
                                               ACompositionExpression
                                               AParallelProductExpression
                                               AFirstProjectionExpression
                                               ASecondProjectionExpression
                                               AClosureExpression
                                               AReflexiveClosureExpression
                                               AIterationExpression
                                               ATransFunctionExpression
                                               ATransRelationExpression
                                               APartialFunctionExpression
                                               ATotalFunctionExpression
                                               APartialSurjectionExpression
                                               ATotalSurjectionExpression
                                               APartialInjectionExpression
                                               ATotalInjectionExpression
                                               APartialBijectionExpression
                                               ATotalBijectionExpression
                                               ALambdaExpression
                                               AFunctionExpression
                                               AImplicationPredicate
                                               AForallPredicate
                                               AExistsPredicate
                                               AIntervalExpression
                                               ASequenceExtensionExpression
                                               AEmptySequenceExpression
                                               AIseqExpression
                                               AIseq1Expression
                                               APermExpression
                                               AConcatExpression
                                               AInsertFrontExpression
                                               AInsertTailExpression
                                               ARevExpression
                                               AFirstExpression
                                               ALastExpression
                                               AFrontExpression
                                               ATailExpression
                                               ARestrictFrontExpression
                                               ARestrictTailExpression
                                               APowerOfExpression
                                               AGeneralSumExpression
                                               AGeneralProductExpression
                                               AGeneralUnionExpression
                                               AGeneralIntersectionExpression
                                               ASeqExpression
                                               ASeq1Expression
                                               AGeneralConcatExpression
                                               AIfThenElseExpression
                                               AQuantifiedIntersectionExpression
                                               AQuantifiedUnionExpression
                                               ARecEntry
                                               AStructExpression
                                               ARecExpression
                                               ARecordFieldExpression
                                               AStringExpression
                                               AStringSetExpression
                                               TStringLiteral
                                               ADefinitionExpression
                                               ALetExpressionExpression
                                               ALetPredicatePredicate
                                               AConstantsMachineClause
                                               APropertiesMachineClause
                                               AConstraintsMachineClause
                                               ASetsMachineClause
                                               ADeferredSetSet
                                               AEnumeratedSetSet
                                               ADefinitionsMachineClause
                                               AAssertionsMachineClause
                                               AOperationsMachineClause
                                               ASkipSubstitution
                                               ABecomesElementOfSubstitution
                                               AOperationCallSubstitution
                                               AAnySubstitution
                                               APredicateParseUnit
                                               AExpressionParseUnit
                                               ANotMemberPredicate
                                               ANotSubsetPredicate
                                               ANotSubsetStrictPredicate
                                               AMaxIntExpression
                                               AMinIntExpression
                                               ASuccessorExpression
                                               APredecessorExpression
                                               ATotalRelationExpression
                                               ATotalSurjectionRelationExpression
                                               ASurjectionRelationExpression
                                               ASizeExpression
                                               ASubstitutionParseUnit
                                               ABecomesSuchSubstitution
                                               AAssertionSubstitution
                                               APreconditionSubstitution
                                               AVarSubstitution
                                               ALetSubstitution
                                               ASequenceSubstitution
                                               AParallelSubstitution
                                               AMachineClauseParseUnit
                                               AChoiceSubstitution AChoiceOrSubstitution
                                               AIfElsifSubstitution
                                               AIfSubstitution
                                               AOperation
                                               ASelectSubstitution
                                               ASelectWhenSubstitution
                                               PExpression
                                               PPredicate
                                               POperation
                                               ADefinitionFileParseUnit
                                               AParseUnitDefinitionParseUnit
                                               PMachineClause
                                               PSubstitution
                                               PDefinition AExtendsMachineClause AIncludesMachineClause AMachineReference AUsesMachineClause APromotesMachineClause AOpSubstitution ASystemMachineVariant AModelMachineVariant ARefinementMachineParseUnit AImplementationMachineParseUnit ASeesMachineClause ACaseOrSubstitution ACaseSubstitution)))


(declare ir->ast-node)

(defn ir-node-left->ast [ir-node]
  (ir->ast-node (:left ir-node)))
(defn ir-node-right->ast [ir-node]
  (ir->ast-node (:right ir-node)))
(defn ir-node-values->ast [ir-node]
  (map ir->ast-node (:values ir-node)))
(defn ir-node-pred->ast [ir-node]
  (ir->ast-node (:pred ir-node)))
(defn ir-node-preds->ast [ir-node]
  (map ir->ast-node (:preds ir-node)))
(defn ir-node-expr->ast [ir-node]
  (ir->ast-node (:expr ir-node)))
(defn ir-node-set->ast [ir-node]
  (ir->ast-node (:set ir-node)))
(defn ir-node-sets->ast [ir-node]
  (map ir->ast-node (:sets ir-node)))
(defn ir-node-num->ast [ir-node]
  (ir->ast-node (:num ir-node)))
(defn ir-node-nums->ast [ir-node]
  (map ir->ast-node (:nums ir-node)))
(defn ir-node-rel->ast [ir-node]
  (ir->ast-node (:rel ir-node)))
(defn ir-node-rels->ast [ir-node]
  (map ir->ast-node (:rels ir-node)))
(defn ir-node-seq->ast [ir-node]
  (ir->ast-node (:seq ir-node)))
(defn ir-node-seqs->ast [ir-node]
  (map ir->ast-node (:seqs ir-node)))
(defn ir-node-sub->ast [ir-node]
  (ir->ast-node (:sub ir-node)))
(defn ir-node-subs->ast [ir-node]
  (map ir->ast-node (:subs ir-node)))
(defn ir-node-elem->ast [ir-node]
  (ir->ast-node (:elem ir-node)))
(defn ir-node-elems->ast [ir-node]
  (map ir->ast-node (:elems ir-node)))
(defn ir-node-assignment->ast [ir-node]
  (ir->ast-node (:assignment ir-node)))
(defn ir-node-cond->ast [ir-node]
  (ir->ast-node (:cond ir-node)))
(defn ir-node-then->ast [ir-node]
  (ir->ast-node (:then ir-node)))
(defn ir-node-else->ast [ir-node]
  (ir->ast-node (:else ir-node)))
(defn ir-node-id->ast [ir-node]
  (ir->ast-node (:id ir-node)))
(defn ir-node-ids->ast [ir-node]
  (map ir->ast-node (:ids ir-node)))
(defn ir-node-args->ast [ir-node]
  (map ir->ast-node (:args ir-node)))


(defn left-associative [f ir-nodes]
  (reduce f ir-nodes))

(defn right-associative [f ir-nodes]
  (reduce
    (fn [acc value]
      (f value acc))
    (reverse ir-nodes)))

(defn chain [ir-tuples f]
  (let [node-tuples (map #(map ir->ast-node %) ir-tuples)
        nodes (map (partial apply f) node-tuples)]
    (reduce
      #(AConjunctPredicate. %1 %2)
      nodes)))

(defn chain-arity-two [ir-nodes f]
  (chain (partition 2 1 ir-nodes) f))

(defn id-vals->ids [id-vals]
  (map ir->ast-node (map first (partition 2 id-vals))))
(defn id-vals->vals [id-vals]
  (map ir->ast-node (map second (partition 2 id-vals))))
(defn id-types->ids [id-types]
  (id-vals->ids id-types))

(defn id-vals->assignment [id-vals]
  (ir->ast-node (apply band (map #(apply b= %) (partition 2 id-vals)))))
(defn id-types->assignment [id-types]
  (ir->ast-node (apply band (map #(apply bmember? %) (partition 2 id-types)))))


;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ir-node->ast-node (fn [ir-node] (:tag ir-node)))

;;; parse units

(defn abstract-machine-parse-unit [ir-node variant]
  (AAbstractMachineParseUnit.
    variant
    (AMachineHeader. (.getIdentifier (ir->ast-node (:name ir-node))) (map ir->ast-node (:parameters ir-node)))
    (map ir->ast-node (:clauses ir-node))))

(defmethod ir-node->ast-node :machine [ir-node]
  (abstract-machine-parse-unit ir-node (AMachineMachineVariant.)))

(defmethod ir-node->ast-node :model [ir-node]
  (abstract-machine-parse-unit ir-node (AModelMachineVariant.)))

(defmethod ir-node->ast-node :system [ir-node]
  (abstract-machine-parse-unit ir-node (ASystemMachineVariant.)))

(defmethod ir-node->ast-node :refinement [ir-node]
  (ARefinementMachineParseUnit.
    (AMachineHeader. (.getIdentifier (ir->ast-node (:name ir-node))) (map ir->ast-node (:parameters ir-node)))
    (TIdentifierLiteral. (name (:ref-machine ir-node)))
    (map ir->ast-node (:clauses ir-node))))

(defmethod ir-node->ast-node :implementation [ir-node]
  (AImplementationMachineParseUnit.
    (AMachineHeader. (.getIdentifier (ir->ast-node (:name ir-node))) (map ir->ast-node (:parameters ir-node)))
    (TIdentifierLiteral. (name (:ref-machine ir-node)))
    (map ir->ast-node (:clauses ir-node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; machine clauses
;; machine inclusions

(defmethod ir-node->ast-node :uses [ir-node]
  (AUsesMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :includes [ir-node]
  (AIncludesMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :machine-reference [ir-node]
  (AMachineReference. [(TIdentifierLiteral. (name (:name ir-node)))] (map ir->ast-node (:args ir-node))))

(defmethod ir-node->ast-node :sees [ir-node]
  (ASeesMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :extends [ir-node]
  (AExtendsMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :promotes [ir-node]
  (APromotesMachineClause. (ir-node-values->ast ir-node)))

;; machine sections

(defmethod ir-node->ast-node :contraints [ir-node]
  (AConstraintsMachineClause. (reduce #(AConjunctPredicate. %1 %2) (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :sets [ir-node]
  (ASetsMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :deferred-set [ir-node]
  (ADeferredSetSet. (.getIdentifier (ir-node-id->ast ir-node))))
(defmethod ir-node->ast-node :enumerated-set [ir-node]
  (AEnumeratedSetSet. (.getIdentifier (ir-node-id->ast ir-node)) (ir-node-elems->ast ir-node)))

(defmethod ir-node->ast-node :constants [ir-node]
  (AConstantsMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :properties [ir-node]
  (APropertiesMachineClause. (reduce #(AConjunctPredicate. %1 %2) (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :definitions [ir-node]
  (ADefinitionsMachineClause. (ir-node-values->ast ir-node)))
;TODO:
;(defmethod ir-node->ast-node :definition)

(defmethod ir-node->ast-node :variables [ir-node]
  (AVariablesMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :invariants [ir-node]
  (AInvariantMachineClause. (reduce #(AConjunctPredicate. %1 %2) (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :assertions [ir-node]
  (AAssertionsMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :init [ir-node]
  (AInitialisationMachineClause. (ASequenceSubstitution. (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :operations [ir-node]
  (AOperationsMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :operation [ir-node]
  (AOperation. (map ir->ast-node (:return-vals ir-node)) (list (TIdentifierLiteral. (name (:name ir-node)))) (ir-node-args->ast ir-node) (ir->ast-node (:body ir-node))))

;;; substitutions

(defmethod ir-node->ast-node :skip [_]
  (ASkipSubstitution.))

(defmethod ir-node->ast-node :block [ir-node]
  (ABlockSubstitution. (ir-node-sub->ast ir-node)))

(defmethod ir-node->ast-node :assignment [ir-node]
  (let [id-vals (:id-vals ir-node)
        ids (id-vals->ids id-vals)
        vals (id-vals->vals id-vals)]
    (AAssignSubstitution. ids vals)))

(defmethod ir-node->ast-node :becomes-element-of [ir-node] (ABecomesElementOfSubstitution. (ir-node-ids->ast ir-node) (ir-node-set->ast ir-node)))
(defmethod ir-node->ast-node :becomes-such [ir-node] (ABecomesSuchSubstitution. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node)))
(defmethod ir-node->ast-node :op-call [ir-node]
  (AOperationCallSubstitution. (ir-node-ids->ast ir-node) (list (TIdentifierLiteral. (name (:op ir-node)))) (ir-node-args->ast ir-node)))
(defmethod ir-node->ast-node :parallel-sub [ir-node] (AParallelSubstitution. (ir-node-subs->ast ir-node)))
(defmethod ir-node->ast-node :sequential-sub [ir-node] (ASequenceSubstitution. (ir-node-subs->ast ir-node)))
(defmethod ir-node->ast-node :any [ir-node] (AAnySubstitution. (ir-node-ids->ast ir-node) (ir->ast-node (:where ir-node)) (ir-node-then->ast ir-node)))
(defmethod ir-node->ast-node :let-sub [ir-node]
  (let [id-vals (:id-vals ir-node)
        ids (id-vals->ids id-vals)
        assignment (id-vals->assignment id-vals)
        sub (ir-node-sub->ast ir-node)]
    (ALetSubstitution. ids assignment sub)))
(defmethod ir-node->ast-node :var [ir-node]
  (AVarSubstitution. (ir-node-ids->ast ir-node) (ir-node-sub->ast ir-node)))
(defmethod ir-node->ast-node :pre [ir-node] (APreconditionSubstitution. (ir-node-pred->ast ir-node) (ir-node-sub->ast ir-node)))
(defmethod ir-node->ast-node :assert [ir-node] (AAssertionSubstitution. (ir-node-pred->ast ir-node) (ir-node-sub->ast ir-node)))

(defn choice-or-substitution [sub]
  (AChoiceOrSubstitution. sub))
(defmethod ir-node->ast-node :choice [ir-node]
  (let [subs (ir-node-subs->ast ir-node)]
    (AChoiceSubstitution. (conj (map choice-or-substitution (rest subs)) (first subs)))))

(defmethod ir-node->ast-node :if-sub [ir-node]
  (AIfSubstitution. (ir-node-cond->ast ir-node) (ir-node-then->ast ir-node) () (ir-node-else->ast ir-node)))

(defn if-else-sub [[condition then]]
  (AIfElsifSubstitution. condition then))
(defmethod ir-node->ast-node :cond [ir-node]
  (let [clauses (map ir->ast-node (:clauses ir-node))
        condition (first clauses)
        then (second clauses)]
        (if (even? (count clauses))
          (let [else-ifs (map if-else-sub (partition 2 (drop 2 clauses)))]
            (AIfSubstitution. condition then else-ifs nil))
          (let [else (last clauses)
                else-ifs (map if-else-sub (partition 2 (drop-last 1 (drop 2 clauses))))]
            (AIfSubstitution. condition then else-ifs else)))))

(defn select-when-sub [[condition then]]
  (ASelectWhenSubstitution. condition then))
(defmethod ir-node->ast-node :select [ir-node]
  (let [clauses (map ir->ast-node (:clauses ir-node))
        condition (first clauses)
        then (second clauses)]
    (if (even? (count clauses))
      (let [else-ifs (map select-when-sub (partition 2 (drop 2 clauses)))]
        (ASelectSubstitution. condition then else-ifs nil))
      (let [else (last clauses)
            else-ifs (map select-when-sub (partition 2 (drop-last 1 (drop 2 clauses))))]
        (ASelectSubstitution. condition then else-ifs else)))))

(defn to-vec [v]
  (if (vector? v)
    v
    [v]))
(defn case-or-sub [[case then]]
  (ACaseOrSubstitution. (to-vec case) then))
(defn get-or-subs-and-else [cases]
  (if (even? (count cases))
    [(map case-or-sub (partition 2 cases)) nil]
    [(map case-or-sub (partition 2 (drop-last cases))) (last cases)]))
(defmethod ir-node->ast-node :case [ir-node]
  (let [expr (ir-node-expr->ast ir-node)
        clauses (map ir->ast-node (:clauses ir-node))
        either-expr (to-vec (first clauses))
        either-sub (second clauses)
        [or-subs else] (get-or-subs-and-else (drop 2 clauses))]
    (ACaseSubstitution. expr either-expr either-sub or-subs else)))

(defmethod ir-node->ast-node :op-subs [ir-node]
  (AOpSubstitution. (ir->ast-node (:op ir-node)) (map ir->ast-node (:args ir-node))))


;;; if

(defmethod ir-node->ast-node :if-expr [ir-node] (AIfThenElseExpression. (ir-node-cond->ast ir-node) (ir-node-then->ast ir-node) '() (ir-node-else->ast ir-node)))


;;; let

(defmethod ir-node->ast-node :let [ir-node]
  (let [id-vals  (:id-vals ir-node)
        ids (id-vals->ids id-vals)
        assignment (id-vals->assignment id-vals)
        expr-or-pred (ir->ast-node (:expr-or-pred ir-node))]
    (cond
      (instance? PExpression expr-or-pred) (ALetExpressionExpression. ids assignment expr-or-pred)
      (instance? PPredicate expr-or-pred) (ALetPredicatePredicate. ids assignment expr-or-pred)
      :else (throw (str "Unsupported ast node for let-block" expr-or-pred)))))

;;; strings

(defmethod ir-node->ast-node :string-set [_] (AStringSetExpression.))


;;; records

(defmethod ir-node->ast-node :struct [ir-node]
  (AStructExpression. (map
                        (fn [[k v]] (ARecEntry. (ir->ast-node k) (ir->ast-node v)))
                        (partition 2 (:id-types ir-node)))))

(defmethod ir-node->ast-node :record [ir-node]
  (ARecExpression. (map
                     (fn [[k v]] (ARecEntry. (ir->ast-node k) (ir->ast-node v)))
                     (partition 2 (:id-vals ir-node)))))

(defmethod ir-node->ast-node :record-get [ir-node]
  (ARecordFieldExpression. (ir->ast-node (:rec ir-node)) (ir-node-id->ast ir-node)))


;;; sequences

(defmethod ir-node->ast-node :empty-sequence [_]
  (AEmptySequenceExpression.))

(defmethod ir-node->ast-node :sequence [ir-node]
  (ASequenceExtensionExpression. (ir-node-elems->ast ir-node)))

(defmethod ir-node->ast-node :seq [ir-node]
  (ASeqExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :seq1 [ir-node]
  (ASeq1Expression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :iseq [ir-node]
  (AIseqExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :iseq1 [ir-node]
  (AIseq1Expression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :perm [ir-node]
  (APermExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :size [ir-node]
  (ASizeExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :concat [ir-node]
  (left-associative #(AConcatExpression. %1 %2) (ir-node-seqs->ast ir-node)))

(defmethod ir-node->ast-node :prepend [ir-node]
  (AInsertFrontExpression. (ir-node-elem->ast ir-node) (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :append [ir-node]
  (left-associative #(AInsertTailExpression. %1 %2) (conj (ir-node-elems->ast ir-node) (ir-node-seq->ast ir-node))))

(defmethod ir-node->ast-node :reverse [ir-node]
  (ARevExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :first [ir-node]
  (AFirstExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :last [ir-node]
  (ALastExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :front [ir-node]
  (AFrontExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :tail [ir-node]
  (ATailExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :conc [ir-node]
  (AGeneralConcatExpression. (ir->ast-node (:seq-of-seqs ir-node))))

(defmethod ir-node->ast-node :take [ir-node]
  (ARestrictFrontExpression. (ir-node-seq->ast ir-node) (ir-node-num->ast ir-node)))

(defmethod ir-node->ast-node :drop [ir-node]
  (ARestrictTailExpression. (ir-node-seq->ast ir-node) (ir-node-num->ast ir-node)))


;;; functions

(defmethod ir-node->ast-node :partial-fn [ir-node]
  (left-associative #(APartialFunctionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-fn [ir-node]
  (left-associative #(ATotalFunctionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :partial-surjection [ir-node]
  (left-associative #(APartialSurjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-surjection [ir-node]
  (left-associative #(ATotalSurjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :partial-injection [ir-node]
  (left-associative #(APartialInjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-injection [ir-node]
  (left-associative #(ATotalInjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :partial-bijection [ir-node]
  (left-associative #(APartialBijectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-bijection [ir-node]
  (left-associative #(ATotalBijectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :lambda [ir-node]
  (ALambdaExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :fn-call [ir-node]
  (AFunctionExpression. (ir->ast-node (:f ir-node)) (map ir->ast-node (:args ir-node))))


;;; relations

(defmethod ir-node->ast-node :relation [ir-node]
  (left-associative #(ARelationsExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-relation [ir-node]
  (left-associative #(ATotalRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :surjective-realtion [ir-node]
  (left-associative #(ASurjectionRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-surjective-relation [ir-node]
  (left-associative #(ATotalSurjectionRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :maplet [ir-node]
  (ACoupleExpression. [(ir-node-left->ast ir-node) (ir-node-right->ast ir-node)]))

(defmethod ir-node->ast-node :dom [ir-node]
  (ADomainExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :ran [ir-node]
  (ARangeExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :id [ir-node]
  (AIdentityExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :domain-restriction [ir-node]
  (ADomainRestrictionExpression. (ir-node-set->ast ir-node) (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :domain-subtraction [ir-node]
  (ADomainSubtractionExpression. (ir-node-set->ast ir-node) (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :range-restriction [ir-node]
  (ARangeRestrictionExpression. (ir-node-rel->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :range-subtraction [ir-node]
  (ARangeSubtractionExpression. (ir-node-rel->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :inverse [ir-node]
  (AReverseExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :image [ir-node]
  (AImageExpression. (ir-node-rel->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :override [ir-node]
  (left-associative #(AOverwriteExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :direct-product [ir-node]
  (left-associative #(ADirectProductExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :composition [ir-node]
  (left-associative #(ACompositionExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :parallel-product [ir-node]
  (left-associative #(AParallelProductExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :prj1 [ir-node]
  (AFirstProjectionExpression. (ir->ast-node (:set1 ir-node)) (ir->ast-node (:set2 ir-node))))

(defmethod ir-node->ast-node :prj2 [ir-node]
  (ASecondProjectionExpression. (ir->ast-node (:set1 ir-node)) (ir->ast-node (:set2 ir-node))))

(defmethod ir-node->ast-node :closure1 [ir-node]
  (AClosureExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :closure [ir-node]
  (AReflexiveClosureExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :iterate [ir-node]
  (AIterationExpression. (ir-node-rel->ast ir-node) (ir-node-num->ast ir-node)))

(defmethod ir-node->ast-node :fnc [ir-node]
  (ATransFunctionExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :rel [ir-node]
  (ATransRelationExpression. (ir-node-rel->ast ir-node)))


;;; numbers

(defmethod ir-node->ast-node :unary-minus [ir-node]
  (AUnaryMinusExpression. (ir-node-num->ast ir-node)))

(defmethod ir-node->ast-node :integer-set [_]
  (AIntegerSetExpression.))

(defmethod ir-node->ast-node :natural-set [_]
  (ANaturalSetExpression.))

(defmethod ir-node->ast-node :natural1-set [_]
  (ANatural1SetExpression.))

(defmethod ir-node->ast-node :int-set [_]
  (AIntSetExpression.))

(defmethod ir-node->ast-node :nat-set [_]
  (ANatSetExpression.))

(defmethod ir-node->ast-node :nat1-set [_]
  (ANat1SetExpression.))

(defmethod ir-node->ast-node :interval [ir-node]
  (AIntervalExpression. (ir->ast-node (:from ir-node)) (ir->ast-node (:to ir-node))))

(defmethod ir-node->ast-node :min-int [_]
  (AMinIntExpression.))

(defmethod ir-node->ast-node :max-int [_]
  (AMaxIntExpression.))

(defmethod ir-node->ast-node :greater [ir-node]
  (chain-arity-two (:nums ir-node) #(AGreaterPredicate. %1 %2)))

(defmethod ir-node->ast-node :less [ir-node]
  (chain-arity-two (:nums ir-node) #(ALessPredicate. %1 %2)))

(defmethod ir-node->ast-node :greater-equals [ir-node]
  (chain-arity-two (:nums ir-node) #(AGreaterEqualPredicate. %1 %2)))

(defmethod ir-node->ast-node :less-equals [ir-node]
  (chain-arity-two (:nums ir-node) #(ALessEqualPredicate. %1 %2)))

(defmethod ir-node->ast-node :max [ir-node]
  (AMaxExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :min [ir-node]
  (AMinExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :add [ir-node]
  (left-associative #(AAddExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :sub [ir-node]
  (left-associative #(AMinusOrSetSubtractExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :mul-or-cart [ir-node]
  (left-associative #(AMultOrCartExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :mul [ir-node]
  (left-associative #(AMultOrCartExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :div [ir-node]
  (left-associative #(ADivExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :pow [ir-node]
  (right-associative #(APowerOfExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :mod [ir-node]
  (left-associative #(AModuloExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :pi [ir-node]
  (AGeneralProductExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :sigma [ir-node]
  (AGeneralSumExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :successor [ir-node]
  (AFunctionExpression. (ASuccessorExpression.) (list (ir-node-num->ast ir-node))))

(defmethod ir-node->ast-node :predecessor [ir-node]
  (AFunctionExpression. (APredecessorExpression.) (list (ir-node-num->ast ir-node))))


;;; sets

(defmethod ir-node->ast-node :comprehension-set [ir-node]
  (AComprehensionSetExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node)))

(defmethod ir-node->ast-node :power-set [ir-node]
  (APowSubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :power1-set [ir-node]
  (APow1SubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :fin [ir-node]
  (AFinSubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :fin1 [ir-node]
  (AFin1SubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :cardinality [ir-node]
  (ACardExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :cartesian-product [ir-node]
  (left-associative #(AMultOrCartExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :union [ir-node]
  (left-associative #(AUnionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :intersection [ir-node]
  (left-associative #(AIntersectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :difference [ir-node]
  (left-associative #(ASetSubtractionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :member [ir-node]
  (AMemberPredicate. (ir-node-elem->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :subset [ir-node]
  (chain-arity-two (:sets ir-node) #(ASubsetPredicate. %1 %2)))

(defmethod ir-node->ast-node :strict-subset [ir-node]
  (chain-arity-two (:sets ir-node) #(ASubsetStrictPredicate. %1 %2)))

(defmethod ir-node->ast-node :general-union [ir-node]
  (AGeneralUnionExpression. (ir->ast-node (:set-of-sets ir-node))))

(defmethod ir-node->ast-node :general-intersection [ir-node]
  (AGeneralIntersectionExpression. (ir->ast-node (:set-of-sets ir-node))))

(defmethod ir-node->ast-node :union-pe [ir-node]
  (AQuantifiedUnionExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :intersection-pe [ir-node]
  (AQuantifiedIntersectionExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))


;;; booleans

(defmethod ir-node->ast-node :bool-set [_]
  (ABoolSetExpression.))

(defmethod ir-node->ast-node :pred->bool [ir-node]
  (AConvertBoolExpression. (ir-node-pred->ast ir-node)))


;;; equal predicates

(defmethod ir-node->ast-node := [ir-node]
  (AEqualPredicate. (ir-node-left->ast ir-node) (ir-node-right->ast ir-node)))

(defmethod ir-node->ast-node :not= [ir-node]
  (ANotEqualPredicate. (ir-node-left->ast ir-node) (ir-node-right->ast ir-node)))


;;; logical predicates

(defmethod ir-node->ast-node :and [ir-node]
  (left-associative #(AConjunctPredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :or [ir-node]
  (left-associative #(ADisjunctPredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :implication [ir-node]
  (left-associative #(AImplicationPredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :equivalence [ir-node]
  (left-associative #(AEquivalencePredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :not [ir-node]
  (let [predicate (ir-node-pred->ast ir-node)
        pt (type predicate)]
    ; simplify
    (cond
      (= AMemberPredicate pt) (ANotMemberPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetPredicate pt) (ANotSubsetPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetStrictPredicate pt) (ANotSubsetStrictPredicate. (.getLeft predicate) (.getRight predicate))
      :else (ANegationPredicate. predicate))))

(defmethod ir-node->ast-node :for-all [ir-node]
  (AForallPredicate. (ir-node-ids->ast ir-node) (ir-node->ast-node (:implication ir-node))))

(defmethod ir-node->ast-node :exists [ir-node]
  (AExistsPredicate. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node)))


;;; misc

#_(defmethod ir-node->ast-node :call [ir-node]
  (println ir-node)
  (ADefinitionExpression. (TIdentifierLiteral. (name (:f ir-node))) (map ir->ast-node (:args ir-node))))


;;;;;;;;;;;;;;

(defn set-expression [elements]
  (if (empty? elements)
    (AEmptySetExpression.)
    (ASetExtensionExpression. elements)))

(defn ir->ast-node [ir]
  (cond
    (map? ir) (ir-node->ast-node ir)
    (set? ir) (set-expression (map ir->ast-node ir))
    (vector? ir) (ACoupleExpression. (map ir->ast-node ir))
    (keyword? ir) (AIdentifierExpression. [(TIdentifierLiteral. (name ir))])
    (string? ir) (AStringExpression. (TStringLiteral. ir)) ;; hack-y thing to avoid renaming of rec-get parameters in preds
    (number? ir) (AIntegerExpression. (TIntegerLiteral. (str ir)))
    (true? ir) (ABooleanTrueExpression.)
    (false? ir) (ABooleanFalseExpression.)
    (nil? ir) nil
    :otherwise (println :unhandled-literal ir)))

(defn ir->ast [ir]
  (let [top-ast-node (ir->ast-node ir)]
    (Start.
      (cond
        (instance? AAbstractMachineParseUnit top-ast-node) top-ast-node
        (instance? ARefinementMachineParseUnit top-ast-node) top-ast-node
        (instance? AImplementationMachineParseUnit top-ast-node) top-ast-node
        ;(instance?  top-ast-node) (ADefinitionFileParseUnit. top-ast-node)
        (instance? PPredicate top-ast-node) (APredicateParseUnit. top-ast-node)
        (instance? PExpression top-ast-node) (AExpressionParseUnit. top-ast-node)
        (instance? PSubstitution top-ast-node) (ASubstitutionParseUnit. top-ast-node)
        (instance? PDefinition top-ast-node) (AParseUnitDefinitionParseUnit. top-ast-node)
        (instance? PMachineClause top-ast-node) (AMachineClauseParseUnit. top-ast-node)
        :else (throw (str "Unsupported top level ast node" top-ast-node)))
      (EOF.))))
