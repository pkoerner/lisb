(ns lisb.translation.ir2ast
  (:require [clojure.math.combinatorics :refer [combinations]])
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
                                               ASelectWhenSubstitution)))

(declare ir->ast)

(defn ir-node-left->ast [ir-node]
  (ir->ast (:left ir-node)))
(defn ir-node-right->ast [ir-node]
  (ir->ast (:right ir-node)))
(defn ir-node-predicate->ast [ir-node]
  (ir->ast (:predicate ir-node)))
(defn ir-node-predicates->ast [ir-node]
  (map ir->ast (:predicates ir-node)))
(defn ir-node-expression->ast [ir-node]
  (ir->ast (:expression ir-node)))
(defn ir-node-set->ast [ir-node]
  (ir->ast (:set ir-node)))
(defn ir-node-sets->ast [ir-node]
  (map ir->ast (:sets ir-node)))
(defn ir-node-number->ast [ir-node]
  (ir->ast (:number ir-node)))
(defn ir-node-numbers->ast [ir-node]
  (map ir->ast (:numbers ir-node)))
(defn ir-node-relation->ast [ir-node]
  (ir->ast (:relation ir-node)))
(defn ir-node-relations->ast [ir-node]
  (map ir->ast (:relations ir-node)))
(defn ir-node-seq->ast [ir-node]
  (ir->ast (:seq ir-node)))
(defn ir-node-seqs->ast [ir-node]
  (map ir->ast (:seqs ir-node)))
(defn ir-node-substitution->ast [ir-node]
  (ir->ast (:substitution ir-node)))
(defn ir-node-substitutions->ast [ir-node]
  (map ir->ast (:substitutions ir-node)))
(defn ir-node-element->ast [ir-node]
  (ir->ast (:element ir-node)))
(defn ir-node-elements->ast [ir-node]
  (map ir->ast (:elements ir-node)))
(defn ir-node-assignment->ast [ir-node]
  (ir->ast (:assignment ir-node)))
(defn ir-node-condition->ast [ir-node]
  (ir->ast (:condition ir-node)))
(defn ir-node-then->ast [ir-node]
  (ir->ast (:then ir-node)))
(defn ir-node-else->ast [ir-node]
  (ir->ast (:else ir-node)))
(defn ir-node-identifier->ast [ir-node]
  (ir->ast (:identifier ir-node)))
(defn ir-node-identifiers->ast [ir-node]
  (map ir->ast (:identifiers ir-node)))
(defn ir-node-parameters->ast [ir-node]
  (map ir->ast (:parameters ir-node)))


(defn left-associative [f ir-nodes]
  (reduce f ir-nodes))

(defn right-associative [f ir-nodes]
  (reduce
    (fn [acc value]
      (f value acc))
    (reverse ir-nodes)))

(defn chain [ir-tuples f]
  (let [node-tuples (map #(map ir->ast %) ir-tuples)
        nodes (map (partial apply f) node-tuples)]
    (reduce
      #(AConjunctPredicate. %1 %2)
      nodes)))

(defn chain-arity-two [ir-nodes f]
  (chain (partition 2 1 ir-nodes) f))

(defn chain-combinitions-arity-two [ir-nodes f]
  (chain (combinations ir-nodes 2) f))


;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ir-node->ast (fn [ir-node] (:tag ir-node)))

;;; parse units

(defmethod ir-node->ast :machine [ir-node]
  (Start. (AAbstractMachineParseUnit. (ir->ast (:variant ir-node)) (ir->ast (:header ir-node)) (map ir->ast (:clauses ir-node))) (EOF.)))
(defmethod ir-node->ast :machine-variant [_]
  (AMachineMachineVariant.))
(defmethod ir-node->ast :machine-header [ir-node]
  (AMachineHeader. (.getIdentifier (ir->ast (:name ir-node))) (ir-node-parameters->ast ir-node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; machine clauses

(defmethod ir-node->ast :contraints [ir-node]
  (AConstraintsMachineClause. (ir-node-predicate->ast ir-node)))

(defmethod ir-node->ast :sets [ir-node]
  (ASetsMachineClause. (map ir->ast (:set-definitions ir-node))))
(defmethod ir-node->ast :deferred-set [ir-node]
  (ADeferredSetSet. (.getIdentifier (ir-node-identifier->ast ir-node))))
(defmethod ir-node->ast :enumerated-set [ir-node]
  (AEnumeratedSetSet. (.getIdentifier (ir-node-identifier->ast ir-node)) (ir-node-elements->ast ir-node)))

(defmethod ir-node->ast :constants [ir-node]
  (AConstantsMachineClause. (ir-node-identifiers->ast ir-node)))

(defmethod ir-node->ast :properties [ir-node]
  (APropertiesMachineClause. (ir-node-predicate->ast ir-node)))

(defmethod ir-node->ast :definitions [ir-node]
  (ADefinitionsMachineClause. (ir->ast (:definitions ir-node))))
;TODO:
;(defmethod ir-node->ast :definition)

(defmethod ir-node->ast :variables [ir-node]
  (AVariablesMachineClause. (ir-node-identifiers->ast ir-node)))

(defmethod ir-node->ast :invariants [ir-node]
  (AInvariantMachineClause. (ir-node-predicate->ast ir-node)))

(defmethod ir-node->ast :assertions [ir-node]
  (AAssertionsMachineClause. (ir-node-predicates->ast ir-node)))

(defmethod ir-node->ast :assign [ir-node]
  (AAssertionsMachineClause. (ir-node-predicates->ast ir-node)))

(defmethod ir-node->ast :init [ir-node]
  (AInitialisationMachineClause. (ir-node-substitution->ast ir-node)))

(defmethod ir-node->ast :operations [ir-node]
  (AOperationsMachineClause. (map ir->ast (:operations ir-node))))
(defmethod ir-node->ast :operation [ir-node]
  (AOperation. (map ir->ast (:return ir-node)) (list (TIdentifierLiteral. (name (:name ir-node)))) (ir-node-parameters->ast ir-node) (ir->ast (:body ir-node))))

;;; substitutions

(defmethod ir-node->ast :skip [_]
  (ASkipSubstitution.))

(defmethod ir-node->ast :block [ir-node]
  (ABlockSubstitution. (ir-node-substitution->ast ir-node)))

(defmethod ir-node->ast :assign [ir-node]
  (AAssignSubstitution. (ir-node-identifiers->ast ir-node) (map ir->ast (:values ir-node))))

(defmethod ir-node->ast :becomes-element-of [ir-node] (ABecomesElementOfSubstitution. (ir-node-identifiers->ast ir-node) (ir-node-set->ast ir-node)))
(defmethod ir-node->ast :becomes-such [ir-node] (ABecomesSuchSubstitution. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node)))
(defmethod ir-node->ast :operation-call [ir-node]
  (AOperationCallSubstitution. (ir-node-identifiers->ast ir-node) (list (TIdentifierLiteral. (name (:operation ir-node)))) (ir-node-parameters->ast ir-node)))
(defmethod ir-node->ast :parallel-substitution [ir-node] (AParallelSubstitution. (ir-node-substitutions->ast ir-node)))
(defmethod ir-node->ast :sequence-substitution [ir-node] (ASequenceSubstitution. (ir-node-substitutions->ast ir-node)))
(defmethod ir-node->ast :any [ir-node] (AAnySubstitution. (ir-node-identifiers->ast ir-node) (ir->ast (:where ir-node)) (ir-node-then->ast ir-node)))
(defmethod ir-node->ast :let-sub [ir-node] (ALetSubstitution. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node) (ir-node-substitution->ast ir-node)))
(defmethod ir-node->ast :var [ir-node]
  (AVarSubstitution. (ir-node-identifiers->ast ir-node) (ir-node-substitution->ast ir-node)))
(defmethod ir-node->ast :precondition [ir-node] (APreconditionSubstitution. (ir-node-predicate->ast ir-node) (ir-node-substitution->ast ir-node)))
(defmethod ir-node->ast :assert [ir-node] (AAssertionSubstitution. (ir-node-predicate->ast ir-node) (ir-node-substitution->ast ir-node)))

(defn choice-or-substitution [sub]
  (AChoiceOrSubstitution. sub))
(defmethod ir-node->ast :choice [ir-node]
  (let [subs (ir-node-substitutions->ast ir-node)]
    (AChoiceSubstitution. (conj (map choice-or-substitution (rest subs)) (first subs)))))

(defmethod ir-node->ast :if-sub [ir-node]
  (AIfSubstitution. (ir-node-condition->ast ir-node) (ir-node-then->ast ir-node) () (ir-node-else->ast ir-node)))

(defn if-else-sub [[condition then]]
  (AIfElsifSubstitution. condition then))
(defmethod ir-node->ast :cond [ir-node]
  (let [clauses (map ir->ast (:clauses ir-node))
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
(defmethod ir-node->ast :select [ir-node]
  (let [clauses (map ir->ast (:clauses ir-node))
        condition (first clauses)
        then (second clauses)]
    (if (even? (count clauses))
      (let [else-ifs (map select-when-sub (partition 2 (drop 2 clauses)))]
        (ASelectSubstitution. condition then else-ifs nil))
      (let [else (last clauses)
            else-ifs (map select-when-sub (partition 2 (drop-last 1 (drop 2 clauses))))]
        (ASelectSubstitution. condition then else-ifs else)))))


;;; if

(defmethod ir-node->ast :if-expr [ir-node] (AIfThenElseExpression. (ir-node-condition->ast ir-node) (ir-node-then->ast ir-node) '() (ir-node-else->ast ir-node)))


;;; let

(defmethod ir-node->ast :let-expr [ir-node]
  (ALetExpressionExpression. (ir-node-identifiers->ast ir-node) (ir-node-assignment->ast ir-node) (ir-node-expression->ast ir-node)))

(defmethod ir-node->ast :let-pred [ir-node]
  (ALetPredicatePredicate. (ir-node-identifiers->ast ir-node) (ir-node-assignment->ast ir-node) (ir-node-predicate->ast ir-node)))


;;; strings

(defmethod ir-node->ast :string-set [_] (AStringSetExpression.))


;;; records

(defmethod ir-node->ast :struct [ir-node]
  (AStructExpression. (map
                        (fn [[k v]] (ARecEntry. (ir->ast k) (ir->ast v)))
                        (partition 2 (:id-types ir-node)))))

(defmethod ir-node->ast :record [ir-node]
  (ARecExpression. (map
                     (fn [[k v]] (ARecEntry. (ir->ast k) (ir->ast v)))
                     (partition 2 (:id-values ir-node)))))

(defmethod ir-node->ast :rec-get [ir-node]
  (ARecordFieldExpression. (ir->ast (:record ir-node)) (ir-node-identifier->ast ir-node)))


;;; sequences

(defmethod ir-node->ast :empty-sequence [_]
  (AEmptySequenceExpression.))

(defmethod ir-node->ast :sequence [ir-node]
  (ASequenceExtensionExpression. (ir-node-elements->ast ir-node)))

(defmethod ir-node->ast :seq [ir-node]
  (ASeqExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :seq1 [ir-node]
  (ASeq1Expression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :iseq [ir-node]
  (AIseqExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :iseq1 [ir-node]
  (AIseq1Expression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :perm [ir-node]
  (APermExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :size [ir-node]
  (ASizeExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :concat [ir-node]
  (left-associative #(AConcatExpression. %1 %2) (ir-node-seqs->ast ir-node)))

(defmethod ir-node->ast :insert-front [ir-node]
  (right-associative #(AInsertFrontExpression. %1 %2) (reverse (conj (ir-node-elements->ast ir-node) (ir-node-seq->ast ir-node)))))

(defmethod ir-node->ast :insert-tail [ir-node]
  (left-associative #(AInsertTailExpression. %1 %2) (conj (ir-node-elements->ast ir-node) (ir-node-seq->ast ir-node))))

(defmethod ir-node->ast :reverse [ir-node]
  (ARevExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast :first [ir-node]
  (AFirstExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast :last [ir-node]
  (ALastExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast :front [ir-node]
  (AFrontExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast :tail [ir-node]
  (ATailExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast :conc [ir-node]
  (AGeneralConcatExpression. (ir->ast (:seq-of-seqs ir-node))))

(defmethod ir-node->ast :restrict-front [ir-node]
  (ARestrictFrontExpression. (ir-node-seq->ast ir-node) (ir-node-number->ast ir-node)))

(defmethod ir-node->ast :restrict-tail [ir-node]
  (ARestrictTailExpression. (ir-node-seq->ast ir-node) (ir-node-number->ast ir-node)))


;;; functions

(defmethod ir-node->ast :partial-fn [ir-node]
  (left-associative #(APartialFunctionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :total-fn [ir-node]
  (left-associative #(ATotalFunctionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :partial-surjection [ir-node]
  (left-associative #(APartialSurjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :total-surjection [ir-node]
  (left-associative #(ATotalSurjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :partial-injection [ir-node]
  (left-associative #(APartialInjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :total-injection [ir-node]
  (left-associative #(ATotalInjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :partial-bijection [ir-node]
  (left-associative #(APartialBijectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :total-bijection [ir-node]
  (left-associative #(ATotalBijectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :lambda [ir-node]
  (ALambdaExpression. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node) (ir-node-expression->ast ir-node)))

(defmethod ir-node->ast :apply [ir-node]
  (AFunctionExpression. (ir->ast (:f ir-node)) (map ir->ast (:args ir-node))))


;;; relations

(defmethod ir-node->ast :relation [ir-node]
  (left-associative #(ARelationsExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :total-relation [ir-node]
  (left-associative #(ATotalRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :surjective-relation [ir-node]
  (left-associative #(ASurjectionRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :total-surjective-relation [ir-node]
  (left-associative #(ATotalSurjectionRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :couple [ir-node]
  (ACoupleExpression. (ir-node-elements->ast ir-node)))

(defmethod ir-node->ast :domain [ir-node]
  (ADomainExpression. (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :range [ir-node]
  (ARangeExpression. (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :identity-relation [ir-node]
  (AIdentityExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :domain-restriction [ir-node]
  (ADomainRestrictionExpression. (ir-node-set->ast ir-node) (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :domain-subtraction [ir-node]
  (ADomainSubtractionExpression. (ir-node-set->ast ir-node) (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :range-restriction [ir-node]
  (ARangeRestrictionExpression. (ir-node-relation->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :range-subtraction [ir-node]
  (ARangeSubtractionExpression. (ir-node-relation->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :inverse-relation [ir-node]
  (AReverseExpression. (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :relational-image [ir-node]
  (AImageExpression. (ir-node-relation->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :relational-override [ir-node]
  (left-associative #(AOverwriteExpression. %1 %2) (ir-node-relations->ast ir-node)))

(defmethod ir-node->ast :direct-product [ir-node]
  (left-associative #(ADirectProductExpression. %1 %2) (ir-node-relations->ast ir-node)))

(defmethod ir-node->ast :relational-composition [ir-node]
  (left-associative #(ACompositionExpression. %1 %2) (ir-node-relations->ast ir-node)))

(defmethod ir-node->ast :parallel-product [ir-node]
  (left-associative #(AParallelProductExpression. %1 %2) (ir-node-relations->ast ir-node)))

(defmethod ir-node->ast :prj1 [ir-node]
  (AFirstProjectionExpression. (ir->ast (:set1 ir-node)) (ir->ast (:set2 ir-node))))

(defmethod ir-node->ast :prj2 [ir-node]
  (ASecondProjectionExpression. (ir->ast (:set1 ir-node)) (ir->ast (:set2 ir-node))))

(defmethod ir-node->ast :closure1 [ir-node]
  (AClosureExpression. (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :closure [ir-node]
  (AReflexiveClosureExpression. (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :iterate [ir-node]
  (AIterationExpression. (ir-node-relation->ast ir-node) (ir-node-number->ast ir-node)))

(defmethod ir-node->ast :functionise [ir-node]
  (ATransFunctionExpression. (ir-node-relation->ast ir-node)))

(defmethod ir-node->ast :relationise [ir-node]
  (ATransRelationExpression. (ir-node-relation->ast ir-node)))


;;; numbers

(defmethod ir-node->ast :unary-minus [ir-node]
  (AUnaryMinusExpression. (ir-node-number->ast ir-node)))

(defmethod ir-node->ast :integer-set [_]
  (AIntegerSetExpression.))

(defmethod ir-node->ast :natural-set [_]
  (ANaturalSetExpression.))

(defmethod ir-node->ast :natural1-set [_]
  (ANatural1SetExpression.))

(defmethod ir-node->ast :int-set [_]
  (AIntSetExpression.))

(defmethod ir-node->ast :nat-set [_]
  (ANatSetExpression.))

(defmethod ir-node->ast :nat1-set [_]
  (ANat1SetExpression.))

(defmethod ir-node->ast :interval [ir-node]
  (AIntervalExpression. (ir->ast (:from ir-node)) (ir->ast (:to ir-node))))

(defmethod ir-node->ast :min-int [_]
  (AMinIntExpression.))

(defmethod ir-node->ast :max-int [_]
  (AMaxIntExpression.))

(defmethod ir-node->ast :less [ir-node]
  (chain-arity-two (:numbers ir-node) #(ALessPredicate. %1 %2)))

(defmethod ir-node->ast :greater [ir-node]
  (chain-arity-two (:numbers ir-node) #(AGreaterPredicate. %1 %2)))

(defmethod ir-node->ast :less-eq [ir-node]
  (chain-arity-two (:numbers ir-node) #(ALessEqualPredicate. %1 %2)))

(defmethod ir-node->ast :greater-eq [ir-node]
  (chain-arity-two (:numbers ir-node) #(AGreaterEqualPredicate. %1 %2)))

(defmethod ir-node->ast :max [ir-node]
  (AMaxExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :min [ir-node]
  (AMinExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :plus [ir-node]
  (left-associative #(AAddExpression. %1 %2) (ir-node-numbers->ast ir-node)))

(defmethod ir-node->ast :minus [ir-node]
  (left-associative #(AMinusOrSetSubtractExpression. %1 %2) (ir-node-numbers->ast ir-node)))

(defmethod ir-node->ast :mult-or-cart [ir-node]
  (left-associative #(AMultOrCartExpression. %1 %2) (ir-node-numbers->ast ir-node)))

(defmethod ir-node->ast :div [ir-node]
  (left-associative #(ADivExpression. %1 %2) (ir-node-numbers->ast ir-node)))

(defmethod ir-node->ast :pow [ir-node]
  (right-associative #(APowerOfExpression. %1 %2) (ir-node-numbers->ast ir-node)))

(defmethod ir-node->ast :mod [ir-node]
  (left-associative #(AModuloExpression. %1 %2) (ir-node-numbers->ast ir-node)))

(defmethod ir-node->ast :pi [ir-node]
  (AGeneralProductExpression. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node) (ir-node-expression->ast ir-node)))

(defmethod ir-node->ast :sigma [ir-node]
  (AGeneralSumExpression. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node) (ir-node-expression->ast ir-node)))

(defmethod ir-node->ast :inc [ir-node]
  (AFunctionExpression. (ASuccessorExpression.) (list (ir-node-number->ast ir-node))))

(defmethod ir-node->ast :dec [ir-node]
  (AFunctionExpression. (APredecessorExpression.) (list (ir-node-number->ast ir-node))))


;;; sets

(defmethod ir-node->ast :comp-set [ir-node]
  (AComprehensionSetExpression. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node)))

(defmethod ir-node->ast :power-set [ir-node]
  (APowSubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :power1-set [ir-node]
  (APow1SubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :fin [ir-node]
  (AFinSubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :fin1 [ir-node]
  (AFin1SubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :card [ir-node]
  (ACardExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :union [ir-node]
  (left-associative #(AUnionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :intersection [ir-node]
  (left-associative #(AIntersectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :difference [ir-node]
  (left-associative #(ASetSubtractionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast :member [ir-node]
  (AMemberPredicate. (ir-node-element->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :subset [ir-node]
  (ASubsetPredicate. (ir->ast (:subset ir-node)) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :subset-strict [ir-node]
  (ASubsetStrictPredicate. (ir->ast (:subset-strict ir-node)) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast :general-union [ir-node]
  (AGeneralUnionExpression. (ir->ast (:set-of-sets ir-node))))

(defmethod ir-node->ast :general-intersection [ir-node]
  (AGeneralIntersectionExpression. (ir->ast (:set-of-sets ir-node))))

(defmethod ir-node->ast :union-pe [ir-node]
  (AQuantifiedUnionExpression. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node) (ir-node-expression->ast ir-node)))

(defmethod ir-node->ast :intersection-pe [ir-node]
  (AQuantifiedIntersectionExpression. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node) (ir-node-expression->ast ir-node)))


;;; booleans

(defmethod ir-node->ast :bool-set [_]
  (ABoolSetExpression.))

(defmethod ir-node->ast :pred->bool [ir-node]
  (AConvertBoolExpression. (ir-node-predicate->ast ir-node)))


;;; equal predicates

(defmethod ir-node->ast :equal [ir-node]
  (AEqualPredicate. (ir-node-left->ast ir-node) (ir-node-right->ast ir-node)))

(defmethod ir-node->ast :not-equal [ir-node]
  (ANotEqualPredicate. (ir-node-left->ast ir-node) (ir-node-right->ast ir-node)))

(defmethod ir-node->ast :distinct [ir-node]
  (chain-combinitions-arity-two (:elements ir-node) #(ANotEqualPredicate. %1 %2)))


;;; logical predicates

(defmethod ir-node->ast :and [ir-node]
  (left-associative #(AConjunctPredicate. %1 %2) (ir-node-predicates->ast ir-node)))

(defmethod ir-node->ast :or [ir-node]
  (left-associative #(ADisjunctPredicate. %1 %2) (ir-node-predicates->ast ir-node)))

(defmethod ir-node->ast :implication [ir-node]
  (left-associative #(AImplicationPredicate. %1 %2) (ir-node-predicates->ast ir-node)))

(defmethod ir-node->ast :equivalence [ir-node]
  (left-associative #(AEquivalencePredicate. %1 %2) (ir-node-predicates->ast ir-node)))

(defmethod ir-node->ast :not [ir-node]
  (let [predicate (ir-node-predicate->ast ir-node)
        pt (type predicate)]
    ; simplify
    (cond
      (= AMemberPredicate pt) (ANotMemberPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetPredicate pt) (ANotSubsetPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetStrictPredicate pt) (ANotSubsetStrictPredicate. (.getLeft predicate) (.getRight predicate))
      :else (ANegationPredicate. predicate))))

(defmethod ir-node->ast :for-all [ir-node]
  (AForallPredicate. (ir-node-identifiers->ast ir-node) (ir->ast (:implication ir-node))))

(defmethod ir-node->ast :exists [ir-node]
  (AExistsPredicate. (ir-node-identifiers->ast ir-node) (ir-node-predicate->ast ir-node)))


;;; misc

(defmethod ir-node->ast :call [ir-node]
  (ADefinitionExpression. (TIdentifierLiteral. (name (:f ir-node))) (map ir->ast (:args ir-node))))


;;;;;;;;;;;;;;

(defn set-expression [elements]
  (if (empty? elements)
    (AEmptySetExpression.)
    (ASetExtensionExpression. elements)))

(defn ir->ast [ir]
  (cond
    (map? ir) (ir-node->ast ir)
    (set? ir) (set-expression (map ir->ast ir))
    (vector? ir) (ACoupleExpression. (map ir->ast ir))
    (keyword? ir) (AIdentifierExpression. [(TIdentifierLiteral. (name ir))])
    (string? ir) (AStringExpression. (TStringLiteral. ir)) ;; hack-y thing to avoid renaming of rec-get parameters in preds
    (number? ir) (AIntegerExpression. (TIntegerLiteral. (str ir)))
    (true? ir) (ABooleanTrueExpression.)
    (false? ir) (ABooleanFalseExpression.)
    (nil? ir) nil
    :otherwise (println :unhandled-literal ir)))

(defn ir->predicate-ast [ir]
  (Start. (APredicateParseUnit. (ir->ast ir)) (EOF.)))

(defn ir->expression-ast [ir]
  (Start. (AExpressionParseUnit. (ir->ast ir)) (EOF.)))

(defn ir->substitution-ast [ir]
  (Start. (ASubstitutionParseUnit. (ir->ast ir)) (EOF.)))

(defn ir->machine-clause-ast [ir]
  (Start. (AMachineClauseParseUnit. (ir->ast ir)) (EOF.)))
