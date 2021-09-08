(ns lisb.translation
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:require [clojure.walk :refer [walk]])
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
                                               ALetPredicatePredicate AConstantsMachineClause APropertiesMachineClause AConstraintsMachineClause ASetsMachineClause ADeferredSetSet AEnumeratedSetSet ADefinitionsMachineClause AAssertionsMachineClause AOperationsMachineClause ASkipSubstitution ABecomesElementOfSubstitution AOperationCallSubstitution AAnySubstitution APredicateParseUnit AExpressionParseUnit ANotMemberPredicate ANotSubsetPredicate ANotSubsetStrictPredicate AMaxIntExpression AMinIntExpression ASuccessorExpression APredecessorExpression ATotalRelationExpression ATotalSurjectionRelationExpression ASurjectionRelationExpression ASizeExpression ASubstitutionParseUnit ABecomesSuchSubstitution AAssertionSubstitution APreconditionSubstitution AVarSubstitution ALetSubstitution ASequenceSubstitution AParallelSubstitution AMachineClauseParseUnit AChoiceSubstitution AChoiceOrSubstitution AIfElsifSubstitution AIfSubstitution AOperation ASelectSubstitution ASelectWhenSubstitution)
           (de.be4.classicalb.core.parser.util PrettyPrinter)
           (de.be4.classicalb.core.parser BParser)))

(declare b->ast)

(defn get-ast [key b-node]
  (b->ast (key b-node)))

(defn get-left-ast [node]
  (get-ast :left node))
(defn get-right-ast [node]
  (get-ast :right node))
(defn get-predicate-ast [node]
  (get-ast :predicate node))
(defn get-predicates-ast [node]
  (map b->ast (:predicates node)))
(defn get-expression-ast [node]
  (get-ast :expression node))
(defn get-set-ast [node]
  (get-ast :set node))
(defn get-sets-ast [node]
  (map b->ast (:sets node)))
(defn get-number-ast [node]
  (get-ast :number node))
(defn get-numbers-ast [node]
  (map b->ast (:numbers node)))
(defn get-relation-ast [node]
  (get-ast :relation node))
(defn get-relations-ast [node]
  (map b->ast (:relations node)))
(defn get-seq-ast [node]
  (get-ast :seq node))
(defn get-seqs-ast [node]
  (map b->ast (:seqs node)))
(defn get-substitution-ast [node]
  (get-ast :substitution node))
(defn get-substitutions-ast [node]
  (map b->ast (:substitutions node)))
(defn get-element-ast [node]
  (get-ast :element node))
(defn get-elements-ast [node]
  (map b->ast (:elements node)))
(defn get-assignment-ast [node]
  (get-ast :assignment node))
(defn get-condition-ast [node]
  (get-ast :condition node))
(defn get-then-ast [node]
  (get-ast :then node))
(defn get-else-ast [node]
  (get-ast :else node))
; identifiers may be saved as set
(defn get-identifiers-ast [node]
  (map b->ast (:identifiers node)))
(defn get-parameters [node]
  (map b->ast (:parameters node)))


(defn left-associative [f nodes]
  (reduce f nodes))

(defn right-associative [f nodes]
  (reduce
    (fn [acc value]
      (f value acc))
    (reverse nodes)))

(defn chain [b-tuples f]
  (let [node-tuples (map #(map b->ast %) b-tuples)
        nodes (map (partial apply f) node-tuples)]
    (reduce
      #(AConjunctPredicate. %1 %2)
      nodes)))

(defn chain-arity-two [b-children f]
  (chain (partition 2 1 b-children) f))

(defn chain-combinitions-arity-two [b-children f]
  (chain (combinations b-children 2) f))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti node->ast (fn [node] (:tag node)))

;;; parse units

(defmethod node->ast :machine [node]
  (Start. (AAbstractMachineParseUnit. (get-ast :variant node) (get-ast :header node) (map b->ast (:clauses node))) (EOF.)))
(defmethod node->ast :machine-variant [_]
  (AMachineMachineVariant.))
(defmethod node->ast :machine-header [node]
  (AMachineHeader. (.getIdentifier (get-ast :name node)) (get-parameters node)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; machine clauses

(defmethod node->ast :contraints [node]
  (AConstraintsMachineClause. (get-predicate-ast node)))

(defmethod node->ast :sets [node]
  (ASetsMachineClause. (map b->ast (:set-definitions node))))
(defmethod node->ast :deferred-set [node]
  (ADeferredSetSet. (.getIdentifier (b->ast (:identifier node)))))
(defmethod node->ast :enumerated-set [node]
  (AEnumeratedSetSet. (.getIdentifier (b->ast (:identifier node))) (get-elements-ast node)))

(defmethod node->ast :constants [node]
  (AConstantsMachineClause. (get-identifiers-ast node)))

(defmethod node->ast :properties [node]
  (APropertiesMachineClause. (get-predicate-ast node)))

(defmethod node->ast :definitions [node]
  (ADefinitionsMachineClause. (get-ast :definitions node)))
;TODO:
;(defmethod node->ast :definition)

(defmethod node->ast :variables [node]
  (AVariablesMachineClause. (get-identifiers-ast node)))

(defmethod node->ast :invariants [node]
  (AInvariantMachineClause. (get-predicate-ast node)))

(defmethod node->ast :assertions [node]
  (AAssertionsMachineClause. (get-predicates-ast node)))

(defmethod node->ast :assign [node]
  (AAssertionsMachineClause. (get-predicates-ast node)))

(defmethod node->ast :init [node]
  (AInitialisationMachineClause. (get-substitution-ast node)))

(defmethod node->ast :operations [node]
  (AOperationsMachineClause. (map b->ast (:operations node))))
(defmethod node->ast :operation [node]
  (AOperation. (map b->ast (:return node)) (list (TIdentifierLiteral. (name (:name node)))) (get-parameters node) (get-ast :body node)))

;;; substitutions

(defmethod node->ast :skip [_]
  (ASkipSubstitution.))

(defmethod node->ast :block [node]
  (ABlockSubstitution. (get-substitution-ast node)))

(defmethod node->ast :assign [node]
  (AAssignSubstitution. (get-identifiers-ast node) (map b->ast (:values node))))

(defmethod node->ast :becomes-element-of [node] (ABecomesElementOfSubstitution. (get-identifiers-ast node) (get-set-ast node)))
(defmethod node->ast :becomes-such [node] (ABecomesSuchSubstitution. (get-identifiers-ast node) (get-predicate-ast node)))
(defmethod node->ast :operation-call [node]
  (AOperationCallSubstitution. (get-identifiers-ast node) (list (TIdentifierLiteral. (name (:operation node)))) (get-parameters node)))
(defmethod node->ast :parallel-substitution [node] (AParallelSubstitution. (get-substitutions-ast node)))
(defmethod node->ast :sequence-substitution [node] (ASequenceSubstitution. (get-substitutions-ast node)))
(defmethod node->ast :any [node] (AAnySubstitution. (get-identifiers-ast node) (get-ast :where node) (get-then-ast node)))
(defmethod node->ast :let-sub [node] (ALetSubstitution. (get-identifiers-ast node) (get-predicate-ast node) (get-substitution-ast node)))
(defmethod node->ast :var [node]
  (println (:identifiers node))
  (println (map b->ast (:identifiers node)))
  (AVarSubstitution. (map b->ast (:identifiers node)) (get-substitution-ast node))) ()
(defmethod node->ast :precondition [node] (APreconditionSubstitution. (get-predicate-ast node) (get-substitution-ast node)))
(defmethod node->ast :assert [node] (AAssertionSubstitution. (get-predicate-ast node) (get-substitution-ast node)))

(defn choice-or-substitution [sub]
  (AChoiceOrSubstitution. sub))
(defmethod node->ast :choice [node]
  (let [subs (:substitutions node)
        first-sub (node->ast (first subs))
        rest-subs (map choice-or-substitution (map node->ast (rest subs)))]
    (AChoiceSubstitution. (conj rest-subs first-sub))))

(defmethod node->ast :if-sub [node]
  (AIfSubstitution. (get-condition-ast node) (get-then-ast node) () (get-else-ast node)))

(defn if-else-sub [[condition then]]
  (AIfElsifSubstitution. condition then))
(defmethod node->ast :cond [node]
  (let [clauses (map b->ast (:clauses node))
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
(defmethod node->ast :select [node]
  (let [clauses (map b->ast (:clauses node))
        condition (first clauses)
        then (second clauses)]
    (if (even? (count clauses))
      (let [else-ifs (map select-when-sub (partition 2 (drop 2 clauses)))]
        (ASelectSubstitution. condition then else-ifs nil))
      (let [else (last clauses)
            else-ifs (map select-when-sub (partition 2 (drop-last 1 (drop 2 clauses))))]
        (ASelectSubstitution. condition then else-ifs else)))))


;;; if

(defmethod node->ast :if-expr [node] (AIfThenElseExpression. (get-condition-ast node) (get-then-ast node) '() (get-else-ast node)))


;;; let

(defmethod node->ast :let-expr [node]
  ;;; #object[de.be4.classicalb.core.parser.node.ALetExpressionExpression 0x66f5b8fe x y x 1 y 2 3 ]
  ;;; #object[java.util.LinkedList 0x19b75b2b [x , y ]]
  ;;; #object[de.be4.classicalb.core.parser.node.AConjunctPredicate 0x4b9c411 x 1 y 2 ]
  ;;; #object[de.be4.classicalb.core.parser.node.AIntegerExpression 0x233f52f8 3 ]
  (let [ast (ALetExpressionExpression. (get-identifiers-ast node) (get-assignment-ast node) (get-expression-ast node))]
    (println ast)
    (println (.getIdentifiers ast))
    (println (.getAssignment ast))
    (println (.getExpr ast)))
  (ALetExpressionExpression. (get-identifiers-ast node) (get-assignment-ast node) (get-expression-ast node)))

(defmethod node->ast :let-pred [node]
  (ALetPredicatePredicate. (get-identifiers-ast node) (get-assignment-ast node) (get-predicate-ast node)))


;;; strings

(defmethod node->ast :string-set [_] (AStringSetExpression.))


;;; records

(defmethod node->ast :struct [node]
  (AStructExpression. (map
                        (fn [[k v]] (ARecEntry. (b->ast k) (b->ast v)))
                        (partition 2 (:id-types node)))))

(defmethod node->ast :record [node]
  (ARecExpression. (map
                     (fn [[k v]] (ARecEntry. (b->ast k) (b->ast v)))
                     (partition 2 (:id-values node)))))

(defmethod node->ast :rec-get [node]
  (ARecordFieldExpression. (get-ast :record node) (get-ast :identifier node)))


;;; sequences

(defmethod node->ast :empty-sequence [_]
  (AEmptySequenceExpression.))

(defmethod node->ast :sequence [node]
  (ASequenceExtensionExpression. (map b->ast (:elements node))))

(defmethod node->ast :seq [node]
  (ASeqExpression. (get-set-ast node)))

(defmethod node->ast :seq1 [node]
  (ASeq1Expression. (get-set-ast node)))

(defmethod node->ast :iseq [node]
  (AIseqExpression. (get-set-ast node)))

(defmethod node->ast :iseq1 [node]
  (AIseq1Expression. (get-set-ast node)))

(defmethod node->ast :perm [node]
  (APermExpression. (get-set-ast node)))

(defmethod node->ast :size [node]
  (ASizeExpression. (get-set-ast node)))

(defmethod node->ast :concat [node]
  (left-associative #(AConcatExpression. %1 %2) (get-seqs-ast node)))

(defmethod node->ast :insert-front [node]
  (right-associative #(AInsertFrontExpression. %1 %2) (reverse (conj (get-elements-ast node) (get-seq-ast node)))))

(defmethod node->ast :insert-tail [node]
  (left-associative #(AInsertTailExpression. %1 %2) (conj (get-elements-ast node) (get-seq-ast node))))

(defmethod node->ast :reverse [node]
  (ARevExpression. (get-seq-ast node)))

(defmethod node->ast :first [node]
  (AFirstExpression. (get-seq-ast node)))

(defmethod node->ast :last [node]
  (ALastExpression. (get-seq-ast node)))

(defmethod node->ast :front [node]
  (AFrontExpression. (get-seq-ast node)))

(defmethod node->ast :tail [node]
  (ATailExpression. (get-seq-ast node)))

(defmethod node->ast :conc [node]
  (AGeneralConcatExpression. (get-ast :seq-of-seqs node)))

(defmethod node->ast :restrict-front [node]
  (ARestrictFrontExpression. (get-seq-ast node) (get-number-ast node)))

(defmethod node->ast :restrict-tail [node]
  (ARestrictTailExpression. (get-seq-ast node) (get-number-ast node)))


;;; functions

(defmethod node->ast :partial-fn [node]
  (left-associative #(APartialFunctionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :total-fn [node]
  (left-associative #(ATotalFunctionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :partial-surjection [node]
  (left-associative #(APartialSurjectionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :total-surjection [node]
  (left-associative #(ATotalSurjectionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :partial-injection [node]
  (left-associative #(APartialInjectionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :total-injection [node]
  (left-associative #(ATotalInjectionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :partial-bijection [node]
  (left-associative #(APartialBijectionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :total-bijection [node]
  (left-associative #(ATotalBijectionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :lambda [node]
  (ALambdaExpression. (get-identifiers-ast node) (get-predicate-ast node) (get-expression-ast node)))

(defmethod node->ast :apply [node]
  (AFunctionExpression. (get-ast :f node) (map b->ast (:args node))))


;;; relations

(defmethod node->ast :relation [node]
  (left-associative #(ARelationsExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :total-relation [node]
  (left-associative #(ATotalRelationExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :surjective-relation [node]
  (left-associative #(ASurjectionRelationExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :total-surjective-relation [node]
  (left-associative #(ATotalSurjectionRelationExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :couple [node]
  (ACoupleExpression. (get-elements-ast node)))

(defmethod node->ast :domain [node]
  (ADomainExpression. (get-relation-ast node)))

(defmethod node->ast :range [node]
  (ARangeExpression. (get-relation-ast node)))

(defmethod node->ast :identity-relation [node]
  (AIdentityExpression. (get-set-ast node)))

(defmethod node->ast :domain-restriction [node]
  (ADomainRestrictionExpression. (get-set-ast node) (get-relation-ast node)))

(defmethod node->ast :domain-subtraction [node]
  (ADomainSubtractionExpression. (get-set-ast node) (get-relation-ast node)))

(defmethod node->ast :range-restriction [node]
  (ARangeRestrictionExpression. (get-relation-ast node) (get-set-ast node)))

(defmethod node->ast :range-subtraction [node]
  (ARangeSubtractionExpression. (get-relation-ast node) (get-set-ast node)))

(defmethod node->ast :inverse-relation [node]
  (AReverseExpression. (get-relation-ast node)))

(defmethod node->ast :relational-image [node]
  (AImageExpression. (get-relation-ast node) (get-set-ast node)))

(defmethod node->ast :relational-override [node]
  (left-associative #(AOverwriteExpression. %1 %2) (get-relations-ast node)))

(defmethod node->ast :direct-product [node]
  (left-associative #(ADirectProductExpression. %1 %2) (get-relations-ast node)))

(defmethod node->ast :relational-composition [node]
  (left-associative #(ACompositionExpression. %1 %2) (get-relations-ast node)))

(defmethod node->ast :parallel-product [node]
  (left-associative #(AParallelProductExpression. %1 %2) (get-relations-ast node)))

(defmethod node->ast :prj1 [node]
  (AFirstProjectionExpression. (get-ast :set1 node) (get-ast :set2 node)))

(defmethod node->ast :prj2 [node]
  (ASecondProjectionExpression. (get-ast :set1 node) (get-ast :set2 node)))

(defmethod node->ast :closure1 [node]
  (AClosureExpression. (get-relation-ast node)))

(defmethod node->ast :closure [node]
  (AReflexiveClosureExpression. (get-relation-ast node)))

(defmethod node->ast :iterate [node]
  (AIterationExpression. (get-relation-ast node) (get-number-ast node)))

(defmethod node->ast :functionise [node]
  (ATransFunctionExpression. (get-relation-ast node)))

(defmethod node->ast :relationise [node]
  (ATransRelationExpression. (get-relation-ast node)))


;;; numbers

(defmethod node->ast :unary-minus [node]
  (AUnaryMinusExpression. (get-number-ast node)))

(defmethod node->ast :integer-set  [_]
  (AIntegerSetExpression.))

(defmethod node->ast  :natural-set [_]
  (ANaturalSetExpression.))

(defmethod node->ast :natural1-set [_]
  (ANatural1SetExpression.))

(defmethod node->ast :int-set [_]
  (AIntSetExpression.))

(defmethod node->ast :nat-set [_]
  (ANatSetExpression.))

(defmethod node->ast :nat1-set [_]
  (ANat1SetExpression.))

(defmethod node->ast :interval [node]
  (AIntervalExpression. (get-ast :from node) (get-ast :to node)))

(defmethod node->ast :min-int  [_]
  (AMinIntExpression.))

(defmethod node->ast :max-int  [_]
  (AMaxIntExpression.))

(defmethod node->ast :less [node]
  (chain-arity-two (:numbers node) #(ALessPredicate. %1 %2)))

(defmethod node->ast :greater [node]
  (chain-arity-two (:numbers node) #(AGreaterPredicate. %1 %2)))

(defmethod node->ast :less-eq [node]
  (chain-arity-two (:numbers node) #(ALessEqualPredicate. %1 %2)))

(defmethod node->ast :greater-eq [node]
  (chain-arity-two (:numbers node) #(AGreaterEqualPredicate. %1 %2)))

(defmethod node->ast :max [node]
  (AMaxExpression. (get-set-ast node)))

(defmethod node->ast :min [node]
  (AMinExpression. (get-set-ast node)))

(defmethod node->ast :plus [node]
  (left-associative #(AAddExpression. %1 %2) (get-numbers-ast node)))

(defmethod node->ast :minus [node]
  (left-associative #(AMinusOrSetSubtractExpression. %1 %2) (get-numbers-ast node)))

(defmethod node->ast :mult-or-cart [node]
  (left-associative #(AMultOrCartExpression. %1 %2) (get-numbers-ast node)))

(defmethod node->ast :div [node]
  (left-associative #(ADivExpression. %1 %2) (get-numbers-ast node)))

(defmethod node->ast :pow [node]
  (right-associative #(APowerOfExpression. %1 %2) (get-numbers-ast node)))

(defmethod node->ast :mod [node]
  (left-associative #(AModuloExpression. %1 %2) (get-numbers-ast node)))

(defmethod node->ast :pi [node]
  (AGeneralProductExpression. (get-identifiers-ast node) (get-predicate-ast node) (get-expression-ast node)))

(defmethod node->ast :sigma  [node]
  (AGeneralSumExpression. (get-identifiers-ast node) (get-predicate-ast node) (get-expression-ast node)))

(defmethod node->ast :inc [node]
  (AFunctionExpression. (ASuccessorExpression.) (list (get-number-ast node))))

(defmethod node->ast :dec [node]
  (AFunctionExpression. (APredecessorExpression.) (list (get-number-ast node))))


;;; sets

(defmethod node->ast :comp-set [node]
  (AComprehensionSetExpression. (get-identifiers-ast node) (get-predicate-ast node)))

(defmethod node->ast :power-set [node]
  (APowSubsetExpression. (get-set-ast node)))

(defmethod node->ast :power1-set [node]
  (APow1SubsetExpression. (get-set-ast node)))

(defmethod node->ast :fin [node]
  (AFinSubsetExpression. (get-set-ast node)))

(defmethod node->ast :fin1 [node]
  (AFin1SubsetExpression. (get-set-ast node)))

(defmethod node->ast :card [node]
  (ACardExpression. (get-set-ast node)))

(defmethod node->ast :union [node]
  (left-associative #(AUnionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :intersection [node]
  (left-associative #(AIntersectionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :difference [node]
  (left-associative #(ASetSubtractionExpression. %1 %2) (get-sets-ast node)))

(defmethod node->ast :member [node]
  (AMemberPredicate. (get-element-ast node) (get-set-ast node)))

(defmethod node->ast :subset [node]
  (ASubsetPredicate. (get-ast :subset node) (get-set-ast node)))

(defmethod node->ast :subset-strict [node]
  (ASubsetStrictPredicate. (get-ast :subset-strict node) (get-set-ast node)))

(defmethod node->ast :general-union [node]
  (AGeneralUnionExpression. (get-ast :set-of-sets node)))

(defmethod node->ast :general-intersection [node]
  (AGeneralIntersectionExpression. (get-ast :set-of-sets node)))

(defmethod node->ast :union-pe [node]
  (AQuantifiedUnionExpression. (get-identifiers-ast node) (get-predicate-ast node) (get-expression-ast node)))

(defmethod node->ast :intersection-pe [node]
  (AQuantifiedIntersectionExpression. (get-identifiers-ast node) (get-predicate-ast node) (get-expression-ast node)))


;;; booleans

(defmethod node->ast :bool-set [_]
  (ABoolSetExpression.))

(defmethod node->ast :pred->bool  [node]
  (AConvertBoolExpression. (get-predicate-ast node)))


;;; equal predicates

(defmethod node->ast :equal [node]
  (AEqualPredicate. (get-left-ast node) (get-right-ast node)))

(defmethod node->ast :not-equal [node]
  (ANotEqualPredicate. (get-left-ast node) (get-right-ast node)))

(defmethod node->ast :distinct [node]
  (chain-combinitions-arity-two (:elements node) #(ANotEqualPredicate. %1 %2)))


;;; logical predicates

(defmethod node->ast :and [node]
  (left-associative #(AConjunctPredicate. %1 %2) (get-predicates-ast node)))

(defmethod node->ast :or [node]
  (left-associative #(ADisjunctPredicate. %1 %2) (get-predicates-ast node)))

(defmethod node->ast :implication [node]
  (left-associative #(AImplicationPredicate. %1 %2) (get-predicates-ast node)))

(defmethod node->ast :equivalence [node]
  (left-associative #(AEquivalencePredicate. %1 %2) (get-predicates-ast node)))

(defmethod node->ast :not [node]
  (let [predicate (get-predicate-ast node)
        pt (type predicate)]
    ; simplify
    (cond
      (= AMemberPredicate pt) (ANotMemberPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetPredicate pt) (ANotSubsetPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetStrictPredicate pt) (ANotSubsetStrictPredicate. (.getLeft predicate) (.getRight predicate))
      :else (ANegationPredicate. predicate))))

(defmethod node->ast :for-all [node]
  (AForallPredicate. (get-identifiers-ast node) (get-ast :implication node)))

(defmethod node->ast :exists [node]
  (AExistsPredicate. (get-identifiers-ast node) (get-predicate-ast node)))

;;; misc

(defmethod node->ast :call [node]
  (ADefinitionExpression. (TIdentifierLiteral. (name (:f node))) (map b->ast (:args node))))

;;;;;;;;;;;;;;

(defn set-expression [elements]
  (if (empty? elements)
    (AEmptySetExpression.)
    (ASetExtensionExpression. elements)))

(defn b->ast [b]
  (cond
    (map? b) (node->ast b)
    (set? b) (set-expression (map b->ast b))
    (vector? b) (ACoupleExpression. (map b->ast b))
    (keyword? b) (AIdentifierExpression. [(TIdentifierLiteral. (name b))])
    (string? b) (AStringExpression. (TStringLiteral. b)) ;; hack-y thing to avoid renaming of rec-get parameters in preds
    (number? b) (AIntegerExpression. (TIntegerLiteral. (str b)))
    (true? b) (ABooleanTrueExpression.)
    (false? b) (ABooleanFalseExpression.)
    (nil? b) nil
    :otherwise (println :unhandled-literal b)))

(defn b->predicate-ast [b]
  (Start. (APredicateParseUnit. (b->ast b)) (EOF.)))

(defn b->expression-ast [b]
  (Start. (AExpressionParseUnit. (b->ast b)) (EOF.)))

(defn b->substitution-ast [b]
  (Start. (ASubstitutionParseUnit. (b->ast b)) (EOF.)))

(defn b->machine-clause-ast [b]
  (Start. (AMachineClauseParseUnit. (b->ast b)) (EOF.)))
