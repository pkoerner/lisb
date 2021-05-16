(ns lisb.translation
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:require [clojure.walk :refer [walk]])
  (:require [lisb.representation :refer [node]])
  (:import de.prob.animator.domainobjects.ClassicalB
           (de.be4.classicalb.core.parser.node Start
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

(declare node-repr->ast)

(defn identifier [n]
  (AIdentifierExpression. [(TIdentifierLiteral. (name n))]))

(defn get-value [node]
  (node-repr->ast (:value node)))
(defn get-left [node]
  (node-repr->ast (:left node)))
(defn get-right [node]
  (node-repr->ast (:right node)))
(defn get-identifiers [node]
  (map node-repr->ast (:identifiers node)))                 ; identifiers may be saved as set
(defn get-parameters [node]
  (map node-repr->ast (:parameters node)))                 ; identifiers may be saved as set
(defn get-predicate [node]
  (node-repr->ast (:predicate node)))               ; identifiers may be saved as set
(defn get-predicates [node]
  (node-repr->ast (:predicates node)))
(defn get-expressions [node]
  (node-repr->ast (:expressions node)))
(defn get-expression [node]
  (node-repr->ast (:expression node)))
(defn get-implication [node]
  (node-repr->ast (:implication node)))
(defn get-set [node]
  (node-repr->ast (:set node)))
(defn get-element [node]
  (node-repr->ast (:element node)))
(defn get-sets [node]
  (node-repr->ast (:sets node)))
(defn get-numbers [node]
  (node-repr->ast (:numbers node)))
(defn get-value [node]
  (node-repr->ast (:value node)))
(defn get-relation [node]
  (node-repr->ast (:relation node)))
(defn get-relations [node]
  (node-repr->ast (:relations node)))
(defn get-seq [node]
  (node-repr->ast (:seq node)))
(defn get-seqs [node]
  (node-repr->ast (:seqs node)))
(defn get-substitution [node]
  (node-repr->ast (:substitution node)))
(defn get-node [key node]
  (node-repr->ast (key node)))


(defn left-associative [f nodes]
  (reduce
    f
    nodes))

(defn right-associative [f nodes]
  (reduce
    (fn [acc value]
      (f value acc))
    (reverse nodes)))

(defn chain-arity-two [unprocessed-children f]
  (let [tuples (partition 2 1 unprocessed-children)
        processed-tuples (map #(map node-repr->ast %) tuples)
        nodes (map (partial apply f) processed-tuples)]
    (reduce
      #(AConjunctPredicate. %1 %2)
      nodes)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-enum
  ([] (AEmptySetExpression.))
  ([& args] (ASetExtensionExpression. args)))

(declare node-repr->ast)

(defmulti process-node (fn [node] (:tag node)))

;;; parse units

(defmethod process-node :machine [node]
  (Start. (AAbstractMachineParseUnit. (node-repr->ast (:variant node)) (node-repr->ast (:header node)) (map node-repr->ast (:clauses node))) (EOF.)))
(defmethod process-node :machine-variant [node]
  (AMachineMachineVariant.))
(defmethod process-node :machine-header [node]
  (AMachineHeader. (.getIdentifier (node-repr->ast (:name node))) (map node-repr->ast (:parameters node))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; machine clauses

(defmethod process-node :contraints [node]
  (AConstraintsMachineClause. (get-predicate node)))

(defmethod process-node :sets [node]
  (ASetsMachineClause. (get-node :set-definitions node)))
(defmethod process-node :deferred-set [node]
  (ADeferredSetSet. (get-identifiers node)))
(defmethod process-node :enumerated-set [node]
  (AEnumeratedSetSet. (get-identifiers node) (get-node :elements node)))

(defmethod process-node :constants [node]
  (AConstantsMachineClause. (get-identifiers node)))

(defmethod process-node :properties [node]
  (APropertiesMachineClause. (get-predicate node)))

(defmethod process-node :definitions [node]
  (ADefinitionsMachineClause. (get-node :definitions node)))

(defmethod process-node :variables [node]
  (AVariablesMachineClause. (get-identifiers node)))

(defmethod process-node :invariants [node]
  (AInvariantMachineClause. (get-predicate node)))

(defmethod process-node :assign [node]
  (AAssertionsMachineClause. (get-node :predicates node)))

(defmethod process-node :init [node]
  (AInitialisationMachineClause. (get-substitution node) ))

(defmethod process-node :operations [node]
  (AOperationsMachineClause. (get-node :operations node)))
(defmethod process-node :operation [node]
  (AOperation. (map node-repr->ast (:return node)) (list (TIdentifierLiteral. (name (:name node)))) (get-parameters node) (get-node :body node)))

;;; substitutions

(defmethod process-node :skip [_]
  (ASkipSubstitution.))

(defmethod process-node :block [node]
  (ABlockSubstitution. (get-substitution node)))

(defmethod process-node :assign [node]
  (AAssignSubstitution. (get-identifiers node) (get-node :values node)))

(defmethod process-node :becomes-element-of [node] (ABecomesElementOfSubstitution. (get-identifiers node) (get-set node)))
(defmethod process-node :becomes-such [node] (ABecomesSuchSubstitution. (get-identifiers node) (get-predicate node)))
(defmethod process-node :operation-call [node]
  (AOperationCallSubstitution. (get-identifiers node) (list (TIdentifierLiteral. (name (:operation node)))) (get-parameters node)))
(defmethod process-node :parallel-substitution [node] (AParallelSubstitution. (get-node :substitutions node)))
(defmethod process-node :sequence-substitution [node] (ASequenceSubstitution. (get-node :substitutions node)))
(defmethod process-node :any [node] (AAnySubstitution. (get-identifiers node) (get-node :where node) (get-node :then node)))
(defmethod process-node :let-sub [node] (ALetSubstitution. (get-identifiers node) (get-predicate node) (get-substitution node)))
(defmethod process-node :var [node]
  (AVarSubstitution. (map node-repr->ast (:identifiers node)) (get-substitution node))) ()
(defmethod process-node :precondition [node] (APreconditionSubstitution. (get-predicate node) (get-substitution node)))
(defmethod process-node :assert [node] (AAssertionSubstitution. (get-predicate node) (get-substitution node)))

(defn choice-or-substitution [sub]
  (AChoiceOrSubstitution. sub))
(defmethod process-node :choice [node]
  (let [subs (:substitutions node)
        first-sub (process-node (first subs))
        rest-subs (map choice-or-substitution (map process-node (rest subs)))]
    (AChoiceSubstitution. (conj rest-subs first-sub))))

(defmethod process-node :if-sub [node]
  (AIfSubstitution. (get-node :condition node) (get-node :then node) () (get-node :else node)))

(defn if-else-sub [[condition then]]
  (AIfElsifSubstitution. condition then))
(defmethod process-node :cond [node]
  (let [clauses (get-node :clauses node)
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
(defmethod process-node :select [node]
  (let [clauses (get-node :clauses node)
        condition (first clauses)
        then (second clauses)]
    (if (even? (count clauses))
      (let [else-ifs (map select-when-sub (partition 2 (drop 2 clauses)))]
        (ASelectSubstitution. condition then else-ifs nil))
      (let [else (last clauses)
            else-ifs (map select-when-sub (partition 2 (drop-last 1 (drop 2 clauses))))]
        (ASelectSubstitution. condition then else-ifs else)))))

;;; if
(defmethod process-node :if-expr [node] (AIfThenElseExpression. (get-node :condition node) (get-node :then node) '() (get-node :else node)))

;;; let

(defmethod process-node :let-expr [node] (ALetExpressionExpression. (get-identifiers node) (get-node :assignment node) (get-expression node)))

(defmethod process-node :let-pred [node] (ALetPredicatePredicate. (get-identifiers node) (get-node :assignment node) (get-predicate node)))

;;; strings

(defmethod process-node :string-set [_] (AStringSetExpression.))


;;; records

(defmethod process-node :struct [node]
  (AStructExpression. (map
                        (fn [[k v]] (ARecEntry. (node-repr->ast k) (node-repr->ast v)))
                        (:id-types node))))

(defmethod process-node :record [node]
  (ARecExpression. (map
                     (fn [[k v]] (ARecEntry. (node-repr->ast k) (node-repr->ast v)))
                     (:id-values node))))

(defmethod process-node :rec-get [node]
  (ARecordFieldExpression. (get-node :record node) (get-node :identifier node)))


;;; sequences

(defmethod process-node :seq [node]
  (ASeqExpression. (get-node :set node)))

(defmethod process-node :seq1 [node]
  (ASeq1Expression. (get-node :set node)))

(defmethod process-node :iseq [node]
  (AIseqExpression. (get-node :set node)))

(defmethod process-node :iseq1 [node]
  (AIseq1Expression. (get-node :set node)))

(defmethod process-node :perm [node]
  (APermExpression. (get-node :set node)))

(defmethod process-node :size [node]
  (ASizeExpression. (get-node :set node)))

(defmethod process-node :concat [node]
  (left-associative #(AConcatExpression. %1 %2) (get-seqs node)))

(defmethod process-node :insert-front [node]
  (left-associative #(AInsertFrontExpression. %1 %2) (concat (get-node :elements node) (list (get-seq node)))))

(defmethod process-node :insert-tail [node]
  (left-associative  #(AInsertTailExpression. %1 %2) (conj (get-node :elements node) (get-seq node))))

(defmethod process-node :reverse [node]
  (AReverseExpression. (get-seq node)))

(defmethod process-node :first [node]
  (AFirstExpression. (get-seq node)))

(defmethod process-node :last [node]
  (ALastExpression. (get-seq node)))

(defmethod process-node :front [node]
  (AFrontExpression. (get-seq node)))

(defmethod process-node :tail [node]
  (ATailExpression. (get-seq node)))

(defmethod process-node :conc [node]
  (AGeneralConcatExpression. (get-node :seq-of-seqs node)))

(defmethod process-node :restrict-front [node]
  (ARestrictFrontExpression. (get-seq node) (get-node :number node)))

(defmethod process-node :restrict-tail [node]
  (ARestrictTailExpression. (get-seq node) (get-node :number node)))


;;; functions

(defmethod process-node :partial-fn [node]
  (left-associative #(APartialFunctionExpression. %1 %2) (get-sets node)))

(defmethod process-node :total-fn [node]
  (left-associative #(ATotalFunctionExpression. %1 %2) (get-sets node)))

(defmethod process-node :partial-surjection [node]
  (left-associative #(APartialSurjectionExpression. %1 %2) (get-sets node)))

(defmethod process-node :total-surjection [node]
  (left-associative  #(ATotalSurjectionExpression. %1 %2) (get-sets node)))

(defmethod process-node :partial-injection [node]
  (left-associative #(APartialInjectionExpression. %1 %2) (get-sets node)))

(defmethod process-node :total-injection [node]
  (left-associative #(ATotalInjectionExpression. %1 %2) (get-sets node)))

(defmethod process-node :partial-bijection [node]
  (left-associative #(APartialBijectionExpression. %1 %2) (get-sets node)))

(defmethod process-node :total-bijection [node]
  (left-associative #(ATotalBijectionExpression. %1 %2) (get-sets node)))

(defmethod process-node :lambda [node]
  (ALambdaExpression. (get-identifiers node) (get-predicate node) (get-expression node)))

(defmethod process-node :call [node]
  (AFunctionExpression. (get-node :f node) (get-node :args node)))


;;; relations

(defmethod process-node :relation [node]
  (left-associative #(ARelationsExpression. %1 %2) (get-sets node)))

(defmethod process-node :total-relation [node]
  (left-associative #(ATotalRelationExpression. %1 %2) (get-sets node)))

(defmethod process-node :surjective-relation [node]
  (left-associative #(ASurjectionRelationExpression. %1 %2) (get-sets node)))

(defmethod process-node :total-surjective-relation [node]
  (left-associative #(ATotalSurjectionRelationExpression. %1 %2) (get-sets node)))

(defmethod process-node :couple [node]
  (ACoupleExpression. (get-node :elements node)))

(defmethod process-node :domain [node]
  (ADomainExpression. (get-relation node)))

(defmethod process-node :range [node]
  (ARangeExpression. (get-relation node)))

(defmethod process-node :identity-relation [node]
  (AIdentityExpression. (get-set node)))

(defmethod process-node :domain-restriction [node]
  (ADomainRestrictionExpression. (get-set node) (get-relation node)))

(defmethod process-node :domain-subtraction [node]
  (ADomainSubtractionExpression. (get-set node) (get-relation node)))

(defmethod process-node :range-restriction [node]
  (ARangeRestrictionExpression. (get-relation node) (get-set node)))

(defmethod process-node :range-subtraction [node]
  (ARangeSubtractionExpression. (get-relation node) (get-set node)))

(defmethod process-node :inverse-relation [node]
  (AReverseExpression. (get-relation node)))

(defmethod process-node :relational-image [node]
  (AImageExpression. (get-relation node) (get-set node)))

(defmethod process-node :relational-override [node]
  (left-associative #(AOverwriteExpression. %1 %2) (get-relations node)))

(defmethod process-node :direct-product [node]
  (left-associative #(ADirectProductExpression. %1 %2) (get-relations node)))

(defmethod process-node :relational-composition [node]
  (left-associative #(ACompositionExpression. %1 %2) (get-relations node)))

(defmethod process-node :parallel-product [node]
  (left-associative #(AParallelProductExpression. %1 %2) (get-relations node)))

(defmethod process-node :prj1 [node]
  (AFirstProjectionExpression. (get-node :set1 node) (get-node :set2 node)))

(defmethod process-node :prj2 [node]
  (ASecondProjectionExpression. (get-node :set1 node) (get-node :set2 node)))

(defmethod process-node :closure1 [node]
  (AClosureExpression. (get-relation node)))

(defmethod process-node :closure [node]
  (AReflexiveClosureExpression. (get-relation node)))

(defmethod process-node :iterate [node]
  (AIterationExpression. (get-relation node) (get-node :number node)))

(defmethod process-node :functionise [node]
  (ATransFunctionExpression. (get-relation node)))

(defmethod process-node :relationise [node]
  (ATransRelationExpression. (get-relation node)))


;;; numbers

(defmethod process-node :unary-minus [node]
  (AUnaryMinusExpression. (get-node :number node)))

(defmethod process-node :integer-set  [_]
  (AIntegerSetExpression.))

(defmethod process-node  :natural-set [_]
  (ANaturalSetExpression.))

(defmethod process-node :natural1-set [_]
  (ANatural1SetExpression.))

(defmethod process-node :int-set [_]
  (AIntSetExpression.))

(defmethod process-node :nat-set [_]
  (ANatSetExpression.))

(defmethod process-node :nat1-set [_]
  (ANat1SetExpression.))

(defmethod process-node :interval [node]
  (AIntervalExpression. (node-repr->ast (:from node)) (node-repr->ast (:to node))))

(defmethod process-node :min-int  [_]
  (AMinIntExpression.))

(defmethod process-node :max-int  [_]
  (AMaxIntExpression.))

(defmethod process-node :less [node]
  (chain-arity-two (:numbers node) #(ALessPredicate. %1 %2)))

(defmethod process-node :greater [node]
  (chain-arity-two (:numbers node) #(AGreaterPredicate. %1 %2)))

(defmethod process-node :less-eq [node]
  (chain-arity-two (:numbers node) #(ALessEqualPredicate. %1 %2)))

(defmethod process-node :greater-eq [node]
  (chain-arity-two (:numbers node) #(AGreaterEqualPredicate. %1 %2)))

(defmethod process-node :max [node]
  (AMaxExpression. (get-set node)))

(defmethod process-node :min [node]
  (AMinExpression. (get-set node)))

(defmethod process-node :plus [node]
  (left-associative #(AAddExpression. %1 %2) (get-numbers node)))

(defmethod process-node :minus [node]
  (left-associative #(AMinusOrSetSubtractExpression. %1 %2) (get-numbers node)))

(defmethod process-node :mult-or-cart [node]
  (left-associative #(AMultOrCartExpression. %1 %2) (get-numbers node)))

(defmethod process-node :div [node]
  (left-associative #(ADivExpression. %1 %2) (get-numbers node)))

(defmethod process-node :pow [node]
  (right-associative #(APowerOfExpression. %1 %2) (get-numbers node)))

(defmethod process-node :mod [node]
  (left-associative #(AModuloExpression. %1 %2) (get-numbers node)))

(defmethod process-node :pi [node]
  (AGeneralProductExpression. (get-identifiers node) (get-predicate node) (get-expression node)))

(defmethod process-node :sigma  [node]
  (AGeneralSumExpression. (get-identifiers node) (get-predicate node) (get-expression node)))

(defmethod process-node :inc [node]
  (AFunctionExpression. (ASuccessorExpression.) (list (get-node :number node))))

(defmethod process-node :dec [node]
  (AFunctionExpression. (APredecessorExpression.) (list (get-node :number node))))


;;; sets

(defmethod process-node :comp-set [node]
  (AComprehensionSetExpression. (get-identifiers node) (get-predicate node)))

(defmethod process-node :power-set [node]
  (APowSubsetExpression. (get-set node)))

(defmethod process-node :power1-set [node]
  (APow1SubsetExpression. (get-set node)))

(defmethod process-node :fin [node]
  (AFinSubsetExpression. (get-set node)))

(defmethod process-node :fin1 [node]
  (AFin1SubsetExpression. (get-set node)))

(defmethod process-node :card [node]
  (ACardExpression. (get-node :set node)))

(defmethod process-node :union [node]
  (left-associative #(AUnionExpression. %1 %2) (get-sets node)))

(defmethod process-node :intersection [node]
  (left-associative #(AIntersectionExpression. %1 %2) (get-sets node)))

(defmethod process-node :difference [node]
  (left-associative #(ASetSubtractionExpression. %1 %2) (get-sets node)))

(defmethod process-node :member [node]
  (AMemberPredicate. (get-element node) (get-set node)))

(defmethod process-node :subset [node]
  (ASubsetPredicate. (get-node :subset node) (get-set node)))

(defmethod process-node :subset-strict [node]
  (ASubsetStrictPredicate. (get-node :subset-strict node) (get-set node)))

(defmethod process-node :general-union [node]
  (AGeneralUnionExpression. (get-sets node)))

(defmethod process-node :general-intersection [node]
  (AGeneralIntersectionExpression. (get-sets node)))

(defmethod process-node :union-pe [node]
  (AQuantifiedUnionExpression. (get-identifiers node) (get-predicate node) (get-expression node)))

(defmethod process-node :intersection-pe [node]
  (AQuantifiedIntersectionExpression. (get-identifiers node) (get-predicate node) (get-expression node)))


;;; booleans

(defmethod process-node :bool-set [_]
  (ABoolSetExpression.))

(defmethod process-node :pred->bool  [node]
  (AConvertBoolExpression. (get-predicate node)))


;;; equal predicates

(defmethod process-node :equal [node]
  (AEqualPredicate. (get-left node) (get-right node)))

(defmethod process-node :not-equal [node]
  (ANotEqualPredicate. (get-left node) (get-right node)))


;;; logical predicates

(defmethod process-node :and [node]
  (left-associative #(AConjunctPredicate. %1 %2) (get-predicates node)))

(defmethod process-node :or [node]
  (left-associative #(ADisjunctPredicate. %1 %2) (get-predicates node) ))

(defmethod process-node :implication [node]
  (left-associative #(AImplicationPredicate. %1 %2) (get-predicates node)))

(defmethod process-node :equivalence [node]
  (left-associative #(AEquivalencePredicate. %1 %2) (get-predicates node)))

(defmethod process-node :not [node]
  (let [predicate (get-predicate node)
        pt (type predicate)]
    ; simplify
    (cond
      (= AMemberPredicate pt) (ANotMemberPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetPredicate pt) (ANotSubsetPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetStrictPredicate pt) (ANotSubsetStrictPredicate. (.getLeft predicate) (.getRight predicate))
      :else (ANegationPredicate. predicate))))

(defmethod process-node :for-all [node]
  (AForallPredicate. (get-identifiers node) (AImplicationPredicate. (get-node :assignment node) (get-node :implication node))))

(defmethod process-node :exists [node]
  (AExistsPredicate. (get-identifiers node) (AConjunctPredicate. (get-node :assignment node) (get-node :conjunct node))))


;;;;;;;;;;;;;;

(defn literal [x]
  (cond (nil? x) nil
        (keyword? x) (identifier x)
        (string? x) (AStringExpression. (TStringLiteral. x)) ;; hack-y thing to avoid renaming
        ;; of rec-get parameters in preds
        (number? x) (AIntegerExpression. (TIntegerLiteral. (str x)))
        (true? x) (ABooleanTrueExpression.)
        (false? x) (ABooleanFalseExpression.)
        (set? x) (apply set-enum (map node-repr->ast x))
        ;(sequential? x) (apply tuple-node (map lisb->ast x))
        :otherwise (println :unhandled-literal x)))

(defn node-repr->ast [lisb]
  (cond
    (map? lisb) (process-node lisb)
    (seq? lisb) (map node-repr->ast lisb)
    :else (literal lisb)))

(defn node-repr->predicate-ast [lisb]
  (Start. (APredicateParseUnit. (node-repr->ast lisb)) (EOF.)))

(defn node-repr->expression-ast [lisb]
  (Start. (AExpressionParseUnit. (node-repr->ast lisb)) (EOF.)))

(defn node-repr->substitution-ast [lisb]
  (Start. (ASubstitutionParseUnit. (node-repr->ast lisb)) (EOF.)))

(defn node-repr->machine-clause-ast [lisb]
  (Start. (AMachineClauseParseUnit. (node-repr->ast lisb)) (EOF.)))

#_(defn abc [partial-constructor node & keys]
  (apply partial-constructor (map node-repr->ast (map #(% node) keys))))

#_(defn lisb->ast [lisb]
  (println "")
  (println lisb)
  (println (map? lisb))
  (println (seq? lisb))
  (println (empty? lisb))
  (if (map? lisb)
    (do
      (println "Map")
      (process-node lisb))
    (if (seq? lisb)
      (if (empty? lisb)
        (do
          (println "Empty seq")
          '())
        (do
          (println "Not empty seq")
          (map node-repr->ast lisb)))
      (do
        (println "Literal")
        (literal lisb)))))

#_(require '[lisb.ast2lisb :refer :all])
#_(node-repr->ast (bmachinestr->lisb "MACHINE Empty\nEND"))

#_(defn get-machine-from-ast [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (.getPrettyPrint pprinter)))
#_(get-machine-from-ast (node-repr->ast (bmachinestr->lisb "MACHINE Empty\nEND")))

#_(.getEOF (.parse (BParser.) "MACHINE Empty\nEND" false))