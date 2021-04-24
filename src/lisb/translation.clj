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
                                               ALetPredicatePredicate AConstantsMachineClause APropertiesMachineClause AConstraintsMachineClause ASetsMachineClause ADeferredSetSet AEnumeratedSetSet ADefinitionsMachineClause AAssertionsMachineClause AOperationsMachineClause ASkipSubstitution ABecomesElementOfSubstitution AOperationCallSubstitution AAnySubstitution)
           (de.be4.classicalb.core.parser.util PrettyPrinter)
           (de.be4.classicalb.core.parser BParser)))

(defn chain [tag tuples]
  (reduce (partial node :and) (map (partial apply node tag) tuples)))

(defn chain-arity-two [tag nodes]
  (chain tag (partition 2 1 nodes)))


(defn identifier [n]
  (AIdentifierExpression. [(TIdentifierLiteral. (name n))]))

;;; TODO: this smells like it could be done nicer
(defn set-enum [& args]
  (if-not (seq args)
    (AEmptySetExpression.)
    (ASetExtensionExpression. args)))

#_(declare abc)
(declare lisb->ast)

(defmulti process-node (fn [node] (:tag node)))

;;; parse units

(defmethod process-node :machine [node]
  (Start. (AAbstractMachineParseUnit. (lisb->ast (:variant node)) (lisb->ast (:header node)) (map lisb->ast (:clauses node))) (EOF.)))
(defmethod process-node :machine-variant [node]
  (AMachineMachineVariant.))
(defmethod process-node :machine-header [node]
  (AMachineHeader. (.getIdentifier (lisb->ast (:name node))) (map lisb->ast (:parameters node))))


;;; machine clauses

(defmethod process-node :contraints [node]
  (AConstraintsMachineClause. (lisb->ast (:predicate node))))

(defmethod process-node :sets [node]
  (ASetsMachineClause. (map lisb->ast (:set-definitions node))))
(defmethod process-node :deferred-set [node]
  (ADeferredSetSet. (lisb->ast (:identifier node))))
(defmethod process-node :enumerated-set [node]
  (AEnumeratedSetSet. (lisb->ast (:identifier node)) (map lisb->ast (:elements node))))

(defmethod process-node :constants [node]
  (AConstantsMachineClause. (map lisb->ast (:identifiers node))))

(defmethod process-node :properties [node]
  (APropertiesMachineClause. (lisb->ast (:predicate node))))

(defmethod process-node :definitions [node]
  (ADefinitionsMachineClause. (map lisb->ast (:definitions node))))

(defmethod process-node :variables [node]
  (AVariablesMachineClause. (map lisb->ast (:identifiers node))))

(defmethod process-node :invariants [node]
  (AInvariantMachineClause. (lisb->ast (:predicate node))))

(defmethod process-node :assign [node]
  (AAssertionsMachineClause. (map lisb->ast (:predicates node))))

(defmethod process-node :init [node]
  (AInitialisationMachineClause. (lisb->ast (:substitution node))))

(defmethod process-node :operations [node]
  (AOperationsMachineClause. (map lisb->ast (:operations node))))


;;; substitutions

(defmethod process-node :skip [_]
  (ASkipSubstitution.))

(defmethod process-node :block [node]
  (ABlockSubstitution. (lisb->ast (:p-substitution node))))

(defmethod process-node :assign [node]
  (AAssignSubstitution. (map lisb->ast (:lhs-exprs node)) (map lisb->ast (:rhs-exprs node))))

(defmethod process-node :becomes-element-of [node]
  (ABecomesElementOfSubstitution. (map lisb->ast (:identifiers node)) (lisb->ast (:set node))))

(defmethod process-node :operation-call [node]
  (AOperationCallSubstitution. (map lisb->ast (:identifiers node)) (lisb->ast (:operation node)) (lisb->ast (:parameters node))))

(defmethod process-node :any [node]
  (AAnySubstitution. (map lisb->ast (:identifiers node)) (lisb->ast (:where node)) (lisb->ast (:then node))))

(defmethod process-node :equal [node]
  (AEqualPredicate. (lisb->ast (:left node)) (lisb->ast (:right node))))

(defmethod process-node :member [node]
  (AMemberPredicate. (lisb->ast (:left node)) (lisb->ast (:right node))))

(defmethod process-node :identifiers [node]
  (AIdentifierExpression. (map lisb->ast (:children node))))

(defmethod process-node :nat-set [node]
  (ANatSetExpression.))

(defmethod process-node :default [node]
  (println node))

(defn literal [x]
  (cond (keyword? x) (identifier x)
        (string? x) x ;; hack-y thing to avoid renaming
        ;; of rec-get parameters in preds
        (number? x) (AIntegerExpression. (TIntegerLiteral. (str x)))
        (true? x) (ABooleanTrueExpression.)
        (false? x) (ABooleanFalseExpression.)
        (set? x) (apply set-enum (map lisb->ast x))
        ;(sequential? x) (apply tuple-node (map lisb->ast x))
        :otherwise (println :unhandled-literal x)

        ))

(defn lisb->ast [lisb]
  (println lisb)
  (cond
    (map? lisb) (process-node lisb)
    (seq? lisb) (map lisb->ast lisb)
    :else (literal lisb)))

#_(defn abc [partial-constructor node & keys]
  (apply partial-constructor (map lisb->ast (map #(% node) keys))))

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
          (map lisb->ast lisb)))
      (do
        (println "Literal")
        (literal lisb)))))

#_(require '[lisb.ast2lisb :refer :all])
#_(lisb->ast (bmachinestr->lisb "MACHINE Empty\nEND"))

#_(defn get-machine-from-ast [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (.getPrettyPrint pprinter)))
#_(get-machine-from-ast (lisb->ast (bmachinestr->lisb "MACHINE Empty\nEND")))

#_(.getEOF (.parse (BParser.) "MACHINE Empty\nEND" false))