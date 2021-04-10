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
                                               ALetPredicatePredicate AConstantsMachineClause APropertiesMachineClause)
           (de.be4.classicalb.core.parser.util PrettyPrinter)
           (de.be4.classicalb.core.parser BParser)))

(defn identifier [n]
  (AIdentifierExpression. [(TIdentifierLiteral. (name n))]))

;;; TODO: this smells like it could be done nicer
(defn set-enum [& args]
  (if-not (seq args)
    (AEmptySetExpression.)
    (ASetExtensionExpression. args)))

(declare lisb->ast)

(defmulti process-node (fn [node] (:tag node)))

(defmethod process-node :machine [node]
  (Start. (AAbstractMachineParseUnit. (lisb->ast (:variant node)) (lisb->ast (:header node)) (map lisb->ast (:clauses node))) (EOF.)))

(defmethod process-node :machine-variant [node]
  (AMachineMachineVariant.))

(defmethod process-node :machine-header [node]
  (AMachineHeader. (.getIdentifier (lisb->ast (:name node))) (map lisb->ast (:parameters node))))

(defmethod process-node :constants [node]
  (AConstantsMachineClause. (map lisb->ast (:children node))))

(defmethod process-node :properties [node]
  (APropertiesMachineClause. (lisb->ast (:predicate node))))

(defmethod process-node :variables [node]
  (AVariablesMachineClause. (map lisb->ast (:children node))))

(defmethod process-node :invariants [node]
  (AInvariantMachineClause. (lisb->ast (:predicate node))))

(defmethod process-node :init [node]
  (AInitialisationMachineClause. (lisb->ast (:children node))))

(defmethod process-node :block [node]
  (ABlockSubstitution. (lisb->ast (:p-substitution node))))

(defmethod process-node :assign [node]
  (AAssignSubstitution. (map lisb->ast (:lhs-exprs node)) (map lisb->ast (:rhs-exprs node))))

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
        ;(true? x) (boolean-true)
        ;(false? x) (boolean-false)
        (set? x) (apply set-enum (map lisb->ast x))
        ;(sequential? x) (apply tuple-node (map lisb->ast x))
        :otherwise (println :unhandled-literal x)

        ))


(defn lisb->ast [lisb]
  (cond
    (map? lisb) (process-node lisb)
    (seq? lisb) (map lisb->ast lisb)
    :else (literal lisb)))

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