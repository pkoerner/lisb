(ns lisb.ast2lisb
  (:require [lisb.representation :refer :all])
  (:import (de.be4.classicalb.core.parser.node
             Start
             AAbstractMachineParseUnit
             AMachineMachineVariant
             AMachineHeader
             AVariablesMachineClause
             AInvariantMachineClause
             AInitialisationMachineClause
             ABlockSubstitution
             AAssignSubstitution
             AMemberPredicate
             AIntegerExpression
             ANatSetExpression
             AAddExpression
                                               AMinusExpression
                                               AMultOrCartExpression
                                               AMinusOrSetSubtractExpression
                                               ADivExpression
                                               AUnaryMinusExpression
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
                                               ANotMemberPredicate
                                               ASubsetPredicate
                                               ASubsetStrictPredicate
                                               ABoolSetExpression
                                               ANaturalSetExpression
                                               ANatural1SetExpression
                                               AIntegerSetExpression
                                               AIntSetExpression
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
                                               TStringLiteral
                                               ADefinitionExpression
                                               ALetExpressionExpression
                                               ALetPredicatePredicate
                                               APredicateParseUnit
                                               AExpressionParseUnit
                                               ADefinitionPredicate
                                               ADefinitionsMachineClause
                                               ADefinitionFileParseUnit
                                               APredicateDefinitionDefinition
                                               AExpressionDefinitionDefinition
                                               AFileDefinitionDefinition
                                               AStringSetExpression)
           (de.be4.classicalb.core.parser BParser)))

(declare ast->lisb read-bmachine)

(defn process-list [nodes args]
  (map #(ast->lisb % args) nodes))

(defmulti ast->lisb (fn [node args] (class node)))

(defmethod ast->lisb Start [node args] (ast->lisb (.getPParseUnit node) args))

(defmethod ast->lisb AAbstractMachineParseUnit [node args]
  (apply (partial bmachine
           (ast->lisb (.getVariant node) args)
           (ast->lisb (.getHeader node) args))
         (process-list (.getMachineClauses node) args)))

(defmethod ast->lisb AMachineMachineVariant [node args]
  (bmachine-variant))

(defmethod ast->lisb AMachineHeader [node args]
  (bmachine-header
    (process-list (.getName node) args)
    (process-list (.getParameters node) args)))

(defmethod ast->lisb AVariablesMachineClause [node args]
  (apply bvariables (process-list (.getIdentifiers node) args)))

(defmethod ast->lisb AInvariantMachineClause [node args]
  (binvariants (ast->lisb (.getPredicates node) args)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb AInitialisationMachineClause [node args]
  (binit (ast->lisb (.getSubstitutions node) args)))        ; AInitialisationMachineClause holds one PSubstitution

; TODO: BlockSubstition müsste entfernt werden können
(defmethod ast->lisb ABlockSubstitution [node args]
  (bblock (ast->lisb (.getSubstitution node) args)))        ; ABlockSubstition holds one PSubstitution

(defmethod ast->lisb AAssignSubstitution [node args]
  (bassign (process-list (.getLhsExpression node) args) (process-list (.getRhsExpressions node) args)))

(defmethod ast->lisb AMemberPredicate [node args]
  (bmember (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args)))

(defmethod ast->lisb AIdentifierExpression [node args]
  (apply bidentifiers (process-list (.getIdentifier node) args)))

(defmethod ast->lisb AIntegerExpression [node args]
  (Long/parseLong (.getText (.getLiteral node))))

(defmethod ast->lisb ANatSetExpression [node args]
  (bnat-set))

(defmethod ast->lisb TIdentifierLiteral [node args]
  (keyword (.getText node)))

(defn bmachine->lisb [bmachine-path]
  (ast->lisb (.parseFile (BParser.) (clojure.java.io/file bmachine-path) false) {:symbols {}}))

(defn bmachinestr->lisb [bmachinestr]
  (ast->lisb (.parse (BParser.) bmachinestr false) {:symbols {}}))
