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
             AStringSetExpression AConstantsMachineClause APropertiesMachineClause AConstraintsMachineClause ASetsMachineClause AConcreteVariablesMachineClause AAssertionsMachineClause AOperationsMachineClause ASkipSubstitution ABecomesElementOfSubstitution ATotalRelationExpression ANotSubsetPredicate ANotSubsetStrictPredicate ASurjectionRelationExpression ATotalSurjectionRelationExpression ASizeExpression AEnumeratedSetSet ADeferredSetSet)
           (de.be4.classicalb.core.parser BParser)))

(declare ast->lisb read-bmachine)

(defn ast-list->lisb [nodes args]
  (map #(ast->lisb % args) nodes))

(defn left-right [lisb-representation node args]
  (lisb-representation (ast->lisb (.getLeft node) args)
                       (ast->lisb (.getRight node) args)))

(defn collect-left-associative [node]
  (let [left-node (.getLeft node)
        right-node (.getRight node)]
    (if (= (class node) (class left-node))
      (conj (collect-left-associative (.getLeft left-node)) right-node)
      [left-node right-node])))


(defn multi-arity [lisb-representation node args]
  (apply lisb-representation (ast-list->lisb (collect-left-associative node) args)))

;;; ast-> lisb start

(defmulti ast->lisb (fn [node args] (class node)))

(defmethod ast->lisb Start [node args] (ast->lisb (.getPParseUnit node) args))

(defmethod ast->lisb AAbstractMachineParseUnit [node args]
  (apply (partial bmachine
                  (ast->lisb (.getVariant node) args)
                  (ast->lisb (.getHeader node) args))
         (ast-list->lisb (.getMachineClauses node) args)))

(defmethod ast->lisb AMachineMachineVariant [node args]
  (bmachine-variant))

(defmethod ast->lisb AMachineHeader [node args]
  (bmachine-header
    (ast->lisb (first (.getName node)) args)                   ; There should be exact one identifier as name!
    (ast-list->lisb (.getParameters node) args)))

;;; machine clauses

(defmethod ast->lisb AConstraintsMachineClause [node args]
  (apply bcontraints (ast->lisb (.getPredicates node) args)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb ASetsMachineClause [node args]
  (apply bsets (ast-list->lisb (.getSetDefinitions node) args)))
(defmethod ast->lisb ADeferredSetSet [node args]
  (bdeferred-set (ast->lisb (first (.getIdentifier node)) args))) ; there should be exact one identifier
(defmethod ast->lisb AEnumeratedSetSet [node args]
  (benumerated-set (ast->lisb (first (.getIdentifier node)) args) (into #{} (ast-list->lisb (.getElements node) args)))) ; there should be exact one identifier

(defmethod ast->lisb AConstantsMachineClause [node args]
  (apply bconstants (ast-list->lisb (.getIdentifiers node) args)))

; TODO: concrete constants

(defmethod ast->lisb APropertiesMachineClause [node args]
  (bproperties (ast->lisb (.getPredicates node) args)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb ADefinitionsMachineClause [node args]
  (apply bdefinitions (ast-list->lisb (.getDefinitions node) args)))

(defmethod ast->lisb AVariablesMachineClause [node args]
  (apply bvariables (ast-list->lisb (.getIdentifiers node) args)))

; TODO: concrete variables

(defmethod ast->lisb AInvariantMachineClause [node args]
  (binvariants (ast->lisb (.getPredicates node) args)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb AAssertionsMachineClause [node args]
  (apply bassertions (ast-list->lisb (.getPredicates node) args)))

(defmethod ast->lisb AInitialisationMachineClause [node args]
  (binit (ast->lisb (.getSubstitutions node) args)))        ; AInitialisationMachineClause holds one PSubstitution

(defmethod ast->lisb AOperationsMachineClause [node args]
  (apply boperations (ast-list->lisb (.getOperations node) args)))

;;; substitutions

(defmethod ast->lisb ASkipSubstitution [node args]
  (bskip))

(defmethod ast->lisb AAssignSubstitution [node args]
  (bassign (ast-list->lisb (.getLhsExpression node) args) (ast-list->lisb (.getRhsExpressions node) args)))

; functional override

(defmethod ast->lisb ABecomesElementOfSubstitution [node args])

; choice by predicate (defmethod ast->lisb [node args])

; <-- call operation (defmethod ast->lisb [node args])

; parallel substitution (defmethod ast->lisb [node args])

; not determistic choice (defmethod ast->lisb [node args])

; let? (defmethod ast->lisb [node args])

; pre (defmethod ast->lisb [node args])

; assert (defmethod ast->lisb [node args])

; choice

; TODO: BlockSubstitution müsste entfernt werden können
(defmethod ast->lisb ABlockSubstitution [node args]
  (bblock (ast->lisb (.getSubstitution node) args)))        ; ABlockSubstitution holds one PSubstitution


;;; if-then-else
(defmethod ast->lisb AIfThenElseExpression [node args])
;;; let
(defmethod ast->lisb ALetExpressionExpression [node args])

;;; trees

;;; reals - (alpha - besser nicht verwenden)

;;; strings
(defmethod ast->lisb AStringExpression [node _]
  (.getText (.getContent node)))

(defmethod ast->lisb AStringSetExpression [_ _]
  (bstring-set))

;;; records
(defmethod ast->lisb ARecordFieldExpression [node args]
  (brec-get
    (ast->lisb (.getRecord node) args)
    (ast->lisb (.getIdentifier node) args)))

(defmethod ast->lisb AStructExpression [node args]
  #_(bstruct
      (set (map (fn [recentry] [(AST->lisb (.getIdentifier recentry) args)
                                (AST->lisb (.getValue recentry) args)]) (.getEntries node)))))


;;; sequences
(defmethod ast->lisb AEmptySequenceExpression [_ _]
  (bsequence))
(defmethod ast->lisb ASequenceExtensionExpression [node args]
  (apply bsequence (ast-list->lisb (.getExpression node) args)))
(defmethod ast->lisb ASeqExpression [node args]
  (bseq (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ASeq1Expression [node args]
  (bseq1 (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AIseqExpression [node args]
  (biseq (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AIseq1Expression [node args]
  (biseq1 (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb APermExpression [node args]
  (bperm (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ASizeExpression [node args]
  ;TODO
  )
(defmethod ast->lisb AConcatExpression [node args]
  (multi-arity bconcat node args))
(defmethod ast->lisb AInsertFrontExpression [node args]
  (left-right b-> node args))
(defmethod ast->lisb AInsertTailExpression [node args]
  (left-right b<- node args))
(defmethod ast->lisb ARevExpression [node args]
  (breverse (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AFirstExpression [node args]
  (bfirst (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ALastExpression [node args]
  (blast (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AFrontExpression [node args]
  (bfront (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ATailExpression [node args]
  (btail (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AGeneralConcatExpression [node args]
  (bconc (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ARestrictFrontExpression [node args]
  (left-right brestrict-front node args))
(defmethod ast->lisb ARestrictTailExpression [node args]
  (left-right brestrict-tail node args))

;;; functions
(defmethod ast->lisb APartialFunctionExpression [node args]
  (left-right b+-> node args))
(defmethod ast->lisb ATotalFunctionExpression [node args]
  (left-right b--> node args))
(defmethod ast->lisb APartialSurjectionExpression [node args]
  (left-right b+->> node args))
(defmethod ast->lisb ATotalSurjectionExpression [node args]
  (left-right b-->> node args))
(defmethod ast->lisb APartialInjectionExpression [node args]
  (left-right b>+> node args))
(defmethod ast->lisb ATotalInjectionExpression [node args]
  (left-right b>-> node args))
(defmethod ast->lisb APartialBijectionExpression [node args]
  (left-right b>+>> node args))
(defmethod ast->lisb ATotalBijectionExpression [node args]
  (left-right b>->> node args))
; lambda abstraction
(defmethod ast->lisb AFunctionExpression [node args]
  (apply (partial bapply (ast->lisb (.getIdentifier node) args)) (ast-list->lisb (.getParameters node) args)))

;;; relations
(defmethod ast->lisb ARelationsExpression [node args]
  (left-right b<-> node args))
(defmethod ast->lisb ATotalRelationExpression [node args]
  (left-right btotal-relation node args))
(defmethod ast->lisb ASurjectionRelationExpression [node args]
  (left-right bsurjective-relation node args))
(defmethod ast->lisb ATotalSurjectionRelationExpression [node args]
  (left-right btotal-surjective-relation node args))
(defmethod ast->lisb ACoupleExpression [node args]
  (apply bcouple (ast-list->lisb (.getList node) args)))
(defmethod ast->lisb ADomainExpression [node args]
  (bdom (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ARangeExpression [node args]
  (bran (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AIdentityExpression [node args]
  (bid (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ADomainRestrictionExpression [node args]
  (left-right b<| node args))
(defmethod ast->lisb ADomainSubtractionExpression [node args]
  (left-right b<<| node args))
(defmethod ast->lisb ARangeRestrictionExpression [node args]
  (left-right b|> node args))
(defmethod ast->lisb ARangeSubtractionExpression [node args]
  (left-right b|>> node args))
(defmethod ast->lisb AReverseExpression [node args]
  (binverse (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AImageExpression [node args]
  (left-right bimage node args))
(defmethod ast->lisb AOverwriteExpression [node args]
  (multi-arity b<+ node args))
(defmethod ast->lisb ADirectProductExpression [node args]
  (multi-arity b>< node args))
(defmethod ast->lisb ACompositionExpression [node args]
  (multi-arity bcomp node args))
(defmethod ast->lisb AParallelProductExpression [node args]
  (multi-arity b|| node args))
(defmethod ast->lisb AFirstProjectionExpression [node args]
  (bprj1 (ast->lisb (.getExp1 node) args) (ast->lisb (.getExp2 node) args)))
(defmethod ast->lisb ASecondProjectionExpression [node args]
  (bprj2 (ast->lisb (.getExp1 node) args) (ast->lisb (.getExp2 node) args)))
(defmethod ast->lisb AClosureExpression [node args]
  (bclosure1 (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AReflexiveClosureExpression [node args]
  (bclosure (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AIterationExpression [node args]
  (left-right biterate node args))
(defmethod ast->lisb ATransFunctionExpression [node args]
  (bfnc (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ATransRelationExpression [node args]
  (brel (ast->lisb (.getExpression node) args)))

;;; numbers
(defmethod ast->lisb AIntegerSetExpression [_ _] (binteger-set))
(defmethod ast->lisb ANaturalSetExpression [_ _] (bnatural-set))
(defmethod ast->lisb ANatural1SetExpression [_ _] (bnatural1-set))
(defmethod ast->lisb AIntSetExpression [_ _] (bint-set))
(defmethod ast->lisb ANatSetExpression [_ _] (bnat-set))
(defmethod ast->lisb ANat1SetExpression [_ _] (bnat1-set))
(defmethod ast->lisb AIntervalExpression [node args]
  (binterval
    (ast->lisb (.getLeftBorder node) args)
    (ast->lisb (.getRightBorder node) args)))
; MININT
; MAXINT
(defmethod ast->lisb ALessPredicate [node args]
  (multi-arity b< node args))
(defmethod ast->lisb AGreaterPredicate [node args]
  (multi-arity b> node args))
(defmethod ast->lisb ALessEqualPredicate [node args]
  (multi-arity b<= node args))
(defmethod ast->lisb AGreaterEqualPredicate [node args]
  (multi-arity b>= node args))
(defmethod ast->lisb AMaxExpression [node args]
  (bmax (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AMinExpression [node args]
  (bmin (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AAddExpression [node args]
  (multi-arity b+ node args))
(defmethod ast->lisb AMinusOrSetSubtractExpression [node args]
  (multi-arity b- node args))
(defmethod ast->lisb AMultOrCartExpression [node args]
  (multi-arity b* node args))
(defmethod ast->lisb ADivExpression [node args]
  (multi-arity bdiv node args))
(defmethod ast->lisb APowerOfExpression [node args]
  (multi-arity b** node args))
(defmethod ast->lisb AModuloExpression [node args]
  (multi-arity bmod node args))
; set product
; set summation
; succ
; pred
; 0xH


;;; sets
(defmethod ast->lisb AEmptySetExpression [node args]
  #{})
(defmethod ast->lisb ASetExtensionExpression [node args]
  (into #{} (ast-list->lisb (.getExpressions node) args))
  #_(apply bset-enum (ast-list->lisb (.getExpressions node) args)))
(defmethod ast->lisb AComprehensionSetExpression [node args]
  (bcomp-set
    (ast-list->lisb (.getIdentifiers node) args)
    (ast->lisb (.getPredicates node) args)))
(defmethod ast->lisb APowSubsetExpression [node args]
  (bpow (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb APow1SubsetExpression [node args]
  (bpow1 (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AFinSubsetExpression [node args]
  (bfin (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AFin1SubsetExpression [node args]
  (bfin1 (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb ACardExpression [node args]
  (bcount (ast->lisb (.getExpression node) args)))
(defmethod ast->lisb AUnionExpression [node args]
  (left-right bunion node args))
(defmethod ast->lisb AIntersectionExpression [node args]
  (left-right bintersection node args))
(defmethod ast->lisb ASetSubtractionExpression [node args]
  (multi-arity bset- node args))
(defmethod ast->lisb AMemberPredicate [node args]
  (left-right bmember node args))
(defmethod ast->lisb ANotMemberPredicate [node args]
  (left-right bnot-member node args))
(defmethod ast->lisb ASubsetPredicate [node args]
  (left-right bsubset node args))
(defmethod ast->lisb ANotSubsetPredicate [node args]
  (left-right bnot-subset node args))
(defmethod ast->lisb ASubsetStrictPredicate [node args]
  (left-right bsubset-strict node args))
(defmethod ast->lisb ANotSubsetStrictPredicate [node args]
  (left-right bnot-subset-strict node args))

;;; booleans

(defmethod ast->lisb ABooleanTrueExpression [_ _]
  true)

(defmethod ast->lisb ABooleanFalseExpression [_ _]
  false)

(defmethod ast->lisb ABoolSetExpression [_ _]
  (bbool-set))

(defmethod ast->lisb AConvertBoolExpression [node args]
  (bpred->bool (ast->lisb (.getPredicate node) args)))

;;; equality predicates

(defmethod ast->lisb AEqualPredicate [node args]
  (b= (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args)))

(defmethod ast->lisb ANotEqualPredicate [node args]
  (bnot= (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args))
  )

;;; logical predicates

(defmethod ast->lisb AConjunctPredicate [node args]
  (band (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args)))

(defmethod ast->lisb ADisjunctPredicate [node args]
  (bor (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args)))

(defmethod ast->lisb AImplicationPredicate [node args]
  (b=> (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args)))

(defmethod ast->lisb AEquivalencePredicate [node args]
  (b<=> (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args)))

(defmethod ast->lisb ANegationPredicate [node args]
  (bnot (ast->lisb (.getPredicate node) args)))

(defmethod ast->lisb AForallPredicate [node args]
  (bforall
    (ast-list->lisb (.getIdentifiers node) args)
    (ast->lisb (.getImplication node) args)))

(defmethod ast->lisb AExistsPredicate [node args]
  (bexists
    (ast-list->lisb (.getIdentifiers node) args)
    (ast->lisb (.getPredicate node) args)))

;;; categorize

(defmethod ast->lisb AIdentifierExpression [node args]
  (ast->lisb (first (.getIdentifier node)) args))           ; Es sollte exakt nur ein Identifier in einer Identifier Expression sein

(defmethod ast->lisb AIntegerExpression [node args]
  (Long/parseLong (.getText (.getLiteral node))))

(defmethod ast->lisb ANatSetExpression [node args]
  (bnat-set))

(defmethod ast->lisb TIdentifierLiteral [node args]
  (keyword (.getText node)))

; until now only used for formulas
(defmethod ast->lisb APredicateParseUnit [node args]
  (ast->lisb (.getPredicate node) args))

; until now only used for formulas
(defmethod ast->lisb AExpressionParseUnit [node args]
  (ast->lisb (.getExpression node) args))

;;; ast->lisb end

(defn b-ast->lisb [b-ast->lisb] (ast->lisb b-ast->lisb {:symbols {}}))
(defn b->lisb [b] (b-ast->lisb (.parse (BParser.) b false)))
(defn b-formula->lisb [b-formula] (b->lisb (str (BParser/FORMULA_PREFIX) b-formula)))
(defn b-expression->lisb [b-expression] (b->lisb (str (BParser/EXPRESSION_PREFIX) b-expression)))
(defn b-substitution->lisb [b-substitution] (b->lisb (str (BParser/SUBSTITUTION_PREFIX) b-substitution)))
(defn b-transition->lisb [b-transition] (b->lisb (str "#TRANSITION " b-transition)))
(defn b-predicate->lisb [b-predicate] (b->lisb (str (BParser/PREDICATE_PREFIX) b-predicate)))
(defn b-operation->lisb [b-operation] (b->lisb (str (BParser/OPERATION_PATTERN_PREFIX) b-operation)))