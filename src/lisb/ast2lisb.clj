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
             AStringSetExpression AConstantsMachineClause APropertiesMachineClause AConstraintsMachineClause ASetsMachineClause AConcreteVariablesMachineClause AAssertionsMachineClause AOperationsMachineClause ASkipSubstitution ABecomesElementOfSubstitution ATotalRelationExpression ANotSubsetPredicate ANotSubsetStrictPredicate ASurjectionRelationExpression ATotalSurjectionRelationExpression ASizeExpression AEnumeratedSetSet ADeferredSetSet ASubstitutionParseUnit ABecomesSuchSubstitution AOperationCallSubstitution AParallelSubstitution ASequenceSubstitution AAnySubstitution ALetSubstitution AVarSubstitution APreconditionSubstitution AAssertionSubstitution AIfSubstitution AIfElsifSubstitution ASuccessorExpression APredecessorExpression AMinIntExpression AMaxIntExpression AChoiceSubstitution AChoiceOrSubstitution ASelectSubstitution ASelectWhenSubstitution AOperation)
           (de.be4.classicalb.core.parser BParser)
           (clojure.lang PersistentList PersistentVector)
           (java.util LinkedList)))

(declare ast->lisb read-bmachine)

(defn ast-list->lisb [nodes args]
  (map #(ast->lisb % args) nodes))

(defn xyz [lisb args & nodes]
  (conj (map #(ast->lisb % args) nodes) lisb)
  #_(apply lisb (map #(ast->lisb % args) nodes)))

(defn left-right [lisb node args]
  (xyz lisb args (.getLeft node) (.getRight node))
  #_(list lisb (ast->lisb (.getLeft node) args) (ast->lisb (.getRight node) args)))

(defn expression [lisb node args]
  (xyz lisb args (.getExpression node)))

(defn collect-left-associative [node]
  (let [left-node (.getLeft node)
        right-node (.getRight node)]
    (if (= (class node) (class left-node))
      (conj (collect-left-associative left-node) right-node)
      [left-node right-node])))


(defn multi-arity [lisb node args]
  (apply (partial xyz lisb args) (collect-left-associative node))
  #_(apply lisb-representation (ast-list->lisb (collect-left-associative node) args)))

;;; ast-> lisb start

(defmulti ast->lisb (fn [node _args] (class node)))

(defmethod ast->lisb Start [node args] (ast->lisb (.getPParseUnit node) args))


;;; parse units

(defmethod ast->lisb AAbstractMachineParseUnit [node args]
  (conj (ast-list->lisb (.getMachineClauses node) args) (ast->lisb (.getHeader node) args) (ast->lisb (.getVariant node) args) 'machine))
(defmethod ast->lisb AMachineMachineVariant [_ _]
  '(machine-variant))
(defmethod ast->lisb AMachineHeader [node args]
  (xyz 'machine-header args (first (.getName node)) (.getParameters node)))  ; there should be exact one identifier as name!)

(defmethod ast->lisb APredicateParseUnit [node args]
  (ast->lisb (.getPredicate node) args))

(defmethod ast->lisb AExpressionParseUnit [node args]
  (ast->lisb (.getExpression node) args))

(defmethod ast->lisb ASubstitutionParseUnit [node args]
  (ast->lisb (.getSubstitution node) args))


;;; machine clauses

(defmethod ast->lisb AConstraintsMachineClause [node args]
  (xyz 'contraints args (.getPredicates node)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb ASetsMachineClause [node args]
  (conj (ast-list->lisb (.getSetDefinitions node) args) 'sets))
(defmethod ast->lisb ADeferredSetSet [node args]
  (xyz 'deferred-set args (first (.getIdentifier node)))) ; there should be exact one identifier
(defmethod ast->lisb AEnumeratedSetSet [node args]
  (list 'enumerated-set (ast->lisb (first (.getIdentifier node)) args) (into #{} (ast-list->lisb (.getElements node) args)))) ; there should be exact one identifier

(defmethod ast->lisb AConstantsMachineClause [node args]
  (conj (ast-list->lisb (.getIdentifiers node) args) 'constants))

; TODO: concrete constants

(defmethod ast->lisb APropertiesMachineClause [node args]
  (xyz 'properties args (.getPredicates node)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb ADefinitionsMachineClause [node args]
  (conj (ast-list->lisb (.getDefinitions node) args) 'definitions))

(defmethod ast->lisb AVariablesMachineClause [node args]
  (conj (ast-list->lisb (.getIdentifiers node) args) 'variables))

; TODO: concrete variables

(defmethod ast->lisb AInvariantMachineClause [node args]
  (xyz 'invariants args (.getPredicates node)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb AAssertionsMachineClause [node args]
  (conj (ast-list->lisb (.getPredicates node) args) 'assertions))

(defmethod ast->lisb AInitialisationMachineClause [node args]
  (xyz 'init args (.getSubstitutions node)))        ; AInitialisationMachineClause holds one PSubstitution

(defmethod ast->lisb AOperationsMachineClause [node args]
  (conj (ast-list->lisb (.getOperations node) args) 'operations))
(defmethod ast->lisb AOperation [node args]
  (xyz 'operation args
       (.getReturnValues node)                              ; TODO: test if multiple return values are possible
       (first (.getOpName node))                            ; there should be exact one identifier
       (.getParameters node)
       (.getOperationBody node)))


;;; substitutions

(defmethod ast->lisb ASkipSubstitution [_ _]
  '(skip))

(defmethod ast->lisb AAssignSubstitution [node args]
  (let [left (ast-list->lisb (.getLhsExpression node) args)
        right (ast-list->lisb (.getRhsExpressions node) args)]
    (conj (interleave left right) 'assign)))

; TODO: functional override

(defmethod ast->lisb ABecomesElementOfSubstitution [node args]
  (xyz 'becomes-element-of args (.getIdentifiers node) (.getSet node)))

(defmethod ast->lisb ABecomesSuchSubstitution [node args]
  (xyz 'becomes-such args (.getIdentifiers node) (.getPredicate node)))

(defmethod ast->lisb AOperationCallSubstitution [node args]
  (xyz 'operation-call args (.getResultIdentifiers node) (first (.getOperation node)) (.getParameters node))) ; there should be exact one identifier in .getOperation

(defmethod ast->lisb AParallelSubstitution [node args]
  (conj (ast->lisb (.getSubstitutions node) args) 'parallel-substitution ))

(defmethod ast->lisb ASequenceSubstitution [node args]
  (conj (ast->lisb (.getSubstitutions node) args) 'sequence-substitution))

(defmethod ast->lisb AAnySubstitution [node args]
  (xyz 'any args (.getIdentifiers node) (.getWhere node) (.getThen node)))

(defmethod ast->lisb ALetSubstitution [node args]
  (xyz 'let-sub args (.getIdentifiers node) (.getPredicate node) (.getSubstitution node)))

(defmethod ast->lisb AVarSubstitution [node args]
  (xyz 'var args (.getIdentifiers node) (.getSubstitution node)))

(defmethod ast->lisb APreconditionSubstitution [node args]
  (xyz 'pre args (.getPredicate node) (.getSubstitution node)))

(defmethod ast->lisb AAssertionSubstitution [node args]
  (xyz 'assert args (.getPredicate node) (.getSubstitution node)))

(defmethod ast->lisb AChoiceSubstitution [node args]
  (conj (ast->lisb (.getSubstitutions node) args) 'choice))
(defmethod ast->lisb AChoiceOrSubstitution [node args]
  (ast->lisb (.getSubstitution node) args))

(defmethod ast->lisb AIfSubstitution [node args]
  (let [condition (ast->lisb (.getCondition node) args)
        then (ast->lisb (.getThen node) args)
        else-ifs (ast-list->lisb (.getElsifSubstitutions node) args)
        else (ast->lisb (.getElse node) args)]
    (if (empty? else-ifs)
      (if else
        (list 'if-sub condition then else)
        (list 'if-sub condition then))
      (list
        'if-sub
        condition
        then
        (reduce
          (fn [acc [condition then]]
            (if acc
              (list 'if-sub condition then acc)
              (list 'if-sub condition then)))
          else
          else-ifs)))))
(defmethod ast->lisb AIfElsifSubstitution [node args]
  [(ast->lisb (.getCondition node) args) (ast->lisb (.getThenSubstitution node) args)])

(defmethod ast->lisb ASelectSubstitution [node args]
  (let [condition (ast->lisb (.getCondition node) args)
        then (ast->lisb (.getThen node) args)
        else-ifs (ast-list->lisb (.getWhenSubstitutions node) args)
        else (ast->lisb (.getElse node) args)]
    (if (empty? else-ifs)
      (if else
        (list 'select condition then else)
        (list 'select condition then))
      (list 'select
        condition
        then
        (reduce
          (fn [acc [condition then]]
            (if acc
              (list 'select condition then acc)
              (list 'select condition then)))
          else
          else-ifs)))))
(defmethod ast->lisb ASelectWhenSubstitution [node args]
  [(ast->lisb (.getCondition node) args) (ast->lisb (.getWhenSubstitution node) args)])

; TODO: case

; BlockSubstitution müsste entfernt werden können
(defmethod ast->lisb ABlockSubstitution [node args]
  (xyz 'block args (.getSubstitution node)))        ; ABlockSubstitution holds one PSubstitution


;;; if-then-else

(defmethod ast->lisb AIfThenElseExpression [node args]
  (xyz 'if-expr args (.getCondition node) (.getThen node) (.getElse node)))


;;; let

(defmethod ast->lisb ALetExpressionExpression [node args]
  (xyz 'let-expr (.getIdentifiers node) (.getAssignment node) (.getExpr node)))
(defmethod ast->lisb ALetPredicatePredicate [node args]
  (xyz 'let-pred (.getIdentifiers node) (.getAssignment node) (.getPred node)))


;;; trees


;;; reals - (alpha - besser nicht verwenden)


;;; strings

(defmethod ast->lisb AStringExpression [node _]
  (.getText (.getContent node)))

(defmethod ast->lisb AStringSetExpression [_ _]
  (list 'string-set))


;;; records

(defmethod ast->lisb ARecordFieldExpression [node args]
  (brec-get
    (ast->lisb (.getRecord node) args)
    (ast->lisb (.getIdentifier node) args)))

(defmethod ast->lisb AStructExpression [node args]
  (let [entries (map (fn [entry] [(ast->lisb (.getIdentifier entry) args) (ast->lisb (.getValue entry) args)]) (.getEntries node))]
    (apply bstruct entries)))

#_(defmethod ast->lisb AStructExpression [node args]
  (bstruct
      (set (map (fn [recentry] [(ast->lisb (.getIdentifier recentry) args)
                                (ast->lisb (.getValue recentry) args)]) (.getEntries node)))))


;;; sequences

(defmethod ast->lisb AEmptySequenceExpression [_ _]
  '(sequence))
(defmethod ast->lisb ASequenceExtensionExpression [node args]
  (conj (ast->lisb (.getExpression node) args) 'sequence)
  #_(apply bsequence (ast-list->lisb (.getExpression node) args)))
(defmethod ast->lisb ASeqExpression [node args]
  (expression 'seq node args))
(defmethod ast->lisb ASeq1Expression [node args]
  (expression 'seq1 node args))
(defmethod ast->lisb AIseqExpression [node args]
  (expression 'iseq node args))
(defmethod ast->lisb AIseq1Expression [node args]
  (expression 'iseq1 node args))
(defmethod ast->lisb APermExpression [node args]
  (expression 'perm node args))
(defmethod ast->lisb ASizeExpression [node args]
  (expression 'count node args))
(defmethod ast->lisb AConcatExpression [node args]
  (multi-arity 'concat node args))
(defmethod ast->lisb AInsertFrontExpression [node args]
  (left-right '-> node args))
(defmethod ast->lisb AInsertTailExpression [node args]
  (left-right '<- node args))
(defmethod ast->lisb ARevExpression [node args]
  (expression 'reverse node args))
(defmethod ast->lisb AFirstExpression [node args]
  (expression 'first node args))
(defmethod ast->lisb ALastExpression [node args]
  (expression 'last node args))
(defmethod ast->lisb AFrontExpression [node args]
  (expression 'front node args))
(defmethod ast->lisb ATailExpression [node args]
  (expression 'tail node args))
(defmethod ast->lisb AGeneralConcatExpression [node args]
  (expression 'conc node args))
(defmethod ast->lisb ARestrictFrontExpression [node args]
  (left-right 'restrict-front node args))
(defmethod ast->lisb ARestrictTailExpression [node args]
  (left-right 'restrict-tail node args))


;;; functions

(defmethod ast->lisb APartialFunctionExpression [node args]
  (left-right '+-> node args))
(defmethod ast->lisb ATotalFunctionExpression [node args]
  (left-right '--> node args))
(defmethod ast->lisb APartialSurjectionExpression [node args]
  (left-right '+->> node args))
(defmethod ast->lisb ATotalSurjectionExpression [node args]
  (left-right '-->> node args))
(defmethod ast->lisb APartialInjectionExpression [node args]
  (left-right '>+> node args))
(defmethod ast->lisb ATotalInjectionExpression [node args]
  (left-right '>-> node args))
(defmethod ast->lisb APartialBijectionExpression [node args]
  (left-right '>+>> node args))
(defmethod ast->lisb ATotalBijectionExpression [node args]
  (left-right '>->> node args))
(defmethod ast->lisb ALambdaExpression [node args]
  (xyz 'lambda args (.getIdentifiers node) (.getPredicate node) (.getExpression node)))
(defmethod ast->lisb AFunctionExpression [node args]
  (let [f (.getIdentifier node)
        params (ast->lisb (.getParameters node) args)]
    (cond
      (= (class f) ASuccessorExpression) (conj params 'inc)
      (= (class f) APredecessorExpression) (conj params 'dec)
      :else (conj params (ast->lisb f args)) #_(apply (partial bcall (ast->lisb f args)) params))))


;;; relations

(defmethod ast->lisb ARelationsExpression [node args]
  (left-right '<-> node args))
(defmethod ast->lisb ATotalRelationExpression [node args]
  (left-right 'total-relation node args))
(defmethod ast->lisb ASurjectionRelationExpression [node args]
  (left-right 'surjective-relation node args))
(defmethod ast->lisb ATotalSurjectionRelationExpression [node args]
  (left-right 'total-surjective-relation node args))
(defmethod ast->lisb ACoupleExpression [node args]
  (conj (ast-list->lisb (.getList node) args) 'couple))
(defmethod ast->lisb ADomainExpression [node args]
  (expression 'dom node args))
(defmethod ast->lisb ARangeExpression [node args]
  (expression 'ran node args))
(defmethod ast->lisb AIdentityExpression [node args]
  (expression 'id node args))
(defmethod ast->lisb ADomainRestrictionExpression [node args]
  (left-right '<| node args))
(defmethod ast->lisb ADomainSubtractionExpression [node args]
  (left-right '<<| node args))
(defmethod ast->lisb ARangeRestrictionExpression [node args]
  (left-right '|> node args))
(defmethod ast->lisb ARangeSubtractionExpression [node args]
  (left-right '|>> node args))
(defmethod ast->lisb AReverseExpression [node args]
  (expression 'inverse node args))
(defmethod ast->lisb AImageExpression [node args]
  (left-right 'image node args))
(defmethod ast->lisb AOverwriteExpression [node args]
  (multi-arity '<+ node args))
(defmethod ast->lisb ADirectProductExpression [node args]
  (multi-arity '>< node args))
(defmethod ast->lisb ACompositionExpression [node args]
  (multi-arity 'comp node args))
(defmethod ast->lisb AParallelProductExpression [node args]
  (multi-arity '|| node args))
(defmethod ast->lisb AFirstProjectionExpression [node args]
  (list 'prj1 (ast->lisb (.getExp1 node) args) (ast->lisb (.getExp2 node) args)))
(defmethod ast->lisb ASecondProjectionExpression [node args]
  (list 'prj2 (ast->lisb (.getExp1 node) args) (ast->lisb (.getExp2 node) args)))
(defmethod ast->lisb AClosureExpression [node args]
  (expression 'closure1 node args))
(defmethod ast->lisb AReflexiveClosureExpression [node args]
  (expression 'closure node args))
(defmethod ast->lisb AIterationExpression [node args]
  (left-right 'iterate node args))
(defmethod ast->lisb ATransFunctionExpression [node args]
  (expression 'fnc node args))
(defmethod ast->lisb ATransRelationExpression [node args]
  (expression 'rel node args))


;;; numbers

(defmethod ast->lisb AIntegerExpression [node _]
  (Long/parseLong (.getText (.getLiteral node))))
(defmethod ast->lisb AUnaryMinusExpression [node args]
  (let [expr (ast->lisb (.getExpression node) args)]
    (if (number? expr)
      (- expr)
      (list '- expr))))
(defmethod ast->lisb AIntegerSetExpression [_ _] '(integer-set))
(defmethod ast->lisb ANaturalSetExpression [_ _] '(natural-set))
(defmethod ast->lisb ANatural1SetExpression [_ _] '(natural1-set))
(defmethod ast->lisb AIntSetExpression [_ _] '(int-set))
(defmethod ast->lisb ANatSetExpression [_ _] '(nat-set))
(defmethod ast->lisb ANat1SetExpression [_ _] '(nat1-set))
(defmethod ast->lisb AIntervalExpression [node args]
  (list 'interval
    (ast->lisb (.getLeftBorder node) args)
    (ast->lisb (.getRightBorder node) args)))
(defmethod ast->lisb AMinIntExpression [_ _]
  '(min-int))
(defmethod ast->lisb AMaxIntExpression [_ _]
  '(max-int))
(defmethod ast->lisb ALessPredicate [node args]
  (multi-arity '< node args))
(defmethod ast->lisb AGreaterPredicate [node args]
  (multi-arity '> node args))
(defmethod ast->lisb ALessEqualPredicate [node args]
  (multi-arity '<= node args))
(defmethod ast->lisb AGreaterEqualPredicate [node args]
  (multi-arity '>= node args))
(defmethod ast->lisb AMaxExpression [node args]
  (expression 'max node args))
(defmethod ast->lisb AMinExpression [node args]
  (expression 'min node args))
(defmethod ast->lisb AAddExpression [node args]
  (multi-arity '+ node args))
(defmethod ast->lisb AMinusOrSetSubtractExpression [node args]
  (multi-arity '- node args))
(defmethod ast->lisb AMultOrCartExpression [node args]
  (multi-arity '* node args))
(defmethod ast->lisb ADivExpression [node args]
  (multi-arity '/ node args))
(defmethod ast->lisb APowerOfExpression [node args]
  (left-right '** node args))
(defmethod ast->lisb AModuloExpression [node args]
  (multi-arity 'mod node args))
(defmethod ast->lisb AGeneralProductExpression [node args]
  (xyz 'pi args (.getIdentifiers node) (.getPredicates node) (.getExpression node)))      ; (.getPredicates node) returns ONE Predicate and no list!
(defmethod ast->lisb AGeneralSumExpression [node args]
  (xyz 'sigma args (.getIdentifiers node) (.getPredicates node) (.getExpression node)))      ; (.getPredicates node) returns ONE Predicate and no list!
; ASuccessorExpression - processed in AFunctionExpression
; APredecessorExpression - processed in AFunctionExpression


;;; sets

(defmethod ast->lisb AEmptySetExpression [node args]
  #{})
(defmethod ast->lisb ASetExtensionExpression [node args]
  (into #{} (ast->lisb (.getExpressions node) args)))
(defmethod ast->lisb AComprehensionSetExpression [node args]
  (xyz 'comp-set args (.getIdentifiers node) (.getPredicates node))) ; .getPredicates returns ONE predicate)
(defmethod ast->lisb APowSubsetExpression [node args]
  (expression 'pow node args))
(defmethod ast->lisb APow1SubsetExpression [node args]
  (expression 'pow1 node args))
(defmethod ast->lisb AFinSubsetExpression [node args]
  (expression 'fin node args))
(defmethod ast->lisb AFin1SubsetExpression [node args]
  (expression 'fin1 node args))
(defmethod ast->lisb ACardExpression [node args]
  (expression 'count node args))
(defmethod ast->lisb AUnionExpression [node args]
  (multi-arity 'union node args))
(defmethod ast->lisb AIntersectionExpression [node args]
  (multi-arity 'intersection node args))
(defmethod ast->lisb ASetSubtractionExpression [node args]
  (multi-arity 'set- node args))
(defmethod ast->lisb AMemberPredicate [node args]
  (left-right 'member node args))
(defmethod ast->lisb ANotMemberPredicate [node args]
  (left-right 'not-member node args))
(defmethod ast->lisb ASubsetPredicate [node args]
  (left-right 'subset node args))
(defmethod ast->lisb ANotSubsetPredicate [node args]
  (left-right 'not-subset node args))
(defmethod ast->lisb ASubsetStrictPredicate [node args]
  (left-right 'subset-strict node args))
(defmethod ast->lisb ANotSubsetStrictPredicate [node args]
  (left-right 'not-subset-strict node args))
(defmethod ast->lisb AGeneralUnionExpression [node args]
  (expression 'unite-sets node args))
(defmethod ast->lisb AGeneralIntersectionExpression [node args]
  (expression 'intersect-sets node args))
(defmethod ast->lisb AQuantifiedUnionExpression [node args]
  (xyz 'union-pe args (.getIdentifiers node) (.getPredicates node) (.getExpression node)))
(defmethod ast->lisb AQuantifiedIntersectionExpression [node args]
  (xyz 'intersection-pe args(.getIdentifiers node) (.getPredicates node) (.getExpression node)))

;;; booleans

(defmethod ast->lisb ABooleanTrueExpression [_ _]
  true)

(defmethod ast->lisb ABooleanFalseExpression [_ _]
  false)

(defmethod ast->lisb ABoolSetExpression [_ _]
  '(bool-set))

(defmethod ast->lisb AConvertBoolExpression [node args]
  (xyz 'pred->bool args (.getPredicate node)))


;;; equality predicates

(defmethod ast->lisb AEqualPredicate [node args]
  (left-right '= node args))

(defmethod ast->lisb ANotEqualPredicate [node args]
  (left-right 'not= node args))


;;; logical predicates

(defmethod ast->lisb AConjunctPredicate [node args]
  (multi-arity 'and node args))

(defmethod ast->lisb ADisjunctPredicate [node args]
  (multi-arity 'or node args))

(defmethod ast->lisb AImplicationPredicate [node args]
  (multi-arity '=> node args))

(defmethod ast->lisb AEquivalencePredicate [node args]
  (multi-arity '<=> node args))

(defmethod ast->lisb ANegationPredicate [node args]
  (xyz 'not args (.getPredicate node)))

(defmethod ast->lisb AForallPredicate [node args]
  (let [identifiers (.getIdentifiers node)
        implication (.getImplication node)
        assignment (.getLeft implication)
        predicate (.getRight implication)]
    (xyz 'for-all args identifiers assignment predicate)))

(defmethod ast->lisb AExistsPredicate [node args]
  (xyz 'exists args (.getIdentifiers node) (.getPredicate node)))


;;; identifier

(defmethod ast->lisb AIdentifierExpression [node args]
  (ast->lisb (first (.getIdentifier node)) args))           ; Es sollte exakt nur ein Identifier in einer Identifier Expression sein

(defmethod ast->lisb TIdentifierLiteral [node _]
  (keyword (.getText node)))


;;; misc

(defmethod ast->lisb nil [_ _] nil)

(defmethod ast->lisb LinkedList [node args]
  (map #(ast->lisb % args) node))

;;; ast->lisb end

(defn b-ast->lisb [b-ast->lisb] (ast->lisb b-ast->lisb {:symbols {}}))
(defn b->lisb [b] (b-ast->lisb (.parse (BParser.) b false)))
(defn b-formula->lisb [b-formula] (b->lisb (str (BParser/FORMULA_PREFIX) b-formula)))
(defn b-expression->lisb [b-expression] (b->lisb (str (BParser/EXPRESSION_PREFIX) b-expression)))
(defn b-substitution->lisb [b-substitution] (b->lisb (str (BParser/SUBSTITUTION_PREFIX) b-substitution)))
(defn b-transition->lisb [b-transition] (b->lisb (str "#TRANSITION " b-transition)))
(defn b-predicate->lisb [b-predicate] (b->lisb (str (BParser/PREDICATE_PREFIX) b-predicate)))
(defn b-operation->lisb [b-operation] (b->lisb (str (BParser/OPERATION_PATTERN_PREFIX) b-operation)))