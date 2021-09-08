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
             AStringSetExpression AConstantsMachineClause APropertiesMachineClause AConstraintsMachineClause ASetsMachineClause AConcreteVariablesMachineClause AAssertionsMachineClause AOperationsMachineClause ASkipSubstitution ABecomesElementOfSubstitution ATotalRelationExpression ANotSubsetPredicate ANotSubsetStrictPredicate ASurjectionRelationExpression ATotalSurjectionRelationExpression ASizeExpression AEnumeratedSetSet ADeferredSetSet ASubstitutionParseUnit ABecomesSuchSubstitution AOperationCallSubstitution AParallelSubstitution ASequenceSubstitution AAnySubstitution ALetSubstitution AVarSubstitution APreconditionSubstitution AAssertionSubstitution AIfSubstitution AIfElsifSubstitution ASuccessorExpression APredecessorExpression AMinIntExpression AMaxIntExpression AChoiceSubstitution AChoiceOrSubstitution ASelectSubstitution ASelectWhenSubstitution AOperation AMachineClauseParseUnit)
           (de.be4.classicalb.core.parser BParser)
           (clojure.lang PersistentList PersistentVector)
           (java.util LinkedList)))

(declare ast->lisb read-bmachine)

(defn lisbify [lisb args & nodes]
  (conj (map #(ast->lisb % args) nodes) lisb))

(defn left-right [lisb node args]
  (lisbify lisb args (.getLeft node) (.getRight node)))

(defn expression [lisb node args]
  (lisbify lisb args (.getExpression node)))

(defn concat-last [lisb args & nodes]
  (concat (apply lisbify lisb args (drop-last nodes)) (map #(ast->lisb % args) (last nodes))))

(defn collect-left-associative [node]
  (let [left-node (.getLeft node)
        right-node (.getRight node)]
    (if (= (class node) (class left-node))
      (conj (collect-left-associative left-node) right-node)
      [left-node right-node])))

(defn collect-right-associative [node]
  (let [left-node (.getLeft node)
        right-node (.getRight node)]
    (if (= (class node) (class right-node))
      (conj (collect-right-associative right-node) left-node)
      [right-node left-node])))


(defn multi-arity [lisb node args]
  (apply (partial lisbify lisb args) (collect-left-associative node)))

;;; ast-> lisb start

(defmulti ast->lisb (fn [node _args] (class node)))

(defmethod ast->lisb Start [node args] (ast->lisb (.getPParseUnit node) args))


;;; parse units

(defmethod ast->lisb AAbstractMachineParseUnit [node args]
  (concat-last 'machine args (.getVariant node) (.getHeader node) (.getMachineClauses node))
  #_(conj (ast-list->lisb (.getMachineClauses node) args) (ast->lisb (.getHeader node) args) (ast->lisb (.getVariant node) args) 'machine))
(defmethod ast->lisb AMachineMachineVariant [_ _]
  '(machine-variant))
(defmethod ast->lisb AMachineHeader [node args]
  (lisbify 'machine-header args (first (.getName node)) (.getParameters node)))  ; there should be exact one identifier as name!)

(defmethod ast->lisb AMachineClauseParseUnit [node args]
  (ast->lisb (.getMachineClause node) args))

(defmethod ast->lisb APredicateParseUnit [node args]
  (ast->lisb (.getPredicate node) args))

(defmethod ast->lisb AExpressionParseUnit [node args]
  (ast->lisb (.getExpression node) args))

(defmethod ast->lisb ASubstitutionParseUnit [node args]
  (ast->lisb (.getSubstitution node) args))


;;; machine clauses

(defmethod ast->lisb AConstraintsMachineClause [node args]
  (lisbify 'constraints args (.getPredicates node)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb ASetsMachineClause [node args]
  (concat-last 'sets args (.getSetDefinitions node)))
(defmethod ast->lisb ADeferredSetSet [node args]
  (lisbify 'deferred-set args (first (.getIdentifier node)))) ; there should be exact one identifier
(defmethod ast->lisb AEnumeratedSetSet [node args]
  (concat-last 'enumerated-set args (first (.getIdentifier node)) (.getElements node))) ; there should be exact one identifier

; same for concrete constants
(defmethod ast->lisb AConstantsMachineClause [node args]
  (concat-last 'constants args (.getIdentifiers node)))

(defmethod ast->lisb APropertiesMachineClause [node args]
  (lisbify 'properties args (.getPredicates node)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb ADefinitionsMachineClause [node args]
  (concat-last 'definitions args (.getDefinitions node)))
; TODO
#_(defmethod ast->lisb AExpressionDefinitionDefinition [node args])
#_(defmethod ast->lisb APredicateDefinitionDefinition [node args])

(defmethod ast->lisb AVariablesMachineClause [node args]
  (concat-last 'variables args (.getIdentifiers node)))

; TODO: concrete variables

(defmethod ast->lisb AInvariantMachineClause [node args]
  (lisbify 'invariant args (.getPredicates node)))     ; (.getPredicates node) returns ONE Predicate and no list!

(defmethod ast->lisb AAssertionsMachineClause [node args]
  (concat-last 'assertions args (.getPredicates node)))

(defmethod ast->lisb AInitialisationMachineClause [node args]
  (lisbify 'init args (.getSubstitutions node)))        ; AInitialisationMachineClause holds one PSubstitution

(defmethod ast->lisb AOperationsMachineClause [node args]
  (concat-last 'operations args (.getOperations node)))
(defmethod ast->lisb AOperation [node args]
  (lisbify 'operation args
           (.getReturnValues node)                              ; TODO: test if multiple return values are possible
           (first (.getOpName node))                            ; there should be exact one identifier
           (.getParameters node)
           (.getOperationBody node)))


;;; substitutions

(defmethod ast->lisb ASkipSubstitution [_ _]
  'skip)

(defmethod ast->lisb ABlockSubstitution [node args]
  (ast->lisb (.getSubstitution node) args))

(defmethod ast->lisb AAssignSubstitution [node args]
  (let [left (ast->lisb (.getLhsExpression node) args)
        right (ast->lisb (.getRhsExpressions node) args)]
    (concat ['assign] (interleave left right))))

; TODO: functional override

(defmethod ast->lisb ABecomesElementOfSubstitution [node args]
  (lisbify 'becomes-element-of args (.getIdentifiers node) (.getSet node)))

(defmethod ast->lisb ABecomesSuchSubstitution [node args]
  (lisbify 'becomes-such args (.getIdentifiers node) (.getPredicate node)))

(defmethod ast->lisb AOperationCallSubstitution [node args]
  (lisbify 'operation-call args (.getResultIdentifiers node) (first (.getOperation node)) (.getParameters node))) ; there should be exact one identifier in .getOperation

(defmethod ast->lisb AParallelSubstitution [node args]
  (concat-last 'parallel-substitution args (.getSubstitutions node)))

(defmethod ast->lisb ASequenceSubstitution [node args]
  (concat-last 'sequence-substitution args (.getSubstitutions node)))

(defmethod ast->lisb AAnySubstitution [node args]
  (lisbify 'any args (.getIdentifiers node) (.getWhere node) (.getThen node)))

(defmethod ast->lisb ALetSubstitution [node args]
  (lisbify 'let-sub args (.getIdentifiers node) (.getPredicate node) (.getSubstitution node)))

(defmethod ast->lisb AVarSubstitution [node args]
  (lisbify 'var-sub args (.getIdentifiers node) (.getSubstitution node)))

(defmethod ast->lisb APreconditionSubstitution [node args]
  (lisbify 'pre args (.getPredicate node) (.getSubstitution node)))

(defmethod ast->lisb AAssertionSubstitution [node args]
  (lisbify 'assert args (.getPredicate node) (.getSubstitution node)))

(defmethod ast->lisb AChoiceSubstitution [node args]
  (println (.getSubstitutions node))
  (concat-last 'choice args (.getSubstitutions node)))
(defmethod ast->lisb AChoiceOrSubstitution [node args]
  (println (ast->lisb (.getSubstitution node) args))
  (ast->lisb (.getSubstitution node) args))

(defmethod ast->lisb AIfSubstitution [node args]
  (let [condition (ast->lisb (.getCondition node) args)
        then (ast->lisb (.getThen node) args)
        else-ifs (mapcat identity (ast->lisb (.getElsifSubstitutions node) args))
        else (ast->lisb (.getElse node) args)]
    (if (empty? else-ifs)
      (if else
        (list 'if-sub condition then else)
        (list 'if-sub condition then))
      (if else
        (concat (list 'cond condition then) else-ifs [else])
        (concat (list 'cond condition then) else-ifs)))))
(defmethod ast->lisb AIfElsifSubstitution [node args]
  [(ast->lisb (.getCondition node) args) (ast->lisb (.getThenSubstitution node) args)])

(defmethod ast->lisb ASelectSubstitution [node args]
  (let [condition (ast->lisb (.getCondition node) args)
        then (ast->lisb (.getThen node) args)
        else-ifs (mapcat identity (ast->lisb (.getWhenSubstitutions node) args))
        else (ast->lisb (.getElse node) args)]
    (if else
      (concat (list 'select condition then) else-ifs [else])
      (concat (list 'select condition then) else-ifs))))
(defmethod ast->lisb ASelectWhenSubstitution [node args]
  [(ast->lisb (.getCondition node) args) (ast->lisb (.getSubstitution node) args)])

; TODO: case

;;; if-then-else

(defmethod ast->lisb AIfThenElseExpression [node args]
  (lisbify 'if-expr args (.getCondition node) (.getThen node) (.getElse node)))


;;; let

(defmethod ast->lisb ALetExpressionExpression [node args]
  (lisbify 'let-expr args (.getIdentifiers node) (.getAssignment node) (.getExpr node)))
(defmethod ast->lisb ALetPredicatePredicate [node args]
  (lisbify 'let-pred args (.getIdentifiers node) (.getAssignment node) (.getPred node)))


;;; trees


;;; reals - (alpha - besser nicht verwenden)


;;; strings

(defmethod ast->lisb AStringExpression [node _]
  (.getText (.getContent node)))

(defmethod ast->lisb AStringSetExpression [_ _]
  'string-set)


;;; records

(defmethod ast->lisb AStructExpression [node args]
  (concat (list 'struct) (mapcat #(ast->lisb % args) (.getEntries node))))

(defmethod ast->lisb ARecExpression [node args]
  (concat (list 'record) (mapcat #(ast->lisb % args) (.getEntries node))))

(defmethod ast->lisb ARecEntry [node args]
  [(ast->lisb (.getIdentifier node) args) (ast->lisb (.getValue node) args)])

(defmethod ast->lisb ARecordFieldExpression [node args]
  (lisbify 'rec-get args (.getRecord node) (.getIdentifier node)))


;;; sequences

(defmethod ast->lisb AEmptySequenceExpression [_ _]
  '(sequence))
(defmethod ast->lisb ASequenceExtensionExpression [node args]
  (concat-last 'sequence args (.getExpression node)))
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
  (expression 'count-seq node args))
(defmethod ast->lisb AConcatExpression [node args]
  (multi-arity 'concat node args))
(defmethod ast->lisb AInsertFrontExpression [node args]
  (apply (partial lisbify 'cons args) (collect-right-associative node)))
(defmethod ast->lisb AInsertTailExpression [node args]
  (multi-arity 'append node args))
(defmethod ast->lisb ARevExpression [node args]
  (expression 'reverse node args))
(defmethod ast->lisb AFirstExpression [node args]
  (expression 'first node args))
(defmethod ast->lisb ALastExpression [node args]
  (expression 'last node args))
(defmethod ast->lisb AFrontExpression [node args]
  (expression 'drop-last node args))
(defmethod ast->lisb ATailExpression [node args]
  (expression 'rest node args))
(defmethod ast->lisb AGeneralConcatExpression [node args]
  (expression 'conc node args))
(defmethod ast->lisb ARestrictFrontExpression [node args]
  (lisbify 'take args (.getRight node) (.getLeft node)))
(defmethod ast->lisb ARestrictTailExpression [node args]
  (lisbify 'drop args (.getRight node) (.getLeft node)))


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
  (lisbify 'lambda args (.getIdentifiers node) (.getPredicate node) (.getExpression node)))
(defmethod ast->lisb AFunctionExpression [node args]
  (let [f (.getIdentifier node)
        params (.getParameters node)]
    (cond
      (= (class f) ASuccessorExpression) (concat-last 'inc args params)
      (= (class f) APredecessorExpression) (concat-last 'dec args params)
      :else (concat-last 'call args f params))))


;;; relations

(defmethod ast->lisb ARelationsExpression [node args]
  (multi-arity '<-> node args))
(defmethod ast->lisb ATotalRelationExpression [node args]
  (multi-arity 'total-relation node args))
(defmethod ast->lisb ASurjectionRelationExpression [node args]
  (multi-arity 'surjective-relation node args))
(defmethod ast->lisb ATotalSurjectionRelationExpression [node args]
  (multi-arity 'total-surjective-relation node args))
(defmethod ast->lisb ACoupleExpression [node args]
  (let [children (mapv #(ast->lisb % args) (.getList node))
        left (first children)]
    (if (vector? left)
      (conj left (second children))
      children)))
(defmethod ast->lisb ADomainExpression [node args]
  (expression 'dom node args))
(defmethod ast->lisb ARangeExpression [node args]
  (expression 'ran node args))
(defmethod ast->lisb AIdentityExpression [node args]
  (expression 'identity node args))
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
(defmethod ast->lisb AIntegerSetExpression [_ _] 'integer-set)
(defmethod ast->lisb ANaturalSetExpression [_ _] 'natural-set)
(defmethod ast->lisb ANatural1SetExpression [_ _] 'natural1-set)
(defmethod ast->lisb AIntSetExpression [_ _] 'int-set)
(defmethod ast->lisb ANatSetExpression [_ _] 'nat-set)
(defmethod ast->lisb ANat1SetExpression [_ _] 'nat1-set)
(defmethod ast->lisb AIntervalExpression [node args]
  (lisbify 'interval args (.getLeftBorder node) (.getRightBorder node)))
(defmethod ast->lisb AMinIntExpression [_ _]
  'min-int)
(defmethod ast->lisb AMaxIntExpression [_ _]
  'max-int)
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
  (lisbify 'pi args (.getIdentifiers node) (.getPredicates node) (.getExpression node)))      ; (.getPredicates node) returns ONE Predicate and no list!
(defmethod ast->lisb AGeneralSumExpression [node args]
  (lisbify 'sigma args (.getIdentifiers node) (.getPredicates node) (.getExpression node)))      ; (.getPredicates node) returns ONE Predicate and no list!
; ASuccessorExpression - processed in AFunctionExpression
; APredecessorExpression - processed in AFunctionExpression


;;; sets

(defmethod ast->lisb AEmptySetExpression [_ _]
  #{})

(defmethod ast->lisb ASetExtensionExpression [node args]
  (into #{} (map #(ast->lisb % args) (.getExpressions node))))

(defmethod ast->lisb AComprehensionSetExpression [node args]
 (lisbify 'comp-set args (.getIdentifiers node) (.getPredicates node)))  ; .getPredicates returns ONE predicate)

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
  (multi-arity 'difference node args))

(defmethod ast->lisb AMemberPredicate [node args]
  (lisbify 'contains? args (.getRight node) (.getLeft node)))

(defmethod ast->lisb ANotMemberPredicate [node args]
  (list 'not (lisbify 'contains? args (.getRight node) (.getLeft node))))

(defmethod ast->lisb ASubsetPredicate [node args]
  (left-right 'subset? node args))

(defmethod ast->lisb ANotSubsetPredicate [node args]
  (list 'not (left-right 'subset? node args)))

(defmethod ast->lisb ASubsetStrictPredicate [node args]
  (left-right 'subset-strict? node args))

(defmethod ast->lisb ANotSubsetStrictPredicate [node args]
  (list 'not (left-right 'subset-strict? node args)))

(defmethod ast->lisb AGeneralUnionExpression [node args]
  (expression 'unite-sets node args))

(defmethod ast->lisb AGeneralIntersectionExpression [node args]
  (expression 'intersect-sets node args))

(defmethod ast->lisb AQuantifiedUnionExpression [node args]
  (lisbify 'union-pe args (.getIdentifiers node) (.getPredicates node) (.getExpression node)))

(defmethod ast->lisb AQuantifiedIntersectionExpression [node args]
  (lisbify 'intersection-pe args (.getIdentifiers node) (.getPredicates node) (.getExpression node)))

;;; booleans

(defmethod ast->lisb ABooleanTrueExpression [_ _]
  true)

(defmethod ast->lisb ABooleanFalseExpression [_ _]
  false)

(defmethod ast->lisb ABoolSetExpression [_ _]
  'bool-set)

(defmethod ast->lisb AConvertBoolExpression [node args]
  (lisbify 'pred->bool args (.getPredicate node)))


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
  (lisbify 'not args (.getPredicate node)))

(defmethod ast->lisb AForallPredicate [node args]
  (lisbify 'for-all args (.getIdentifiers node) (.getImplication node)))

(defmethod ast->lisb AExistsPredicate [node args]
  (lisbify 'exists args (.getIdentifiers node) (.getPredicate node)))


;;; identifier

(defmethod ast->lisb AIdentifierExpression [node args]
  (ast->lisb (first (.getIdentifier node)) args))           ; Es sollte exakt nur ein Identifier in einer Identifier Expression sein

(defmethod ast->lisb TIdentifierLiteral [node _]
  (keyword (.getText node)))


;;; misc

(defmethod ast->lisb nil [_ _] nil)

; for the most part i want a vector
(defmethod ast->lisb LinkedList [node args]
  (mapv #(ast->lisb % args) node))

;;; ast->lisb end
(defn b-ast->lisb [b-ast->lisb] `(b ~(ast->lisb b-ast->lisb {:symbols {}})))

(defn b->lisb [b] (b-ast->lisb (.parse (BParser.) b false)))
(defn b-formula->lisb [b-formula] (b-ast->lisb (.parseFormula (BParser.) b-formula)))
(defn b-expression->lisb [b-expression] (b-ast->lisb (.parseExpression (BParser.) b-expression)))
(defn b-substitution->lisb [b-substitution] (b-ast->lisb (.parseSubstitution (BParser.) b-substitution)))
(defn b-predicate->lisb [b-predicate] (b-ast->lisb (.parsePredicate (BParser.) b-predicate)))
(defn b-operation->lisb [b-operation] (b-ast->lisb (.parseTransition (BParser.) b-operation)))
