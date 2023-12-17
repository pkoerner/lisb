(ns lisb.translation.ir2ast
  (:require [lisb.translation.lisb2ir :refer [b= bnot= band bmember? bimplication machine-clause-tags substitution-tags
                                              seq-tags fn-tags rel-tags num-tags set-tags boolean-tags expr-tags
                                              pred-tags]])
  (:require [clojure.spec.alpha :as s])
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
                                               ARealExpression
                                               TIntegerLiteral
                                               TRealLiteral
                                               ABooleanTrueExpression
                                               ABooleanFalseExpression
                                               AConvertBoolExpression
                                               AConvertRealExpression
                                               AConvertIntCeilingExpression
                                               AConvertIntFloorExpression
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
                                               ARealSetExpression
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
                                               AFreetypesMachineClause
                                               AFreetype
                                               AElementFreetypeConstructor
                                               AConstructorFreetypeConstructor
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
                                               AOperationReference
                                               AMachineReferenceNoParams
                                               PDefinition AExtendsMachineClause AIncludesMachineClause AMachineReference AUsesMachineClause APromotesMachineClause AOpSubstitution ASystemMachineVariant AModelMachineVariant ARefinementMachineParseUnit AImplementationMachineParseUnit ASeesMachineClause ACaseOrSubstitution ACaseSubstitution AExpressionDefinitionDefinition APredicateDefinitionDefinition ASubstitutionDefinitionDefinition TDefLiteralSubstitution TDefLiteralPredicate AFileDefinitionDefinition)))


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
(defn ir-node-constructors->ast [ir-node]
  (map ir->ast-node (:constructors ir-node)))
(defn ir-node-num->ast [ir-node]
  (ir->ast-node (:num ir-node)))
(defn ir-node-nums->ast [ir-node]
  (map ir->ast-node (:nums ir-node)))
(defn ir-node-nums-or-sets->ast [ir-node]
  (map ir->ast-node (:nums-or-sets ir-node)))
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
(defn ir-node-subs->seq-sub-ast [ir-node]
  (let [subs (ir-node-subs->ast ir-node)]
    (if (= 1 (count subs))
      (first subs)
      (ASequenceSubstitution. subs))))
(defn ir-node-elem->ast [ir-node]
  (ir->ast-node (:elem ir-node)))
(defn ir-node-elems->ast [ir-node]
  (map ir->ast-node (:elems ir-node)))
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

(defn process-clauses [f clauses]
  (if (even? (count clauses))
    [(map f (partition 2 clauses)) nil]
    [(map f (partition 2 (drop-last clauses))) (last clauses)]))

;;; specs

(s/def ::id keyword?)
(s/def ::name keyword?)
(s/def ::abstract-machine-name keyword?)
(s/def ::op keyword?)

(s/def ::machine-clause (s/and (s/keys :req-un [::tag])
                               #(contains? machine-clause-tags (:tag %))))
(s/def ::sub (s/and (s/keys :req-un [::tag])
                    #(contains? substitution-tags (:tag %))))
(s/def ::misc (s/and (s/keys :req-un [::tag])
                     #(contains? #{:record-get :fn-call :first :last :let :if-expr} (:tag %))))
(s/def ::rec (s/or
               :keyword keyword?
               :misc ::misc
               :ir-rec (s/and (s/keys :req-un [::tag])
                              #(= :record (:tag %)))))
(s/def ::set (s/or
               :set-literal set?
               :keyword keyword?
               :misc ::misc
               :ir (s/and (s/keys :req-un [::tag])
                          (s/or
                            :set #(contains? set-tags (:tag %))
                            :rel #(contains? rel-tags (:tag %))
                            :f #(contains? fn-tags (:tag %))
                            :seq #(contains? seq-tags (:tag %))))))
(s/def ::seq (s/or
               :string string?
               :set ::set))
(s/def ::f ::set)
(s/def ::rel ::set)
(s/def ::num (s/or
               :literal number?
               :keyword keyword?
               ::misc ::misc
               :ir-num (s/and (s/keys :req-un [::tag])
                              #(contains? num-tags (:tag %)))))
(s/def ::bool (s/or
                :true true?
                :false false?
                :misc ::misc
                :pred->bool  (s/and (s/keys :req-un [::tag])
                                    #(= :pred->bool (:tag %)))))
(s/def ::expr (s/or
                :keyword keyword?
                :misc ::misc
                :bool ::bool
                :num ::num
                :seq ::seq
                :struct (s/and (s/keys :req-un [::tag])
                               #(= :struct (:tag %)))
                :record ::rec))
(s/def ::pred (s/and (s/keys :req-un [::tag]) #(contains? pred-tags (:tag %))))

(s/def ::elem ::expr)
(s/def ::left ::expr)
(s/def ::right ::expr)
(s/def ::set1 ::set)
(s/def ::set2 ::set)
(s/def ::set-of-sets ::set)
(s/def ::seq-of-seqs ::seq)
(s/def ::from ::num)
(s/def ::to ::num)
(s/def ::cond ::pred)
(s/def ::implication (s/and (s/keys :req-un [::tag]) #(= :implication (:tag %))))
(s/def ::expr-or-pred (s/or :expr ::expr :pred ::pred))

;;; col/seq specs

(s/def ::ids (s/coll-of ::id))
(s/def ::id-vals (s/* (s/cat ::id any?)))
(s/def ::id-types (s/* (s/cat ::id any?)))

(s/def ::machine-clauses (s/* ::machine-clause))
(s/def ::subs (s/+ ::sub))
(s/def ::body ::sub)
(s/def ::clauses (s/cat :clauses (s/* (s/cat :cond ::pred :then ::sub)) :else (s/? ::sub)))
(s/def ::cases (s/cat :cases (s/* (s/cat :case ::expr :then ::sub)) :else (s/? ::sub)))
(s/def ::sets (s/* ::set))
(s/def ::seqs (s/or
                :keyword keyword?
                :seq (s/* ::seq)))
(s/def ::rels (s/* ::rel))
(s/def ::nums (s/* ::num))
(s/def ::nums-or-sets (s/or :nums ::nums
                            :sets ::sets))
(s/def ::preds (s/+ ::pred))

(s/def ::args (s/* ::expr))
(s/def ::returns (s/* keyword?))
(s/def ::elems (s/+ ::elem))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ir-node->ast-node (fn [ir-node] (:tag ir-node)))

;;; parse units

(defn abstract-machine-parse-unit [ir-node variant]
  (s/assert (s/keys :req-un [::name ::machine-clauses ::args]) ir-node)
  (AAbstractMachineParseUnit.
    variant
    (AMachineHeader. (.getIdentifier (ir->ast-node (:name ir-node))) (map ir->ast-node (:args ir-node)))
    (map ir->ast-node (:machine-clauses ir-node))))

(defmethod ir-node->ast-node :machine [ir-node]
  (abstract-machine-parse-unit ir-node (AMachineMachineVariant.)))

(defmethod ir-node->ast-node :model [ir-node]
  (abstract-machine-parse-unit ir-node (AModelMachineVariant.)))

(defmethod ir-node->ast-node :system [ir-node]
  (abstract-machine-parse-unit ir-node (ASystemMachineVariant.)))

(defmethod ir-node->ast-node :refinement [ir-node]
  (s/assert (s/keys :req-un [::name ::args ::abstract-machine-name ::machine-clauses]) ir-node)
  (ARefinementMachineParseUnit.
    (AMachineHeader. (.getIdentifier (ir->ast-node (:name ir-node))) (map ir->ast-node (:args ir-node)))
    (TIdentifierLiteral. (name (:abstract-machine-name ir-node)))
    (map ir->ast-node (:machine-clauses ir-node))))

(defmethod ir-node->ast-node :implementation [ir-node]
  (s/assert (s/keys :req-un [::name ::args ::abstract-machine-name ::machine-clauses]) ir-node)
  (AImplementationMachineParseUnit.
    (AMachineHeader. (.getIdentifier (ir->ast-node (:name ir-node))) (map ir->ast-node (:args ir-node)))
    (TIdentifierLiteral. (name (:abstract-machine-name ir-node)))
    (map ir->ast-node (:machine-clauses ir-node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; machine clauses
;; machine inclusions

(defmethod ir-node->ast-node :uses [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  ;; TODO: handle machine references with parameters
  (AUsesMachineClause. (map (fn [x] (AMachineReferenceNoParams. [(TIdentifierLiteral. (name x))])) (:values ir-node))))

(defmethod ir-node->ast-node :includes [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AIncludesMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :machine-reference [ir-node]
  (s/assert (s/keys :req-un [::name ::args]) ir-node)
  (AMachineReference. [(TIdentifierLiteral. (name (:name ir-node)))] (map ir->ast-node (:args ir-node))))

(defmethod ir-node->ast-node :sees [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  ;; TODO: handle machine references with parameters
  (ASeesMachineClause. (map (fn [x] (AMachineReferenceNoParams. [(TIdentifierLiteral. (name x))])) (:values ir-node))))

(defmethod ir-node->ast-node :extends [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AExtendsMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :promotes [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (APromotesMachineClause. (map (fn [x] (AOperationReference. [(TIdentifierLiteral. (name x))])) (:values ir-node))))

;; machine sections

(defmethod ir-node->ast-node :contraints [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AConstraintsMachineClause. (reduce #(AConjunctPredicate. %1 %2) (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :sets [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (ASetsMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :deferred-set [ir-node]
  (s/assert (s/keys :req-un [::id]) ir-node)
  (ADeferredSetSet. (.getIdentifier (ir-node-id->ast ir-node))))
(defmethod ir-node->ast-node :enumerated-set [ir-node]
  (s/assert (s/keys :req-un [::id ::elems]) ir-node)
  (AEnumeratedSetSet. (.getIdentifier (ir-node-id->ast ir-node)) (ir-node-elems->ast ir-node)))

(defmethod ir-node->ast-node :constants [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AConstantsMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :properties [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (APropertiesMachineClause. (reduce #(AConjunctPredicate. %1 %2) (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :definitions [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (ADefinitionsMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :expression-definition [ir-node]
  (AExpressionDefinitionDefinition. (TIdentifierLiteral. (name (:name ir-node))) (ir-node-args->ast ir-node) (ir-node-expr->ast ir-node)))
(defmethod ir-node->ast-node :predicate-definition [ir-node]
  (APredicateDefinitionDefinition. (TDefLiteralPredicate. (name (:name ir-node))) (ir-node-args->ast ir-node) (ir-node-pred->ast ir-node)))
(defmethod ir-node->ast-node :substitution-definition [ir-node]
  (ASubstitutionDefinitionDefinition. (TDefLiteralSubstitution. (name (:name ir-node))) (ir-node-args->ast ir-node) (ir-node-sub->ast ir-node)))
(defmethod ir-node->ast-node :file-definition [ir-node]
  (AFileDefinitionDefinition. (ir-node->ast-node (:name ir-node))))

(defmethod ir-node->ast-node :freetypes [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AFreetypesMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :freetype [ir-node]
  ; not yet released feature: parameters for freetype definitions
  #_(AFreetype. (TIdentifierLiteral. (name (:id ir-node))) (ir-node-args->ast ir-node) (ir-node-constructors->ast ir-node))
  (AFreetype. (TIdentifierLiteral. (name (:id ir-node))) (ir-node-constructors->ast ir-node)))
(defmethod ir-node->ast-node :ft-element [ir-node]
  (AElementFreetypeConstructor. (TIdentifierLiteral. (name (:id ir-node)))))
(defmethod ir-node->ast-node :ft-constructor [ir-node]
  (AConstructorFreetypeConstructor. (TIdentifierLiteral. (name (:id ir-node))) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :variables [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AVariablesMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :invariants [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AInvariantMachineClause. (reduce #(AConjunctPredicate. %1 %2) (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :assertions [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AAssertionsMachineClause. (ir-node-values->ast ir-node)))

(defmethod ir-node->ast-node :init [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AInitialisationMachineClause. (ASequenceSubstitution. (ir-node-values->ast ir-node))))

(defmethod ir-node->ast-node :operations [ir-node]
  (s/assert (s/keys :req-un [::values]) ir-node)
  (AOperationsMachineClause. (ir-node-values->ast ir-node)))
(defmethod ir-node->ast-node :op [ir-node]
  (s/assert (s/keys :req-un [::returns ::name ::args ::body]) ir-node)
  (AOperation. (map ir->ast-node (:returns ir-node)) (list (TIdentifierLiteral. (name (:name ir-node)))) (ir-node-args->ast ir-node) (ir->ast-node (:body ir-node))))

;;; substitutions

(defmethod ir-node->ast-node :skip [_]
  (ASkipSubstitution.))

(defmethod ir-node->ast-node :block [ir-node]
  (s/assert (s/keys :req-un [::sub]) ir-node)
  (ABlockSubstitution. (ir-node-sub->ast ir-node)))

(defmethod ir-node->ast-node :assignment [ir-node]
  (s/assert (s/keys :req-un [::id-vals]) ir-node)
  (let [id-vals (:id-vals ir-node)
        ids (id-vals->ids id-vals)
        vals (id-vals->vals id-vals)]
    (AAssignSubstitution. ids vals)))

(defmethod ir-node->ast-node :becomes-element-of [ir-node]
  (s/assert (s/keys :req-un [::ids ::set]) ir-node)
  (ABecomesElementOfSubstitution. (ir-node-ids->ast ir-node) (ir-node-set->ast ir-node)))
(defmethod ir-node->ast-node :becomes-such [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred]) ir-node)
  (ABecomesSuchSubstitution. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node)))
(defmethod ir-node->ast-node :op-call [ir-node]
  (s/assert (s/keys :req-un [::returns ::op ::args]) ir-node)
  (let [returns (map ir->ast-node (:returns ir-node))
        name (list (TIdentifierLiteral. (name (:op ir-node))))
        args (ir-node-args->ast ir-node)]
    (if (empty? returns)
      (AOpSubstitution. (AIdentifierExpression. name) args)
      (AOperationCallSubstitution. returns name args))))
(defmethod ir-node->ast-node :parallel-sub [ir-node]
  (s/assert (s/keys :req-un [::subs]) ir-node)
  (AParallelSubstitution. (ir-node-subs->ast ir-node)))
(defmethod ir-node->ast-node :sequential-sub [ir-node]
  (s/assert (s/keys :req-un [::subs]) ir-node)
  (ASequenceSubstitution. (ir-node-subs->ast ir-node)))
(defmethod ir-node->ast-node :any [ir-node]
  (s/assert (s/keys :req-un [::pred ::subs]) ir-node)
  (AAnySubstitution. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-subs->seq-sub-ast ir-node)))
(defmethod ir-node->ast-node :let-sub [ir-node]
  (s/assert (s/keys :req-un [::id-vals ::subs]) ir-node)
  (let [id-vals (:id-vals ir-node)
        ids (id-vals->ids id-vals)
        assignment (id-vals->assignment id-vals)
        sub (ir-node-subs->seq-sub-ast ir-node)]
    (ALetSubstitution. ids assignment sub)))
(defmethod ir-node->ast-node :var [ir-node]
  (s/assert (s/keys :req-un [::ids ::subs]) ir-node)
  (AVarSubstitution. (ir-node-ids->ast ir-node) (ir-node-subs->seq-sub-ast ir-node)))
(defmethod ir-node->ast-node :precondition [ir-node]
  (s/assert (s/keys :req-un [::pred ::subs]) ir-node)
  (APreconditionSubstitution. (ir-node-pred->ast ir-node) (ir-node-subs->seq-sub-ast ir-node)))
(defmethod ir-node->ast-node :assert [ir-node]
  (s/assert (s/keys :req-un [::pred ::subs]) ir-node)
  (AAssertionSubstitution. (ir-node-pred->ast ir-node) (ir-node-subs->seq-sub-ast ir-node)))

(defn choice-or-substitution [sub]
  (AChoiceOrSubstitution. sub))
(defmethod ir-node->ast-node :choice [ir-node]
  (s/assert (s/keys :req-un [::subs]) ir-node)
  (let [subs (ir-node-subs->ast ir-node)]
    (AChoiceSubstitution. (conj (map choice-or-substitution (rest subs)) (first subs)))))

(defmethod ir-node->ast-node :if-sub [ir-node]
  (s/assert (s/keys :req-un [::cond ::then]) ir-node)
  (AIfSubstitution. (ir-node-cond->ast ir-node) (ir-node-then->ast ir-node) () (ir-node-else->ast ir-node)))

(defn if-else-sub [[condition then]]
  (AIfElsifSubstitution. condition then))
(defmethod ir-node->ast-node :cond [ir-node]
  (s/assert (s/keys :req-un [::clauses]) ir-node)
  (let [clauses (map ir->ast-node (:clauses ir-node))
        condition (first clauses)
        then (second clauses)
        [else-ifs else] (process-clauses if-else-sub (drop 2 clauses))]
    (AIfSubstitution. condition then else-ifs else)))

(defn select-when-sub [[condition then]]
  (ASelectWhenSubstitution. condition then))
(defmethod ir-node->ast-node :select [ir-node]
  (s/assert (s/keys :req-un [::clauses]) ir-node)
  (let [clauses (map ir->ast-node (:clauses ir-node))
        condition (first clauses)
        then (second clauses)
        [else-ifs else] (process-clauses select-when-sub (drop 2 clauses))]
    (ASelectSubstitution. condition then else-ifs else)))

(defn to-vec [v]
  (if (vector? v)
    v
    [v]))
(defn case-or-sub [[case then]]
  (ACaseOrSubstitution. (to-vec case) then))
(defmethod ir-node->ast-node :case [ir-node]
  (s/assert (s/keys :req-un [::expr ::cases]) ir-node)
  (let [expr (ir-node-expr->ast ir-node)
        cases (map ir->ast-node (:cases ir-node))
        either-expr (to-vec (first cases))
        either-sub (second cases)
        [or-subs else] (process-clauses case-or-sub (drop 2 cases))]
    (ACaseSubstitution. expr either-expr either-sub or-subs else)))


;;; if

(defmethod ir-node->ast-node :if-expr [ir-node]
  (s/assert (s/keys :req-un [::cond ::then ::else]) ir-node)
  (AIfThenElseExpression. (ir-node-cond->ast ir-node) (ir-node-then->ast ir-node) '() (ir-node-else->ast ir-node)))


;;; let

(defmethod ir-node->ast-node :let [ir-node]
  (s/assert (s/keys :req-un [::id-vals ::expr-or-pred]) ir-node)
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
  (s/assert (s/keys :req-un [::id-types]) ir-node)
  (AStructExpression. (map
                        (fn [[k v]] (ARecEntry. (ir->ast-node k) (ir->ast-node v)))
                        (partition 2 (:id-types ir-node)))))

(defmethod ir-node->ast-node :record [ir-node]
  (s/assert (s/keys :req-un [::id-vals]) ir-node)
  (ARecExpression. (map
                     (fn [[k v]] (ARecEntry. (ir->ast-node k) (ir->ast-node v)))
                     (partition 2 (:id-vals ir-node)))))

(defmethod ir-node->ast-node :record-get [ir-node]
  (s/assert (s/keys :req-un [::rec ::id]) ir-node)
  (ARecordFieldExpression. (ir->ast-node (:rec ir-node)) (ir-node-id->ast ir-node)))


;;; sequences

(defmethod ir-node->ast-node :empty-sequence [_]
  (AEmptySequenceExpression.))

(defmethod ir-node->ast-node :sequence [ir-node]
  (s/assert (s/keys :req-un [::elems]) ir-node)
  (ASequenceExtensionExpression. (ir-node-elems->ast ir-node)))

(defmethod ir-node->ast-node :seq [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (ASeqExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :seq1 [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (ASeq1Expression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :iseq [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (AIseqExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :iseq1 [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (AIseq1Expression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :perm [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (APermExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :size [ir-node]
  (s/assert (s/keys :req-un [::seq]) ir-node)
  (ASizeExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :concat [ir-node]
  (s/assert (s/keys :req-un [::seqs]) ir-node)
  (left-associative #(AConcatExpression. %1 %2) (ir-node-seqs->ast ir-node)))

(defmethod ir-node->ast-node :prepend [ir-node]
  (s/assert (s/keys :req-un [::elem ::seq]) ir-node)
  (AInsertFrontExpression. (ir-node-elem->ast ir-node) (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :append [ir-node]
  (s/assert (s/keys :req-un [::elems ::seq]) ir-node)
  (left-associative #(AInsertTailExpression. %1 %2) (conj (ir-node-elems->ast ir-node) (ir-node-seq->ast ir-node))))

(defmethod ir-node->ast-node :reverse [ir-node]
  (s/assert (s/keys :req-un [::seq]) ir-node)
  (ARevExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :first [ir-node]
  (s/assert (s/keys :req-un [::seq]) ir-node)
  (AFirstExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :last [ir-node]
  (s/assert (s/keys :req-un [::seq]) ir-node)
  (ALastExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :front [ir-node]
  (s/assert (s/keys :req-un [::seq]) ir-node)
  (AFrontExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :tail [ir-node]
  (s/assert (s/keys :req-un [::seq]) ir-node)
  (ATailExpression. (ir-node-seq->ast ir-node)))

(defmethod ir-node->ast-node :conc [ir-node]
  (s/assert (s/keys :req-un [::seq-of-seqs]) ir-node)
  (AGeneralConcatExpression. (ir->ast-node (:seq-of-seqs ir-node))))

(defmethod ir-node->ast-node :take [ir-node]
  (s/assert (s/keys :req-un [::seq ::num]) ir-node)
  (ARestrictFrontExpression. (ir-node-seq->ast ir-node) (ir-node-num->ast ir-node)))

(defmethod ir-node->ast-node :drop [ir-node]
  (s/assert (s/keys :req-un [::seq ::num]) ir-node)
  (ARestrictTailExpression. (ir-node-seq->ast ir-node) (ir-node-num->ast ir-node)))


;;; functions

(defmethod ir-node->ast-node :partial-fn [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(APartialFunctionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-fn [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ATotalFunctionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :partial-surjection [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(APartialSurjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-surjection [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ATotalSurjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :partial-injection [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(APartialInjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-injection [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ATotalInjectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :partial-bijection [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(APartialBijectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-bijection [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ATotalBijectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :lambda [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred ::expr]) ir-node)
  (ALambdaExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :fn-call [ir-node]
  (s/assert (s/keys :req-un [::f ::args]) ir-node)
  (AFunctionExpression. (ir->ast-node (:f ir-node)) (map ir->ast-node (:args ir-node))))


;;; relations

(defmethod ir-node->ast-node :relation [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ARelationsExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-relation [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ATotalRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :surjective-realtion [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ASurjectionRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :total-surjective-relation [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ATotalSurjectionRelationExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :maplet [ir-node]
  (s/assert (s/keys :req-un [::left ::right]) ir-node)
  (ACoupleExpression. [(ir-node-left->ast ir-node) (ir-node-right->ast ir-node)]))

(defmethod ir-node->ast-node :dom [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (ADomainExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :ran [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (ARangeExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :id [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (AIdentityExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :domain-restriction [ir-node]
  (s/assert (s/keys :req-un [::rel ::set]) ir-node)
  (ADomainRestrictionExpression. (ir-node-set->ast ir-node) (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :domain-subtraction [ir-node]
  (s/assert (s/keys :req-un [::rel ::set]) ir-node)
  (ADomainSubtractionExpression. (ir-node-set->ast ir-node) (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :range-restriction [ir-node]
  (s/assert (s/keys :req-un [::rel ::set]) ir-node)
  (ARangeRestrictionExpression. (ir-node-rel->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :range-subtraction [ir-node]
  (s/assert (s/keys :req-un [::rel ::set]) ir-node)
  (ARangeSubtractionExpression. (ir-node-rel->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :inverse [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (AReverseExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :image [ir-node]
  (s/assert (s/keys :req-un [::rel ::set]) ir-node)
  (AImageExpression. (ir-node-rel->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :override [ir-node]
  (s/assert (s/keys :req-un [::rels]) ir-node)
  (left-associative #(AOverwriteExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :direct-product [ir-node]
  (s/assert (s/keys :req-un [::rels]) ir-node)
  (left-associative #(ADirectProductExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :composition [ir-node]
  (s/assert (s/keys :req-un [::rels]) ir-node)
  (left-associative #(ACompositionExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :parallel-product [ir-node]
  (s/assert (s/keys :req-un [::rels]) ir-node)
  (left-associative #(AParallelProductExpression. %1 %2) (ir-node-rels->ast ir-node)))

(defmethod ir-node->ast-node :prj1 [ir-node]
  (s/assert (s/keys :req-un [::set1 ::set2]) ir-node)
  (AFirstProjectionExpression. (ir->ast-node (:set1 ir-node)) (ir->ast-node (:set2 ir-node))))

(defmethod ir-node->ast-node :prj2 [ir-node]
  (s/assert (s/keys :req-un [::set1 ::set2]) ir-node)
  (ASecondProjectionExpression. (ir->ast-node (:set1 ir-node)) (ir->ast-node (:set2 ir-node))))

(defmethod ir-node->ast-node :closure1 [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (AClosureExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :closure [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (AReflexiveClosureExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :iterate [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (AIterationExpression. (ir-node-rel->ast ir-node) (ir-node-num->ast ir-node)))

(defmethod ir-node->ast-node :fnc [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (ATransFunctionExpression. (ir-node-rel->ast ir-node)))

(defmethod ir-node->ast-node :rel [ir-node]
  (s/assert (s/keys :req-un [::rel]) ir-node)
  (ATransRelationExpression. (ir-node-rel->ast ir-node)))


;;; numbers

(defmethod ir-node->ast-node :unary-minus [ir-node]
  (s/assert (s/keys :req-un [::num]) ir-node)
  (AUnaryMinusExpression. (ir-node-num->ast ir-node)))

(defmethod ir-node->ast-node :integer-set [_]
  (AIntegerSetExpression.))

(defmethod ir-node->ast-node :real-set [_]
  (ARealSetExpression.))

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

(defmethod ir-node->ast-node :convert-to-real [ir-node]
  (AConvertRealExpression. (ir->ast-node (:expr ir-node))))

(defmethod ir-node->ast-node :floor [ir-node]
  (AConvertIntFloorExpression. (ir->ast-node (:expr ir-node))))

(defmethod ir-node->ast-node :ceil [ir-node]
  (AConvertIntCeilingExpression. (ir->ast-node (:expr ir-node))))

(defmethod ir-node->ast-node :interval [ir-node]
  (s/assert (s/keys :req-un [::from ::to]) ir-node)
  (AIntervalExpression. (ir->ast-node (:from ir-node)) (ir->ast-node (:to ir-node))))

(defmethod ir-node->ast-node :min-int [_]
  (AMinIntExpression.))

(defmethod ir-node->ast-node :max-int [_]
  (AMaxIntExpression.))

(defmethod ir-node->ast-node :greater [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (chain-arity-two (:nums ir-node) #(AGreaterPredicate. %1 %2)))

(defmethod ir-node->ast-node :less [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (chain-arity-two (:nums ir-node) #(ALessPredicate. %1 %2)))

(defmethod ir-node->ast-node :greater-equals [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (chain-arity-two (:nums ir-node) #(AGreaterEqualPredicate. %1 %2)))

(defmethod ir-node->ast-node :less-equals [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (chain-arity-two (:nums ir-node) #(ALessEqualPredicate. %1 %2)))

(defmethod ir-node->ast-node :max [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (AMaxExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :min [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (AMinExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :add [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (left-associative #(AAddExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :sub [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (left-associative #(AMinusOrSetSubtractExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :cartesian-product-or-multiplication [ir-node]
  (s/assert (s/keys :req-un [::nums-or-sets]) ir-node)
  (left-associative #(AMultOrCartExpression. %1 %2) (ir-node-nums-or-sets->ast ir-node)))

(defmethod ir-node->ast-node :mul [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (left-associative #(AMultOrCartExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :div [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (left-associative #(ADivExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :pow [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (right-associative #(APowerOfExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :mod [ir-node]
  (s/assert (s/keys :req-un [::nums]) ir-node)
  (left-associative #(AModuloExpression. %1 %2) (ir-node-nums->ast ir-node)))

(defmethod ir-node->ast-node :pi [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred ::expr]) ir-node)
  (AGeneralProductExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :sigma [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred ::expr]) ir-node)
  (AGeneralSumExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :successor [ir-node]
  (s/assert (s/keys :req-un [::num]) ir-node)
  (AFunctionExpression. (ASuccessorExpression.) (list (ir-node-num->ast ir-node))))

(defmethod ir-node->ast-node :predecessor [ir-node]
  (s/assert (s/keys :req-un [::num]) ir-node)
  (AFunctionExpression. (APredecessorExpression.) (list (ir-node-num->ast ir-node))))


;;; sets

(defmethod ir-node->ast-node :comprehension-set [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred]) ir-node)
  (let [ids (map ir->ast-node (:ids ir-node))]
    (AComprehensionSetExpression. ids (ir-node-pred->ast ir-node))))

(defmethod ir-node->ast-node :power-set [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (APowSubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :power1-set [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (APow1SubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :fin [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (AFinSubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :fin1 [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (AFin1SubsetExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :cardinality [ir-node]
  (s/assert (s/keys :req-un [::set]) ir-node)
  (ACardExpression. (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :cartesian-product [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(AMultOrCartExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :union [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(AUnionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :intersection [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(AIntersectionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :difference [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (left-associative #(ASetSubtractionExpression. %1 %2) (ir-node-sets->ast ir-node)))

(defmethod ir-node->ast-node :member [ir-node]
  (s/assert (s/keys :req-un [::elem ::set]) ir-node)
  (AMemberPredicate. (ir-node-elem->ast ir-node) (ir-node-set->ast ir-node)))

(defmethod ir-node->ast-node :subset [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (chain-arity-two (:sets ir-node) #(ASubsetPredicate. %1 %2)))

(defmethod ir-node->ast-node :strict-subset [ir-node]
  (s/assert (s/keys :req-un [::sets]) ir-node)
  (chain-arity-two (:sets ir-node) #(ASubsetStrictPredicate. %1 %2)))

(defmethod ir-node->ast-node :unite-sets [ir-node]
  (s/assert (s/keys :req-un [::set-of-sets]) ir-node)
  (AGeneralUnionExpression. (ir->ast-node (:set-of-sets ir-node))))

(defmethod ir-node->ast-node :intersect-sets [ir-node]
  (s/assert (s/keys :req-un [::set-of-sets]) ir-node)
  (AGeneralIntersectionExpression. (ir->ast-node (:set-of-sets ir-node))))

(defmethod ir-node->ast-node :union-pe [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred ::expr]) ir-node)
  (AQuantifiedUnionExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))

(defmethod ir-node->ast-node :intersection-pe [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred ::expr]) ir-node)
  (AQuantifiedIntersectionExpression. (ir-node-ids->ast ir-node) (ir-node-pred->ast ir-node) (ir-node-expr->ast ir-node)))


;;; booleans

(defmethod ir-node->ast-node :bool-set [_]
  (ABoolSetExpression.))

(defmethod ir-node->ast-node :pred->bool [ir-node]
  (s/assert (s/keys :req-un [::pred]) ir-node)
  (AConvertBoolExpression. (ir-node-pred->ast ir-node)))


;;; equal predicates

(defmethod ir-node->ast-node :equals [ir-node]
  (s/assert (s/keys :req-un [::left ::right]) ir-node)
  (AEqualPredicate. (ir-node-left->ast ir-node) (ir-node-right->ast ir-node)))

(defmethod ir-node->ast-node :not-equals [ir-node]
  (s/assert (s/keys :req-un [::left ::right]) ir-node)
  (ANotEqualPredicate. (ir-node-left->ast ir-node) (ir-node-right->ast ir-node)))


;;; logical predicates

(defmethod ir-node->ast-node :and [ir-node]
  (s/assert (s/keys :req-un [::preds]) ir-node)
  (left-associative #(AConjunctPredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :or [ir-node]
  (s/assert (s/keys :req-un [::preds]) ir-node)
  (left-associative #(ADisjunctPredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :implication [ir-node]
  (s/assert (s/keys :req-un [::preds]) ir-node)
  (left-associative #(AImplicationPredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :equivalence [ir-node]
  (s/assert (s/keys :req-un [::preds]) ir-node)
  (left-associative #(AEquivalencePredicate. %1 %2) (ir-node-preds->ast ir-node)))

(defmethod ir-node->ast-node :not [ir-node]
  (s/assert (s/keys :req-un [::pred]) ir-node)
  (let [predicate (ir-node-pred->ast ir-node)
        pt (type predicate)]
    ; simplify
    (cond
      (= AMemberPredicate pt) (ANotMemberPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetPredicate pt) (ANotSubsetPredicate. (.getLeft predicate) (.getRight predicate))
      (= ASubsetStrictPredicate pt) (ANotSubsetStrictPredicate. (.getLeft predicate) (.getRight predicate))
      :else (ANegationPredicate. predicate))))

(defmethod ir-node->ast-node :for-all [ir-node]
  (s/assert (s/keys :req-un [::ids ::implication]) ir-node)
  (AForallPredicate. (ir-node-ids->ast ir-node) (ir-node->ast-node (:implication ir-node))))

(defmethod ir-node->ast-node :exists [ir-node]
  (s/assert (s/keys :req-un [::ids ::pred]) ir-node)
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
    ;(vector? ir) (ACoupleExpression. (map ir->ast-node ir))
    (keyword? ir) (AIdentifierExpression. [(TIdentifierLiteral. (name ir))])
    (string? ir) (AStringExpression. (TStringLiteral. ir)) ;; hack-y thing to avoid renaming of rec-get parameters in preds
    (integer? ir) (AIntegerExpression. (TIntegerLiteral. (str ir)))
    (float? ir) (ARealExpression. (TRealLiteral. (str ir)))
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

#_(s/check-asserts true)
