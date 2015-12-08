(ns lisb.translation
  (:require [clojure.walk :refer [walk]])
  (:import de.prob.animator.domainobjects.ClassicalB
           (de.be4.classicalb.core.parser.node AAddExpression
                                               AMinusExpression
                                               AMultOrCartExpression
                                               ADivExpression
                                               AUnaryMinusExpression
                                               AIntegerExpression
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
                                               TStringLiteral
                                               ALetExpressionExpression
                                               ALetPredicatePredicate
                                               )))

(declare to-ast-wrapped to-ast)

(defn identifier [n]
  (let [token (TIdentifierLiteral. (name n))]
    (AIdentifierExpression. [token])))

(defn integer [n]
  (let [token (TIntegerLiteral. (str n))]
    (AIntegerExpression. token)))

;;; TODO: this smells like it could be done nicer
(defn enumerated-set-node [& args]
  (if-not (seq args)
    (AEmptySetExpression.)
    (ASetExtensionExpression. args)))

(defn boolean-true []
  (ABooleanTrueExpression.))

(defn boolean-false []
  (ABooleanFalseExpression.))

(defn less-node [l r]
  (ALessPredicate. l r))

(defn greater-node [l r]
  (AGreaterPredicate. l r))

(defn less-eq-node [l r]
  (ALessEqualPredicate. l r))

(defn greater-eq-node [l r]
  (AGreaterEqualPredicate. l r))

(defn plus-node [l r]
  (AAddExpression. l r))

(defn minus-node [l r]
  (AMinusExpression. l r))

(defn mul-node [l r]
  (AMultOrCartExpression. l r))

(defn div-node [l r]
  (ADivExpression. l r))

(defn unaryminus-node [n]
  (AUnaryMinusExpression. n))

(defn conjunction-node [l r]
  (AConjunctPredicate. l r))

(defn equal-node [l r]
  (AEqualPredicate. l r))

(defn equivalence-node [l r]
  (AEquivalencePredicate. l r))

(defn disjunction-node [l r]
  (ADisjunctPredicate. l r))

(defn not-node [p]
  (ANegationPredicate. p))

(defn not-equals-node [l r]
  (ANotEqualPredicate. l r))

(defn to-bool-node [p]
  (AConvertBoolExpression. p))

(defn list-node [& args]
  args)

(defn comprehension-set-node [v p]
  (AComprehensionSetExpression. v p))

(defn power-set-node [s]
  (APowSubsetExpression. s))

(defn power1-set-node [s]
  (APow1SubsetExpression. s))

(defn finite-subset-node [s]
  (AFinSubsetExpression. s))

(defn finite1-subset-node [s]
  (AFin1SubsetExpression. s))

(defn card-node [s]
  (ACardExpression. s))

(defn set-union-node [l r]
  (AUnionExpression. l r))

(defn set-intersection-node [l r]
  (AIntersectionExpression. l r))

(defn set-difference-node [l r]
  (ASetSubtractionExpression. l r))

(defn member-node [e s]
  (AMemberPredicate. e s))

(defn subset-node [l r]
  (ASubsetPredicate. l r))

(defn subset-strict-node [l r]
  (ASubsetStrictPredicate. l r))

(defn bool-set-node []
  (ABoolSetExpression.))

(defn natural-set-node []
  (ANaturalSetExpression.))

(defn natural1-set-node []
  (ANatural1SetExpression.))

(defn int-set-node []
  (AIntegerSetExpression.))

(defn nat-set-node []
  (ANatSetExpression.))

(defn nat1-set-node []
  (ANat1SetExpression.))

(defn max-node [s]
  (AMaxExpression. s))

(defn min-node [s]
  (AMinExpression. s))

(defn mod-node [n m]
  (AModuloExpression. n m))

(defn tuple-node [l r]
  (ACoupleExpression. [l r]))

(defn relations-node [l r]
  (ARelationsExpression. l r))

(defn domain-node [r]
  (ADomainExpression. r))

(defn range-node [r]
  (ARangeExpression. r))

(defn identity-node [s]
  (AIdentityExpression. s))

(defn dom-restrict-node [s r]
  (ADomainRestrictionExpression. s r))

(defn dom-subtract-node [s r]
  (ADomainSubtractionExpression. s r))

(defn range-restrict-node [r s]
  (ARangeRestrictionExpression. r s))

(defn range-subtract-node [r s]
  (ARangeSubtractionExpression. r s))

(defn inverse-node [r]
  (AReverseExpression. r))

(defn image-node [r s]
  (AImageExpression. r s))

(defn override-node [l r]
  (AOverwriteExpression. l r))

(defn direct-product-node [l r]
  (ADirectProductExpression. l r))

(defn comp-node [l r]
  (ACompositionExpression. l r))

(defn parallel-product-node [l r]
  (AParallelProductExpression. l r))

(defn projection1-node [s t]
  (AFirstProjectionExpression. s t))

(defn projection2-node [s t]
  (ASecondProjectionExpression. s t))

(defn closure-node [r]
  (AReflexiveClosureExpression. r))

(defn closure1-node [r]
  (AClosureExpression. r))

(defn iterate-node [r n]
  (AIterationExpression. r n))

(defn functionise-node [r]
  (ATransFunctionExpression. r))

(defn relationise-node [r]
  (ATransRelationExpression. r))

(defn partial-function-node [l r]
  (APartialFunctionExpression. l r))

(defn total-function-node [l r]
  (ATotalFunctionExpression. l r))

(defn partial-surjection-node [l r]
  (APartialSurjectionExpression. l r))

(defn total-surjection-node [l r]
  (ATotalSurjectionExpression. l r))

(defn partial-injection-node [l r]
  (APartialInjectionExpression. l r))

(defn total-injection-node [l r]
  (ATotalInjectionExpression. l r))

(defn partial-bijection-node [l r]
  (APartialBijectionExpression. l r))

(defn total-bijection-node [l r]
  (ATotalBijectionExpression. l r))

(defn lambda-node [vs p e]
  (ALambdaExpression. vs p e))

(defn fn-application-node [f & args]
  (AFunctionExpression. f args))

(defn implication-node [l r]
  (AImplicationPredicate. l r))

(defn forall-node [identifiers impl]
  (AForallPredicate. identifiers impl))

(defn exists-node [identifiers pred]
  (AExistsPredicate. identifiers pred))

(defn interval-node [from to]
  (AIntervalExpression. from to))

(defn sequence-node [& elements]
  (if (seq elements)
      (ASequenceExtensionExpression. elements)
      (AEmptySequenceExpression.)))

(defn iseq-node [s]
  (AIseqExpression. s))

(defn iseq1-node [s]
  (AIseq1Expression. s))

(defn perm-node [s]
  (APermExpression. s))

(defn concat-node [l r]
  (AConcatExpression. l r))

(defn prepend-node [e s]
  (AInsertFrontExpression. e s))

(defn append-node [s e]
  (AInsertTailExpression. s e))

;; TODO: is ARevExpression correct? the parser generates a function expression...
(defn reverse-node [s]
  (ARevExpression. s))

(defn first-node [s]
  (AFirstExpression. s))

(defn last-node [s]
  (ALastExpression. s))

(defn front-node [s]
  (AFrontExpression. s))

(defn tail-node [s]
  (ATailExpression. s))

(defn restrict-front-node [s n]
  (ARestrictFrontExpression. s n))

(defn restrict-tail-node [s n]
  (ARestrictTailExpression. s n))

(defn power-of-node [l r]
  (APowerOfExpression. l r))

(defn sigma-node [ids p e]
  (AGeneralSumExpression. ids p e))

(defn pi-node [ids p e]
  (AGeneralProductExpression. ids p e))

(defn general-union-node [s]
  (AGeneralUnionExpression. s))

(defn general-intersection-node [s]
  (AGeneralIntersectionExpression. s))

(defn seq-node [s]
  (ASeqExpression. s))

(defn seq1-node [s]
  (ASeq1Expression. s))

(defn conc-node [s]
  (AGeneralConcatExpression. s))

(defn if-node [c t e]
  (AIfThenElseExpression. c t e))

(defn quantified-union-node [v p e]
  (AQuantifiedUnionExpression. v p e))

(defn quantified-intersection-node [v p e]
  (AQuantifiedIntersectionExpression. v p e))

(defn struct-node [ks vs]
  (AStructExpression. (map (fn [k v] (ARecEntry. (identifier k) v)) ks vs)))

(defn record-node [ks vs]
  (ARecExpression. (map (fn [k v] (ARecEntry. (identifier k) v)) ks vs)))

(defn record-get-node [s id]
  (ARecordFieldExpression. s (identifier id)))



(defn string-node [s]
  (AStringExpression. (TStringLiteral. s)))


(defn let-pred-node [ks vs p]
  (ALetPredicatePredicate. ks vs p))

(defn let-expr-node [ks vs e]
  (ALetExpressionExpression. ks vs e))

(defn bpredcode-node [code]
  (.. (ClassicalB. code) getAst getPParseUnit getPredicate))

(defn bexprcode-node [code]
  (.. (ClassicalB. code) getAst getPParseUnit getExpression))

(defn literal [x]
  (cond (keyword? x) (identifier x)
        (string? x) x ;; hack-y thing to avoid renaming
                      ;; of rec-get parameters in preds
        (number? x) (integer x)
        (true? x) (boolean-true)
        (false? x) (boolean-false)
        (set? x) (apply enumerated-set-node (map to-ast x))
        (sequential? x) (apply tuple-node (map to-ast x))
        :otherwise (println :unhandled-literal x)
        
        ))

(def to-ast-map {:less less-node
                 :greater greater-node
                 :less-eq less-eq-node
                 :greater-eq greater-eq-node
                 :plus plus-node
                 :mul mul-node
                 :div div-node
                 :and conjunction-node
                 :minus minus-node
                 :unaryminus unaryminus-node
                 :equals equal-node
                 :equivalence equivalence-node
                 :or disjunction-node
                 :not not-node
                 :not-equals not-equals-node
                 :to-bool to-bool-node
                 :comp-set comprehension-set-node
                 :power-set power-set-node
                 :power1-set power1-set-node
                 :finite-subset finite-subset-node
                 :finite1-subset finite1-subset-node
                 :card card-node
                 :set-union set-union-node
                 :set-intersection set-intersection-node
                 :set-difference set-difference-node
                 :member member-node
                 :subset subset-node
                 :subset-strict subset-strict-node
                 :bool-set bool-set-node
                 :natural-set natural-set-node
                 :natural1-set natural1-set-node
                 :int-set int-set-node
                 :nat-set nat-set-node
                 :nat1-set nat1-set-node
                 :max max-node
                 :min min-node
                 :mod mod-node
                 :list list-node
                 :relation relations-node
                 :domain domain-node
                 :range range-node
                 :identity-relation identity-node
                 :domain-restriction dom-restrict-node
                 :domain-subtraction dom-subtract-node
                 :range-restriction range-restrict-node
                 :range-subtraction range-subtract-node
                 :inverse-relation inverse-node
                 :relational-image image-node
                 :relational-override override-node
                 :direct-product direct-product-node
                 :relational-composition comp-node
                 :parallel-product parallel-product-node
                 :proj1 projection1-node
                 :proj2 projection2-node
                 :closure closure-node
                 :closure1 closure1-node
                 :iterate iterate-node
                 :functionise functionise-node
                 :relationise relationise-node
                 :partial-fn partial-function-node
                 :total-fn total-function-node
                 :partial-surjection partial-surjection-node
                 :total-surjection total-surjection-node
                 :partial-injection partial-injection-node
                 :total-injection total-injection-node
                 :partial-bijection partial-bijection-node
                 :total-bijection total-bijection-node
                 :lambda lambda-node
                 :fn-application fn-application-node
                 :implication implication-node
                 :forall forall-node
                 :exists exists-node
                 :interval interval-node
                 :sequence sequence-node
                 :iseq iseq-node
                 :iseq1 iseq1-node
                 :perm perm-node
                 :concat concat-node
                 :prepend prepend-node
                 :append append-node
                 :reverse reverse-node
                 :first first-node
                 :last last-node
                 :front front-node
                 :tail tail-node
                 :restrict-front restrict-front-node
                 :restrict-tail restrict-tail-node
                 :pow power-of-node
                 :sigma sigma-node
                 :pi pi-node
                 :general-union general-union-node
                 :general-intersection general-intersection-node
                 :seq seq-node
                 :seq1 seq1-node
                 :conc conc-node
                 :if if-node
                 :union-pe quantified-union-node
                 :intersection-pe quantified-intersection-node
                 :struct struct-node
                 :record record-node
                 :record-get record-get-node
                 :enumerated-set enumerated-set-node
                 :tuple tuple-node
                 :bexpr bexprcode-node
                 :bpred bpredcode-node
                 :string string-node
                 :let-pred let-pred-node
                 :let-expr let-expr-node
                 })


(defn to-ast-inner [data]
  (if (map? data)
      (let [processed-args (to-ast-wrapped (:children data))]
        (apply (to-ast-map (:tag data)) processed-args))
      (literal data)))

(defn to-ast-wrapped [datav]
  (walk to-ast-inner identity datav))

(defn to-ast [data]
  (first (to-ast-wrapped [data]))) ; wraps data into a list and unwraps it on return

