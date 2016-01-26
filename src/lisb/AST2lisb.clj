(ns lisb.AST2lisb
  (:require [clojure.pprint])
  (:import (de.be4.classicalb.core.parser.node AAddExpression
                                               AMinusExpression
                                               AMultOrCartExpression
                                               AMinusOrSetSubtractExpression
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
                                               TStringLiteral
                                               ADefinitionExpression
                                               ALetExpressionExpression
                                               ALetPredicatePredicate
                                               Start
                                               APredicateParseUnit
                                               AExpressionParseUnit
                                               ADefinitionPredicate
                                               )))



(def symbol-repr (partial symbol "lisb.representation"))

(declare AST->lisb)

(defn left-right [node name args]
  (list (symbol-repr name) (AST->lisb (.getLeft node) args) (AST->lisb (.getRight node) args)))

(defn identifier-list [l args]
  (map #(AST->lisb % args) l))


(defmulti AST->lisb (fn [node args] (class node)))
(defmethod AST->lisb AIntegerExpression [node _] (Long/parseLong (.. node getLiteral getText)))
(defmethod AST->lisb AIdentifierExpression [node args] (let [id (.getText (first (.getIdentifier node)))]
                                                         (if (args id) (symbol id) (keyword id))))

(defmethod AST->lisb AStringExpression [node _] (.. node getContent getText))

;;; Equality
(defmethod AST->lisb AEqualPredicate [node args] (left-right node "b=" args))
(defmethod AST->lisb ANotEqualPredicate [node args] (left-right node "bnot=" args))

;;; Booleans
(defmethod AST->lisb ABooleanTrueExpression [_ _] true)
(defmethod AST->lisb ABooleanFalseExpression [_ _] false)
(defmethod AST->lisb ABoolSetExpression     [_ _] (list (symbol-repr "bbool-set")))
(defmethod AST->lisb AConvertBoolExpression [node args] (list (symbol-repr "bpred->bool") (AST->lisb (.getPredicate node) args)))


;;; Numbers
;; INTEGER
(defmethod AST->lisb ANaturalSetExpression  [_ _] (list (symbol-repr "bnatural-set")))
(defmethod AST->lisb ANatural1SetExpression [_ _] (list (symbol-repr "bnatural1-set")))
;; INT
(defmethod AST->lisb AIntSetExpression  [_ _] (list (symbol-repr "bint-set")))
(defmethod AST->lisb AIntegerSetExpression  [_ _] (list (symbol-repr "binteger-set")))
(defmethod AST->lisb ANatSetExpression      [_ _] (list (symbol-repr "bnat-set")))
(defmethod AST->lisb ANat1SetExpression     [_ _] (list (symbol-repr "bnat1-set")))
(defmethod AST->lisb AIntervalExpression [node args] (list (symbol-repr "binterval")
                                                           (AST->lisb (.getLeftBorder node) args)
                                                           (AST->lisb (.getRightBorder node) args)))
;; MININT
;; MAXINT
(defmethod AST->lisb AGreaterPredicate [node args] (left-right node "b>" args))
(defmethod AST->lisb ALessPredicate [node args] (left-right node "b<" args))
(defmethod AST->lisb AGreaterEqualPredicate [node args] (left-right node "b>=" args))
(defmethod AST->lisb ALessEqualPredicate [node args] (left-right node "b<=" args))
(defmethod AST->lisb AMaxExpression [node args] (list (symbol-repr "bmax") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AMinExpression [node args] (list (symbol-repr "bmin") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AAddExpression [node args] (left-right node "b+" args))
(defmethod AST->lisb AMinusOrSetSubtractExpression [node args] (left-right node "bminus-or-set-subtract" args))
(defmethod AST->lisb AUnaryMinusExpression [node args] (list (symbol-repr "bminus") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AMultOrCartExpression [node args] (left-right node "b*" args))
(defmethod AST->lisb ADivExpression [node args] (left-right node "bdiv" args))
(defmethod AST->lisb APowerOfExpression [node args] (left-right node "b**" args))
(defmethod AST->lisb AModuloExpression [node args] (left-right node "bmod" args))
(defmethod AST->lisb AGeneralProductExpression [node args] (list (symbol-repr "bpi")
                                                                 (identifier-list (.getIdentifiers node) args)
                                                                 (AST->lisb (.getPredicates node) args)
                                                                 (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AGeneralSumExpression [node args] (list (symbol-repr "bsigma")
                                                             (identifier-list (.getIdentifiers node) args)
                                                             (AST->lisb (.getPredicates node) args)
                                                             (AST->lisb (.getExpression node) args)))
;; succ
;; pred

;;; Logical predicates
(defmethod AST->lisb AConjunctPredicate [node args] (left-right node "band" args))
(defmethod AST->lisb ADisjunctPredicate [node args] (left-right node "bor" args))
(defmethod AST->lisb AImplicationPredicate [node args] (left-right node "b=>" args))
(defmethod AST->lisb AEquivalencePredicate [node args] (left-right node "b<=>" args))
(defmethod AST->lisb ANegationPredicate [node args] (list (symbol-repr "bnot") (AST->lisb (.getPredicate node) args)))
(defmethod AST->lisb AForallPredicate [node args] (list (symbol-repr "bforall")
                                                        (identifier-list (.getIdentifiers node) args)
                                                        (AST->lisb (.getImplication node) args)))
(defmethod AST->lisb AExistsPredicate [node args] (list (symbol-repr "bexists")
                                                        (identifier-list (.getIdentifiers node) args)
                                                        (AST->lisb (.getPredicate node) args)))

;;; Sets
(defmethod AST->lisb AEmptySetExpression [_ _] #{})
(defmethod AST->lisb ASetExtensionExpression [node args] (apply list (symbol-repr "bset-enum") (map #(AST->lisb % args) (.getExpressions node))))
(defmethod AST->lisb AComprehensionSetExpression [node args] (list (symbol-repr "bset")
                                                                   (identifier-list (.getIdentifiers node) args)
                                                                   (AST->lisb (.getPredicates node) args)))
(defmethod AST->lisb APowSubsetExpression [node args] (list (symbol-repr "bpow") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb APow1SubsetExpression [node args] (list (symbol-repr "bpow1") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AFinSubsetExpression [node args] (list (symbol-repr "bfin") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AFin1SubsetExpression [node args] (list (symbol-repr "bfin1") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb ACardExpression [node args] (list (symbol-repr "bcount") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AUnionExpression [node args] (left-right node "bunion" args))
(defmethod AST->lisb AIntersectionExpression [node args] (left-right node "bintersection" args))
(defmethod AST->lisb AMemberPredicate [node args] (left-right node "bmember" args))
;; not element of
(defmethod AST->lisb ASubsetPredicate [node args] (left-right node "bsubset" args))
;; not subset of
(defmethod AST->lisb ASubsetStrictPredicate [node args] (left-right node "bsubset-strict" args))
;; not strict subset of
(defmethod AST->lisb AGeneralUnionExpression [node args] (list (symbol-repr "bunite-sets")
                                                               (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AGeneralIntersectionExpression [node args] (list (symbol-repr "bintersect-sets")
                                                                      (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AQuantifiedUnionExpression [node args] (list (symbol-repr "bunion-pe")
                                                                  (identifier-list (.getIdentifiers node) args)
                                                                  (AST->lisb (.getPredicates node) args)
                                                                  (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AQuantifiedIntersectionExpression [node args] (list (symbol-repr "bintersection-pe")
                                                                         (identifier-list (.getIdentifiers node) args)
                                                                         (AST->lisb (.getPredicates node) args)
                                                                         (AST->lisb (.getExpression node) args)))


;;; Relations
(defmethod AST->lisb ARelationsExpression [node args] (left-right node "b<->" args))
(defmethod AST->lisb ACoupleExpression [node args] (apply list (symbol-repr "btuple") (map #(AST->lisb % args) (.getList node))))
(defmethod AST->lisb ADomainExpression [node args] (list (symbol-repr "bdom") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb ARangeExpression [node args] (list (symbol-repr "bran") (AST->lisb (.getExpression node) args)))
;; identity relation
(defmethod AST->lisb ADomainRestrictionExpression [node args] (left-right node "b<|" args))
(defmethod AST->lisb ADomainSubtractionExpression [node args] (left-right node "b<<|" args))
(defmethod AST->lisb ARangeRestrictionExpression [node args] (left-right node "b|>" args))
(defmethod AST->lisb ARangeSubtractionExpression [node args] (left-right node "b|>>" args))
(defmethod AST->lisb AReverseExpression [node args] (list (symbol-repr "binverse") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AImageExpression [node args] (left-right node "bimage" args))
(defmethod AST->lisb AOverwriteExpression [node args] (left-right node "b<+" args))
(defmethod AST->lisb ADirectProductExpression [node args] (left-right node "b><" args))
(defmethod AST->lisb ACompositionExpression [node args] (left-right node "bcomp" args))
(defmethod AST->lisb AParallelProductExpression [node args] (left-right node "b||" args))
(defmethod AST->lisb AFirstProjectionExpression [node args] (list (symbol-repr "bprj1")
                                                                  (AST->lisb (.getExp1 node) args)
                                                                  (AST->lisb (.getExp2 node) args)))
(defmethod AST->lisb ASecondProjectionExpression [node args] (list (symbol-repr "bprj2")
                                                                   (AST->lisb (.getExp1 node) args)
                                                                   (AST->lisb (.getExp2 node) args)))
;; closure1
;; closure
(defmethod AST->lisb AIterationExpression [node args] (left-right node "biterate" args))
;; fnc
;; rel


;;; Functions
(defmethod AST->lisb APartialFunctionExpression [node args] (left-right node "b+->" args))
(defmethod AST->lisb ATotalFunctionExpression [node args] (left-right node "b-->" args))
(defmethod AST->lisb APartialSurjectionExpression [node args] (left-right node "b+->>" args))
(defmethod AST->lisb ATotalSurjectionExpression [node args] (left-right node "b-->>" args))
(defmethod AST->lisb APartialInjectionExpression [node args] (left-right node "b>+>" args))
(defmethod AST->lisb ATotalInjectionExpression [node args] (left-right node "b>->" args))
(defmethod AST->lisb APartialBijectionExpression [node args] (left-right node "b>+>>" args))
(defmethod AST->lisb ATotalBijectionExpression [node args] (left-right node "b>->>" args))
(defmethod AST->lisb ALambdaExpression [node args] (list (symbol-repr "blambda")
                                                         (identifier-list (.getIdentifiers node) args)
                                                         (AST->lisb (.getPredicate node) args)
                                                         (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AFunctionExpression [node args] (apply list (symbol-repr "bapply")
                                                                 (AST->lisb (.getIdentifier node) args)
                                                                 (identifier-list (.getParameters node) args)))







(defmethod AST->lisb Start [node args] (AST->lisb (.getPParseUnit node) args))
(defmethod AST->lisb APredicateParseUnit [node args] (AST->lisb (.getPredicate node) args))
(defmethod AST->lisb AExpressionParseUnit [node args] (AST->lisb (.getExpression node) args))
(defmethod AST->lisb ADefinitionPredicate [node args] (apply list (symbol-repr "bcall")
                                                                  (.. node getDefLiteral getText)
                                                                  (identifier-list (.getParameters node) args)))
(defmethod AST->lisb ADefinitionExpression [node args] (apply list (symbol-repr "bcall")
                                                                   (.. node getDefLiteral getText)
                                                                   (identifier-list (.getParameters node) args)))




(defn bstr->lisb
  ([s] (bstr->lisb s #{}))
  ([s args]
   (let [ast (de.be4.classicalb.core.parser.BParser/parse (str "#FORMULA " s))]
     (AST->lisb ast args))))


(defn parse-bmachine [filename]
  (.parseFile (de.be4.classicalb.core.parser.BParser.) (java.io.File. filename) false))

(defn extract-definitions-clause [ast]
  (->> ast
       .getPParseUnit
       .getMachineClauses
       (filter #(instance? de.be4.classicalb.core.parser.node.ADefinitionsMachineClause %))
       first))

(defn extract-predicates-from-clause [clause]
  (filter #(instance? de.be4.classicalb.core.parser.node.APredicateDefinitionDefinition %) (.getDefinitions clause)))


(defn prepare-predicate-definition [preddef]
  {:name (.. preddef getName getText)
   :params (->> preddef .getParameters (map #(.getText (first (.getIdentifier %)))))
   :predicate (.getRhs preddef)})

(defn prepared-predef-to-lisb [{:keys [name params predicate]}]
  (list (symbol-repr "defpred")
        (symbol name)
        (vec (map symbol params))
        (AST->lisb predicate (set params))))


(defn bmachine->lisbfile
  [bmachine-path output-path]
  (with-open [out (clojure.java.io/writer output-path)]
   (doall
     (->> bmachine-path
          parse-bmachine
          extract-definitions-clause
          extract-predicates-from-clause
          (map prepare-predicate-definition)
          (map prepared-predef-to-lisb)
          (map #(clojure.pprint/pprint % out)))))
  nil)
