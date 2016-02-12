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
                                               ANotMemberPredicate
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
                                               ADefinitionsMachineClause 
                                               ADefinitionFileParseUnit 
                                               APredicateDefinitionDefinition
                                               AExpressionDefinitionDefinition
                                               AFileDefinitionDefinition
                                               AAbstractMachineParseUnit 
                                               AStringSetExpression 
                                               )))



(def symbol-repr (partial symbol "lisb.representation"))

(declare AST->lisb read-bmachine)

(defn left-right [node name args]
  (list (symbol-repr name) (AST->lisb (.getLeft node) args) (AST->lisb (.getRight node) args)))

(defn identifier-list [l args]
  (map #(AST->lisb % args) l))

(defn collect-left-associative [node args]
  (let [c (class node)]
    (if (= c (class (.getLeft node)))
      (conj (collect-left-associative (.getLeft node) args) (.getRight node))
      [(.getLeft node) (.getRight node)])))

(defn multi-arity [name node args]
  (apply list
         (symbol-repr name)
         (map #(AST->lisb % args) (collect-left-associative node args))))

(defmulti AST->lisb (fn [node args] (class node)))
(defmethod AST->lisb AIntegerExpression [node _] (Long/parseLong (.. node getLiteral getText)))
(defmethod AST->lisb AIdentifierExpression [node args] (let [id (.getText (first (.getIdentifier node)))]
                                                         (if ((:symbols args) id) (symbol id) (keyword id))))

(defmethod AST->lisb AStringExpression [node _] (.. node getContent getText))
(defmethod AST->lisb AStringSetExpression [_ _] (list (symbol-repr "bstring-set")))

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
(defmethod AST->lisb AAddExpression [node args] (multi-arity "b+" node args))
(defmethod AST->lisb AMinusOrSetSubtractExpression [node args] (multi-arity "bminus-or-set-subtract" node args))
(defmethod AST->lisb AUnaryMinusExpression [node args] (list (symbol-repr "bminus") (AST->lisb (.getExpression node) args)))
(defmethod AST->lisb AMultOrCartExpression [node args] (multi-arity "b*" node args))
(defmethod AST->lisb ADivExpression [node args] (multi-arity "bdiv" node args))
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
(defmethod AST->lisb AConjunctPredicate [node args] (multi-arity "band" node args))
(defmethod AST->lisb ADisjunctPredicate [node args] (multi-arity "bor" node args))
(defmethod AST->lisb AImplicationPredicate [node args] (multi-arity "b=>" node args))
(defmethod AST->lisb AEquivalencePredicate [node args] (multi-arity "b<=>" node args))
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
(defmethod AST->lisb AUnionExpression [node args] (multi-arity "bunion" node args))
(defmethod AST->lisb AIntersectionExpression [node args] (multi-arity "bintersection" node args))
(defmethod AST->lisb AMemberPredicate [node args] (left-right node "bmember" args))
(defmethod AST->lisb ANotMemberPredicate [node args] (left-right node "bnot-member" args))
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

(defmethod AST->lisb ARecordFieldExpression [node args] (list (symbol-repr "brec-get")
                                                              (AST->lisb (.getRecord node) args)
                                                              (AST->lisb (.getIdentifier node) args)))

(defmethod AST->lisb AStructExpression [node args] (list (symbol-repr "bstruct")
                                                         (into #{} (map (fn [recentry] [(AST->lisb (.getIdentifier recentry) args)
                                                                                        (AST->lisb (.getValue recentry) args)]) (.getEntries node)))))
(defmethod AST->lisb ASetSubtractionExpression [node args] (multi-arity "bset-" node args))



(defmethod AST->lisb AFileDefinitionDefinition [node args]
  (let [filename (AST->lisb (.getFilename node) args)
        path (str (.getParent (java.io.File. (:path args)))
                  java.io.File/separator
                  filename)]
    (when-not ((:ignore args) filename)
      {filename (read-bmachine path (:ignore args))})))

(defmethod AST->lisb TStringLiteral [node _]
  (.getText node))


(defn definition [node args]
  (let [name (.. node getName getText)
        params (->> node .getParameters (map #(.getText (first (.getIdentifier %)))))
        predicate (.getRhs node)]
    (list (symbol-repr "defpred")
          (symbol name)
          (vec (map symbol params))
          (AST->lisb predicate (assoc args :symbols (set params))))))

(defmethod AST->lisb APredicateDefinitionDefinition [node args]
  (definition node args))

(defmethod AST->lisb AExpressionDefinitionDefinition [node args]
  (definition node args))

(defmethod AST->lisb AAbstractMachineParseUnit [node args]
  (let [definition-clause (first (filter #(instance? ADefinitionsMachineClause %)
                                         (.getMachineClauses node)))]
    (AST->lisb definition-clause args)))

(defmethod AST->lisb ADefinitionFileParseUnit [node args]
  (AST->lisb (.getDefinitionsClauses node) args))

(defmethod AST->lisb ADefinitionsMachineClause [node args]
  (map #(AST->lisb % args) (.getDefinitions node)))







(defn bstr->lisb
  ([s] (bstr->lisb s #{}))
  ([s args]
   (let [ast (de.be4.classicalb.core.parser.BParser/parse (str "#FORMULA " s))]
     (AST->lisb ast {:symbols args}))))


(defn parse-bmachine [filename]
  (.parseFile (de.be4.classicalb.core.parser.BParser.) (java.io.File. filename) false))

(defn read-bmachine [bmachine-path ignore-definition-files]
  (-> bmachine-path
      parse-bmachine
      (AST->lisb {:path bmachine-path
                  :ignore ignore-definition-files})))

(defn bmachine->lisbfile [bmachine output-path]
  (with-open [out (clojure.java.io/writer output-path)]
    (doall (map #(clojure.pprint/pprint % out) bmachine)))
  nil)


(defn filename-without-extension [s]
  (first (re-seq #"[^.]*" s)))

(defn namespacify-preds [prefix name dependencies c]
  (conj c (list
            'ns
            (symbol (str prefix "." name))
            (apply list
                   :use
                   ['lisb.representation]
                   (map (comp vector symbol (partial str prefix ".") filename-without-extension) dependencies)))))

(defn process-definitions-tree [filename data]
  (reduce (fn deftree-r [acc [filename data]]
            (let [included (apply merge (filter map? data))
                  preds (filter sequential? data)]
              (-> (reduce deftree-r acc included) 
                  (assoc-in [:files filename] preds)
                  (assoc-in [:depends-on filename] (keys included)))))
          {:files {}
           :depends-on {}}
          {filename data}))

(defn bpath->lisbfiles [bmachine ignore output-folder prefix]
  (.mkdirs (java.io.File. output-folder))
  (let [tree (read-bmachine bmachine ignore)
        r (process-definitions-tree (.getName (java.io.File. bmachine)) tree)]
    (doseq [[fname preds] (:files r)]
      (let [without-ext (filename-without-extension fname)
            newname (str without-ext ".clj")]
        (bmachine->lisbfile (namespacify-preds prefix without-ext (get-in r [:depends-on fname]) preds)
                            (str output-folder java.io.File/separator newname))))))


