(ns lisb.AST2lisb)

(defn overwrite-appendstr [instance node s]
  (.applyLeftAssociative instance
                         (.getLeft node)
                         node
                         (.getRight node)
                         s))


(defn translate-to-lisb [instance sb fname & args]
  (.append sb (str "(b" (clojure.string/trim fname)))
  (doseq [arg args]
    (if (instance? java.util.List arg)
      (do (.append sb "[")
          (doseq [e arg] (.append sb " ") (.apply e instance))
          (.append sb "]"))
      (do (.append sb " ")
          (.apply arg instance))))
  (.append sb ")"))


(defn get-visitor [sb symbols]
  (proxy [de.prob.model.classicalb.PrettyPrinter] []
    (getPrettyPrint []
      (.toString sb))

    (caseAIntegerExpression [expr] (.append sb (.. expr getLiteral getText)))
    (caseANatSetExpression      [_] (.append sb "(bnat-set)"))
    (caseANat1SetExpression     [_] (.append sb "(bnat1-set)"))
    (caseANaturalSetExpression  [_] (.append sb "(bnatural-set)"))
    (caseANatural1SetExpression [_] (.append sb "(bnatural1-set)"))
    (caseAIntSetExpression      [_] (.append sb "(bint-set)"))
    (caseAIntegerSetExpression  [_] (.append sb "(binteger-set)"))

    (caseABooleanTrueExpression  [_] (.append sb " true "))
    (caseABooleanFalseExpression [_] (.append sb " false "))
    (caseABoolSetExpression      [_] (.append sb "(bbool-set)"))
    (caseAConvertBoolExpression  [node] (translate-to-lisb this sb "pred->bool" (.getPredicate node)))

    (caseAEmptySetExpression [_] (.append sb "#{}"))
    (caseASetExtensionExpression [node]
      (apply translate-to-lisb this sb "set-enum" (.getExpressions node)))

    (caseACoupleExpression [node] (apply translate-to-lisb this sb "couple" (.getList node)))

    (caseTIdentifierLiteral [node]
      (let [s (.getText node)]
        (.append sb (if (symbols s) s (str (keyword s))))))

    (applyLeftAssociative [left node right append]
      (translate-to-lisb this sb append left right))
    (applyRightAssociative [left node right append]
      (.applyLeftAssociative this left node right append))

    (caseAUnaryMinusExpression   [node] (translate-to-lisb this sb "-" (.getExpression node)))
    (caseADivExpression          [node] (overwrite-appendstr this node "div"))
    (caseAMaxExpression          [node] (translate-to-lisb this sb "max" (.getExpression node)))
    (caseAMinExpression          [node] (translate-to-lisb this sb "min" (.getExpression node)))
    (caseAIntervalExpression     [node] (translate-to-lisb this sb "interval" (.getLeftBorder node) (.getRightBorder node)))
    (caseAGeneralSumExpression   [node] (translate-to-lisb this sb "sigma" (.getIdentifiers node) (.getPredicates node) (.getExpression node)))
    (caseAGeneralProductExpression [node] (translate-to-lisb this sb "pi" (.getIdentifiers node) (.getPredicates node) (.getExpression node)))

    (caseAConjunctPredicate      [node] (overwrite-appendstr this node "and"))
    (caseADisjunctPredicate      [node] (overwrite-appendstr this node "or"))
    (caseANegationPredicate      [node] (translate-to-lisb this sb "not" (.getPredicate node)))
    (caseANotEqualPredicate      [node] (overwrite-appendstr this node "not="))

    (caseAForallPredicate        [node] (translate-to-lisb this sb "forall" (.getIdentifiers node) (.getImplication node)))
    (caseAExistsPredicate        [node] (translate-to-lisb this sb "exists" (.getIdentifiers node) (.getPredicate node)))

    (caseAComprehensionSetExpression [node] (translate-to-lisb this sb "set" (.getIdentifiers node) (.getPredicates node)))
    (caseASubsetPredicate        [node] (overwrite-appendstr this node "subset"))
    (caseASubsetStrictPredicate  [node] (overwrite-appendstr this node "subset-strict"))
    (caseAMemberPredicate        [node] (overwrite-appendstr this node "member"))
    (caseAUnionExpression        [node] (overwrite-appendstr this node "union"))
    (caseAIntersectionExpression [node] (overwrite-appendstr this node "intersection"))
    (caseAPowSubsetExpression    [node] (translate-to-lisb this sb "pow" (.getExpression node)))
    (caseAPow1SubsetExpression   [node] (translate-to-lisb this sb "pow1" (.getExpression node)))
    (caseAFinSubsetExpression    [node] (translate-to-lisb this sb "fin" (.getExpression node)))
    (caseAFin1SubsetExpression   [node] (translate-to-lisb this sb "fin1" (.getExpression node)))
    (caseACardExpression         [node] (translate-to-lisb this sb "count" (.getExpression node)))
    (caseAGeneralUnionExpression [node] (translate-to-lisb this sb "unite-sets" (.getExpression node)))
    (caseAGeneralIntersectionExpression [node] (translate-to-lisb this sb "intersect-sets" (.getExpression node)))
    (caseAQuantifiedUnionExpression [node] (translate-to-lisb this sb "union-pe" (.getIdentifiers node) (.getPredicates node) (.getExpression node)))
    (caseAQuantifiedIntersectionExpression [node] (translate-to-lisb this sb "intersection-pe" (.getIdentifiers node) (.getPredicates node) (.getExpression node)))

    (caseADomainExpression       [node] (translate-to-lisb this sb "dom" (.getExpression node)))
    (caseARangeExpression        [node] (translate-to-lisb this sb "ran" (.getExpression node)))
    (caseAImageExpression        [node] (translate-to-lisb this sb "image" (.getLeft node) (.getRight node)))
    (caseAReverseExpression      [node] (translate-to-lisb this sb "inverse" (.getExpression node)))
    (caseAFirstProjectionExpression [node] (translate-to-lisb this sb "prj1" (.getExp1 node) (.getExp2 node)))
    (caseASecondProjectionExpression [node] (translate-to-lisb this sb "prj2" (.getExp1 node) (.getExp2 node)))

    (caseALambdaExpression       [node] (translate-to-lisb this sb "lambda" (.getIdentifiers node) (.getPredicate node) (.getExpression node)))
    (caseAFunctionExpression     [node] (apply translate-to-lisb this sb "apply" (.getIdentifier node) (.getParameters node))) ;; FIXME: this breaks succ and pred...
    ))

(defn fresh-visitor
  ([] (get-visitor (StringBuffer.) #{}))
  ([symbol-set] (get-visitor (StringBuffer.) symbol-set)))

(defn parse-print
  ([s] (parse-print s #{}))
  ([s symbols]
    (let [ast (de.be4.classicalb.core.parser.BParser/parse (str "#FORMULA " s))
          v (fresh-visitor symbols)]
      (.apply ast v)
      (.getPrettyPrint v))))

(def parse-print-read
  (comp read-string parse-print))

