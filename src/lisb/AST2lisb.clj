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
    (.append sb " ")
    (.apply arg instance))
  (.append sb ")"))


(defn get-visitor [sb]
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

    (applyLeftAssociative [left node right append]
      (translate-to-lisb this sb append left right))
    (applyRightAssociative [left node right append]
      (.applyLeftAssociative this left node right append))

    (caseADivExpression          [node] (overwrite-appendstr this node "div"))
    (caseAMaxExpression          [node] (translate-to-lisb this sb "max" (.getExpression node)))
    (caseAMinExpression          [node] (translate-to-lisb this sb "min" (.getExpression node)))

    (caseAConjunctPredicate      [node] (overwrite-appendstr this node "and"))
    (caseADisjunctPredicate      [node] (overwrite-appendstr this node "or"))
    (caseANegationPredicate      [node] (translate-to-lisb this sb "not" (.getPredicate node)))

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

    (caseADomainExpression       [node] (translate-to-lisb this sb "dom" (.getExpression node)))
    (caseARangeExpression        [node] (translate-to-lisb this sb "ran" (.getExpression node)))
    (caseAImageExpression        [node] (translate-to-lisb this sb "image" (.getLeft node) (.getRight node)))
    (caseAReverseExpression      [node] (translate-to-lisb this sb "inverse" (.getExpression node)))
    ))

(defn fresh-visitor []
  (get-visitor (StringBuffer.)))

(defn parse-print [s]
  (let [ast (de.be4.classicalb.core.parser.BParser/parse (str "#FORMULA " s))
        v (fresh-visitor)]
    (.apply ast v)
    (.getPrettyPrint v)))

(def parse-print-read
  (comp read-string parse-print))

