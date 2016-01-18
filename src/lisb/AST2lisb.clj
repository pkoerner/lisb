(ns lisb.AST2lisb)

(defn overwrite-appendstr [instance node s]
  (.applyLeftAssociative instance
                         (.getLeft node)
                         node
                         (.getRight node)
                         s))

(defn get-visitor [sb]
  (proxy [de.prob.model.classicalb.PrettyPrinter] []
    (getPrettyPrint []
      (.toString sb))

    (caseAIntegerExpression [expr]
      (.append sb (.. expr getLiteral getText)))

    (caseABooleanTrueExpression [expr]
      (.append sb " true "))

    (caseABooleanFalseExpression [expr]
      (.append sb " false "))


    (caseAEmptySetExpression [_]
      (.append sb "#{}"))

    (caseASetExtensionExpression [node]
      (.append sb "#{")
      (doseq [n (.getExpressions node)]
        (.apply n this)
        (.append sb " "))
      (.append sb "}"))

    (caseACoupleExpression [node]
      (.append sb "(bcouple ")
      (.apply (first (.getList node)) this)
      (.append sb " ")
      (.apply (second (.getList node)) this)
      (.append sb ")"))

    (applyLeftAssociative [left node right append]
      (.append sb "(b")
      (.append sb (clojure.string/trim append))
      (.append sb " ")
      (.apply left this)
      (.append sb " ")
      (.apply right this)
      (.append sb ")"))  

    (applyRightAssociative [left node right append]
      (.applyLeftAssociative this left node right append))

    (caseAConjunctPredicate [node]
      (overwrite-appendstr this node "and"))
    (caseADisjunctPredicate [node]
      (overwrite-appendstr this node "or"))
    (caseAUnionExpression [node]
      (overwrite-appendstr this node "union"))
    (caseAIntersectionExpression [node]
      (overwrite-appendstr this node "intersection"))))

(defn fresh-visitor []
  (get-visitor (StringBuffer.)))

(defn parse-print [s]
  (let [ast (de.be4.classicalb.core.parser.BParser/parse (str "#FORMULA " s))
        v (fresh-visitor)]
    (.apply ast v)
    (.getPrettyPrint v)))

(def parse-print-read
  (comp read-string parse-print))

