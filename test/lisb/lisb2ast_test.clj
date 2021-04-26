(ns lisb.lisb2ast-test
  (:require [clojure.test :refer :all]
            [lisb.lisb2ast :refer :all]))

(import de.be4.classicalb.core.parser.visualisation.ASTPrinter)
(def printer (ASTPrinter.))
(defn print-ast [b-ast]
  (.apply b-ast printer))

(import de.be4.classicalb.core.parser.util.PrettyPrinter)
(defn get-machine-from-ast [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (.getPrettyPrint pprinter)))

(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                  "TRUE=FALSE" '(= true false)
                  "TRUE/=FALSE" '(not= true false)
                  )))

#_(defn wtf [lisb]
  (eval (concat '(let [+ *]) (list lisb))))

#_(defn wtf2 [lisb]
  (eval (conj (list lisb) '[+ *] 'let)))
