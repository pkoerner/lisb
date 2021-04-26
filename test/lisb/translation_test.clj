(ns lisb.translation-test
  (:require [clojure.test :refer :all]
            [lisb.translation :refer :all]
            [lisb.ast2lisb :refer :all]))

(import de.be4.classicalb.core.parser.BParser)
(def bparser (new BParser))

(defn parse-b-machine [b-machine]
  (.parse bparser b-machine false))

(defn parse-b-formula [b-formula] (.parse bparser (str "#FORMULA " b-formula) false))
;(defn b-expression->lisb [b-expression]  (b->lisb (str "#EXPRESSION " b-expression)))
;(defn b-substitution->lisb [b-substitution]  (b->lisb (str "#SUBSTITUTION " b-substitution)))
;(defn b-transition->lisb [b-transition] (b->lisb (str "#TRANSITION " b-transition)))
;(defn b-predicate->lisb [b-predicate] (b->lisb (str "#PREDICATE " b-predicate)))

(import de.be4.classicalb.core.parser.util.PrettyPrinter)
(defn print-machine-from-ast [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (println (.getPrettyPrint pprinter))))
(defn get-machine-from-ast [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (.getPrettyPrint pprinter)))

(import de.be4.classicalb.core.parser.visualisation.ASTPrinter)
(def printer (ASTPrinter.))
(defn print-bmachine [b-machine]
  (let [ast (parse-b-machine b-machine)]
    (.apply ast printer)))
(defn print-ast [ast]
  (apply ast printer))

;;; parse units

(deftest machine-test
  (testing "empty machine"
    (is (= "MACHINE Empty\nEND"
           (get-machine-from-ast (parse-b-machine "MACHINE Empty\nEND"))))
    (is (= "MACHINE Empty\nEND"
           (get-machine-from-ast (node-repr->ast (b->lisb "MACHINE Empty\nEND")))))))

;;; machine clauses

#_(deftest constants-test
    (testing "constants"
      (let [bmachine (slurp (clojure.java.io/resource "machines/Constant.mch"))]
        (is (= (get-machine-from-ast (parse-b-machine bmachine)) ; formats bmachine
               (get-machine-from-ast (node-repr->ast (b->lisb bmachine))))))))


(deftest variables-test
  (testing "variables"
    (let [bmachine (slurp (clojure.java.io/resource "machines/Variable.mch"))]
      (is (= (get-machine-from-ast (parse-b-machine bmachine)) ; formats bmachine
             (get-machine-from-ast (node-repr->ast (b->lisb bmachine))))))))

(deftest sets-test
  (testing "sets"
    (let [empty-set "{}"
          set-enum1 "{E}"
          set-enum2 "{E,F}"
          set-enum2-alt "{F,E}"]
      (is (= empty-set
             (get-machine-from-ast (node-repr->ast (b-formula->lisb empty-set)))))
      (is (= set-enum1
             (get-machine-from-ast (node-repr->ast (b-formula->lisb set-enum1)))))
      (is (let [machine-set-enum2 (get-machine-from-ast (node-repr->ast (b-formula->lisb set-enum2-alt)))] ; order doesn't matter
            (or (= machine-set-enum2 set-enum2) (= machine-set-enum2 set-enum2-alt)))))))

(deftest equality-predicates-test
  (testing "equality-predicates"
    (let [equal "TRUE = FALSE"
          unequal "TRUE /= FALSE"]
      (is (= equal (print-ast (node-repr->ast (b-predicate->lisb equal))))))))