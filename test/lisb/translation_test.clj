(ns lisb.translation-test
  (:require [clojure.test :refer :all]
            [lisb.translation :refer :all]
            [lisb.ast2lisb :refer :all]))

(import de.be4.classicalb.core.parser.BParser)
(def bparser (new BParser))
(defn parse-b-machine [b-machine]
  (.parse bparser b-machine false))

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
(defn print-ast [b-machine]
  (let [ast (parse-b-machine b-machine)]
    (.apply ast printer)))


(deftest machine-test
  (testing "empty machine"
    (is (= "MACHINE Empty\nEND"
           (get-machine-from-ast (parse-b-machine "MACHINE Empty\nEND"))))
    (is (= "MACHINE Empty\nEND"
           (get-machine-from-ast (lisb->ast (bmachinestr->lisb "MACHINE Empty\nEND")))))))


(deftest variable-test
  (testing "variable"
    (let [bmachine (slurp (clojure.java.io/resource "machines/Variable.mch"))]
      (is (= (get-machine-from-ast (parse-b-machine bmachine))
             (get-machine-from-ast (lisb->ast (bmachinestr->lisb bmachine))))))))
