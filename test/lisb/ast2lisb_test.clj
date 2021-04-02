(ns lisb.ast2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.ast2lisb :refer :all]
            [lisb.representation :refer :all]))

(import de.be4.classicalb.core.parser.BParser)
(def bparser (new BParser))
(defn parse-b-machine [b-machine]
  (.parse bparser b-machine false))

(import de.be4.classicalb.core.parser.visualisation.ASTPrinter)
(def printer (ASTPrinter.))
(defn print-ast [b-machine]
  (let [ast (parse-b-machine b-machine)]
    (.apply ast printer)))

(deftest machine-test
  (testing "machine"
    (is
      (=
        (bmachine
          (bmachine-variant)
          (bmachine-header '(:Empty) ()))
        (bmachinestr->lisb "MACHINE Empty\nEND")))))

(deftest variable-test
  (testing "variable"
    (let [bmachine-input (slurp (clojure.java.io/resource "machines/Variable.mch"))]
      (is (= (bmachine
               (bmachine-variant)
               (bmachine-header (list :Variable) ())
               (bvariables (bidentifiers :nat))
               (binvariants (bmember (bidentifiers :nat) (bnat-set)))
               (binit (bblock (bassign (list (bidentifiers :nat)) (list 0)))))
             (bmachinestr->lisb bmachine-input))))))
