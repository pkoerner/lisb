(ns lisb.translation.ast2b
  (:import (de.be4.classicalb.core.parser.util PrettyPrinter)))

(defn ast->b [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (.getPrettyPrint pprinter)))
