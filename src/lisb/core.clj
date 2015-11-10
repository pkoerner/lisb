(ns lisb.core
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]])
  (:import de.prob.Main
           de.prob.scripting.Api
           de.prob.animator.command.CbcSolveCommand
           de.prob.animator.domainobjects.ClassicalB
           (de.be4.classicalb.core.parser.node Start
                                               EOF
                                               APredicateParseUnit)))


(defn get-api [] (.getInstance (Main/getInjector) Api))

(defn create-empty-machine []
  (let [tf (java.io.File/createTempFile "evalb" ".mch" nil)
        tn (.getAbsolutePath tf)
        ]
    (.deleteOnExit tf)
    (spit tf "MACHINE empty \n END")
    tn))

(defn state-space []
  (let [machine (create-empty-machine)
        api (get-api)]
    (.b_load api machine)))





(defn predicate [ast]
  (let [p (APredicateParseUnit. ast)
        start (Start. p (EOF.))]
    (ClassicalB. start)))


(defn eval [state-space ast]
  (let [cmd (CbcSolveCommand. (predicate ast))
        _ (.execute state-space cmd)
        free (.getFreeVariables cmd)
        result (.. cmd getValue translate)
        ]
    (when (.. result getValue booleanValue)
      (into {} (map (fn [k][k (.getSolution result k)]) free)))))

