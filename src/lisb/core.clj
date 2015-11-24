(ns lisb.core
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]])
  (:import de.prob.Main
           de.prob.scripting.Api
           de.prob.animator.command.CbcSolveCommand
           de.prob.animator.domainobjects.ClassicalB
           de.prob.animator.domainobjects.EvalResult
           de.prob.animator.domainobjects.ComputationNotCompletedResult
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
    (ClassicalB. start
                 de.prob.animator.domainobjects.FormulaExpand/truncate
                 "")))


(defmulti get-result (comp type first))

(defmethod get-result EvalResult [[v free]]
  (let [result (.translate v)]
    (when (.. result getValue booleanValue)
      (into {} (map (fn [k][k (.getSolution result k)]) free)))))

(defmethod get-result ComputationNotCompletedResult [[v _]]
  (let [reason-string (.getReason v)]
    (when (not= reason-string "contradiction found")
      (throw (Exception. reason-string)))))



(defn eval [state-space ast]
  (let [cmd (CbcSolveCommand. (predicate ast))
        _ (.execute state-space cmd)
        free (.getFreeVariables cmd)]
    (get-result [(.getValue cmd) free])))

