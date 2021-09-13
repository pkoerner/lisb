(ns lisb.prob.animator
  (:import com.google.inject.Guice
           com.google.inject.Stage
           de.prob.MainModule
           de.prob.scripting.Api
           de.prob.animator.command.EvaluateFormulaCommand
           de.prob.animator.domainobjects.ClassicalB
           de.prob.animator.domainobjects.FormulaExpand
           de.prob.animator.domainobjects.EvalResult
           de.prob.animator.domainobjects.ComputationNotCompletedResult
           de.prob.animator.domainobjects.EnumerationWarning))


; XXX load an instance of MainModule.class to ensure Prob 2.0 is properly loaded.
; Among other things this sets prob.home to load files from the ProB stdlib.
(def injector (Guice/createInjector Stage/PRODUCTION [(MainModule.)]))


(def api (.getInstance injector Api))


(defn state-space [ast]
  (.b_load api ast))


(defmulti get-result type)

(defmethod get-result EvalResult [v]
  (if (= "time_out" (.getValue v))
    :timeout
    (let [result (.translate v)
          free (.getKeys result)]
      (when (.. result getValue booleanValue)
        (into {} (map (fn [k][k (.getSolution result k)]) free))))))

(defmethod get-result ComputationNotCompletedResult [v]
  (let [reason (.getReason v)]
    (when (not= reason "contradiction found")
      (throw (Exception. reason)))))

(defmethod get-result EnumerationWarning [v]
  (let [message (.toString v)]
    (throw (Exception. ^String message))))


(defn eval-formula
  ([state-space formula-ast]
   (let [eval-element (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")
         cmd (EvaluateFormulaCommand. eval-element "root")
         _ (.execute state-space cmd)]
     (get-result (.getValue cmd)))))
