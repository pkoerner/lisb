(ns lisb.core
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [b->ast b->predicate-ast]])
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

(defn empty-state-space []
  (let [empty-machine-ast (b->ast bempty-machine)]
    (.b_load api empty-machine-ast)))

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
    (throw (Exception. message))))

(defn eval-formula
  ([ast] (eval-formula (empty-state-space) ast))
  ([state-space ast] (let [eval-element (ClassicalB. ast FormulaExpand/TRUNCATE "")
                           cmd (EvaluateFormulaCommand. eval-element "root")
                           _ (.execute state-space cmd)]
                       (get-result (.getValue cmd)))))

(defn choose-rest [c]
  (let [n (count c)]
    (->> (concat c c)
         (partition n 1)
         (map (fn [[h & t]] [h t]))
         butlast)))

(defn sat-conjuncts?
  ([c]
   (eval-formula (b->predicate-ast c)))
  ([c & r]
   (eval-formula (b->predicate-ast (apply band c r)))))

(defn timeout-conjuncts?
  ([c]
   (= :timeout (eval-formula (b->predicate-ast c))))
  ([c & r]
   (= :timeout (eval-formula (b->predicate-ast (apply band c r))))))

(defn unsat-core-aux [sat? c]
  (let [poss (choose-rest c)
        [_ r] (first (drop-while
                       (comp sat? second)
                       poss))]
    (if r
      (unsat-core-aux sat? r)
      (set c))))


(defn unsat-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (not (eval-formula (b->predicate-ast (apply band conjuncts))))]}
  (unsat-core-aux (partial apply sat-conjuncts?) conjuncts))


(defn unsat-core-predicate [p c]
  {:pre [(not (eval-formula (b->predicate-ast (p c))))
         (set? c)]}
  (unsat-core-aux #(eval-formula (b->predicate-ast (p (set %)))) c))

(defn timeout-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (= :timeout (eval-formula (b->predicate-ast (apply band conjuncts))))]}
  (unsat-core-aux (partial apply (complement timeout-conjuncts?)) conjuncts))
