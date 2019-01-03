(ns lisb.core
  (:require [lisb.frontends.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]])
  (:import de.prob.Main
           de.prob.scripting.Api
           de.prob.animator.command.EvaluateFormulaCommand
           de.prob.animator.domainobjects.ClassicalB
           de.prob.animator.domainobjects.EvalResult
           de.prob.animator.domainobjects.ComputationNotCompletedResult
           (de.be4.classicalb.core.parser.node Start
                                               EOF
                                               APredicateParseUnit)))



; XXX load an instance of Main.class to ensure Prob 2.0 is properly loaded.
; Among other things this sets prob.home to load files from the ProB stdlib.
(def prob-main (.getInstance (Main/getInjector) Main))

(defn get-api [] (.getInstance (Main/getInjector) Api))

(defn create-empty-machine []
  (let [tf (java.io.File/createTempFile "evalb" ".mch" nil)
        tn (.getAbsolutePath tf)
        ]
    (.deleteOnExit tf)
    (spit tf "MACHINE empty \n DEFINITIONS \"CHOOSE.def\" \n  END")
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


(defmulti get-result type)

(defmethod get-result EvalResult [v]
  (if (= "time_out" (.getValue v))
    :timeout
    (let [result (.translate v)
          free (.getKeys result)]
      (when (.. result getValue booleanValue)
        (into {} (map (fn [k][k (.getSolution result k)]) free))))))

(defmethod get-result ComputationNotCompletedResult [[v _]]
  (let [reason-string (.getReason v)]
    (when (not= reason-string "contradiction found")
      (throw (Exception. reason-string)))))


(defonce ^:private secret-state-space (state-space))

(defn eval
  ([ast]
    (eval secret-state-space ast))
  ([state-space ast]
    (let [cmd (EvaluateFormulaCommand. (predicate ast) "root")
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
   (eval (to-ast c)))
  ([c & r]
   (eval (to-ast (apply band c r)))))

(defn timeout-conjuncts?
  ([c]
   (= :timeout (eval (to-ast c))))
  ([c & r]
   (= :timeout (eval (to-ast (apply band c r))))))

(defn unsat-core-aux [sat? c]
  (let [poss (choose-rest c)
        [_ r] (first (drop-while (comp sat?
                                       second)
                                 poss))]
    (if r
      (unsat-core-aux sat? r)
      (set c))))


(defn unsat-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (not (eval (to-ast (apply band conjuncts))))]}
  (unsat-core-aux (partial apply sat-conjuncts?) conjuncts))


(defn unsat-core-predicate [p c]
  {:pre [(not (eval (to-ast (p c))))
         (set? c)]}
  (unsat-core-aux #(eval (to-ast (p (set %)))) c))

(defn timeout-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (= :timeout (eval (to-ast (apply band conjuncts))))]}
  (unsat-core-aux (partial apply (complement timeout-conjuncts?)) conjuncts))
