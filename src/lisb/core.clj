(ns lisb.core
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [b->predicate-ast]])
  #_(:require [lisb.translationOLD :refer [to-ast]])
  (:import com.google.inject.Guice
           com.google.inject.Stage
           com.google.inject.Module
           de.prob.MainModule)
  (:import de.prob.Main
           de.prob.scripting.Api
           de.prob.animator.command.EvaluateFormulaCommand
           de.prob.animator.domainobjects.ClassicalB
           de.prob.animator.domainobjects.EvalResult
           de.prob.animator.domainobjects.ComputationNotCompletedResult
           (de.be4.classicalb.core.parser.node Start
                                               EOF
                                               APredicateParseUnit)))

(def prob-main (Guice/createInjector Stage/PRODUCTION [(MainModule.)]))

; XXX load an instance of Main.class to ensure Prob 2.0 is properly loaded.
; Among other things this sets prob.home to load files from the ProB stdlib.
#_(def prob-main (.getInstance (Main/getInjector) Main))

#_(defn get-api [] (.getInstance (Main/getInjector) Api))

#_(defn create-empty-machine []
  (let [tf (java.io.File/createTempFile "evalb" ".mch" nil)
        tn (.getAbsolutePath tf)
        ]
    (.deleteOnExit tf)
    (spit tf "MACHINE empty \n DEFINITIONS \"CHOOSE.def\" \n  END")
    tn))

#_(defn state-space []
  (let [machine (create-empty-machine)
        api (get-api)]
    (.b_load api machine)))





#_(defn predicate [ast]
  (let [p (APredicateParseUnit. ast)
        start (Start. p (EOF.))]
    (ClassicalB. start
                 de.prob.animator.domainobjects.FormulaExpand/truncate
                 "")))


(defmulti get-result type)

#_(defmethod get-result EvalResult [v]
  (if (= "time_out" (.getValue v))
    :timeout
    (let [result (.translate v)
          free (.getKeys result)]
      (when (.. result getValue booleanValue)
        (into {} (map (fn [k][k (.getSolution result k)]) free))))))

#_(defmethod get-result ComputationNotCompletedResult [[v _]]
  (let [reason-string (.getReason v)]
    (when (not= reason-string "contradiction found")
      (throw (Exception. reason-string)))))


#_(defonce ^:private secret-state-space (state-space))

#_(defn eval
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


#_(defn sat-conjuncts?
  ([c]
   (eval (to-ast c)))
  ([c & r]
   (eval (to-ast (apply band c r)))))

#_(defn timeout-conjuncts?
  ([c]
   (= :timeout (eval (to-ast c))))
  ([c & r]
   (= :timeout (eval (to-ast (apply band c r))))))

#_(defn unsat-core-aux [sat? c]
  (let [poss (choose-rest c)
        [_ r] (first (drop-while (comp sat?
                                       second)
                                 poss))]
    (if r
      (unsat-core-aux sat? r)
      (set c))))


#_(defn unsat-core [& conjuncts]
  #_{:pre [(seq (rest conjuncts))
         (not (eval (to-ast (apply band conjuncts))))]}
  (unsat-core-aux (partial apply sat-conjuncts?) conjuncts))


#_(defn unsat-core-predicate [p c]
  {:pre [(not (eval (to-ast (p c))))
         (set? c)]}
  (unsat-core-aux #(eval (to-ast (p (set %)))) c))

#_(defn timeout-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (= :timeout (eval (to-ast (apply band conjuncts))))]}
  (unsat-core-aux (partial apply (complement timeout-conjuncts?)) conjuncts))
