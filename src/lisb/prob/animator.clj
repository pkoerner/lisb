(ns lisb.prob.animator
  (:require [lisb.prob.retranslate :refer [retranslate]])
  (:require [lisb.translation.lisb2ir :refer [b=]]) ;; TODO: change this to avoid cyclic dependencies
  (:require [lisb.translation.util :refer [ir->b]]) ;; TODO: change this to avoid cyclic dependencies
  (:import com.google.inject.Guice
           com.google.inject.Stage
           de.prob.MainModule
           de.prob.scripting.Api
           de.prob.animator.command.EvaluateFormulasCommand
           de.prob.animator.domainobjects.ClassicalB
           de.prob.animator.domainobjects.FormulaExpand
           de.prob.animator.domainobjects.EvalResult
           de.prob.animator.domainobjects.ComputationNotCompletedResult
           de.prob.animator.domainobjects.EnumerationWarning
           de.prob.statespace.State
           ))


; XXX load an instance of MainModule.class to ensure Prob 2.0 is properly loaded.
; Among other things this sets prob.home to load files from the ProB stdlib.
(def injector (Guice/createInjector Stage/PRODUCTION [(MainModule.)]))


(def api (.getInstance injector Api))


(defn state-space! [ast]
  (.b_load api ast))


(defmulti get-result type)

(defmethod get-result EvalResult [v]
  (if (= "time_out" (.getValue v))
    :timeout
    (let [result (.translate v)
          free (.getKeys result)]
      (if (instance? de.hhu.stups.prob.translator.BBoolean (.getValue result))
        (when (.. result getValue booleanValue)
          (into {} (map (fn [k] [k (retranslate (.getSolution result k))]) free)))
        (retranslate (.getValue result))))))

(defmethod get-result ComputationNotCompletedResult [v]
  (let [reason (.getReason v)]
    (when (not= reason "contradiction found")
      (throw (Exception. reason)))))

(defmethod get-result EnumerationWarning [v]
  (let [message (.toString v)]
    (throw (Exception. ^String message))))


(defn eval-formula
  ([state-space formula-ast]
   (let [eval-element (ClassicalB. formula-ast FormulaExpand/EXPAND "")
         cmd (EvaluateFormulasCommand. (list eval-element) "root")
         _ (.execute state-space cmd)]
     (get-result (first (.getValues cmd))))))


(defn root-state [state-space]
  (.getRoot state-space))

(defn translate-transition [trans]
  (if (seq (.getParameterValues trans))
    [(keyword (.getName trans)) 
     (zipmap (.getParameterNames trans) (.getParameterValues trans))]
    (keyword (.getName trans))))

(defn get-transitions [state]
  (.explore state)
  (let [transs (.getTransitions state)]
    (map translate-transition transs)))

(defn successor 
  ([state op-kw]
   (successor state op-kw {}))
  ([state op-kw parameter-map]
  (.perform state (name op-kw)
            (into-array String
                        (map (fn [[k v]] (ir->b (b= k v)))
                             parameter-map)))))

;; TODO: states should probably be proxy objects
;; that print as maps but delegate the relevant methods to the original state object

(defn reify-mappy [state]
  (reify
    clojure.lang.ILookup
    (valAt [this k]
      (.valAt this k nil))
    (valAt [this k not-found]
      (let [m (.getVariableValues state FormulaExpand/TRUNCATE)
            formalism (type (first (keys m)))
            key-var (first (filter #(= (.getCode %) (name k)) (keys m)))
            ;; TODO use this line instead once the AbstractEvalElement is fixed
            ;key-var (clojure.lang.Reflector/invokeConstructor formalism (into-array [(name k)]))
            ]
        (def vv (get m key-var))
        (if key-var
          (retranslate (de.hhu.stups.prob.translator.Translator/translate (.getValue (get m key-var))))
          not-found)))
    ))

(defmethod clojure.core/print-method State [this writer]
  (print-simple 
    (into {} (map (fn [[k v]]
                    [(keyword (.getCode k))
                     (if-not (instance? de.prob.animator.domainobjects.IdentifierNotInitialised v)
                       (.getValue v) 
                       :prob/uninitialised)]) 
                  (concat (.getConstantValues this FormulaExpand/EXPAND)
                          (.getVariableValues this FormulaExpand/EXPAND))))
    writer))

(comment
  (do
  (use 'clojure.reflect)
  (use 'clojure.pprint)
  (def rr (comp print-table :members reflect))
  (use 'lisb.examples.sebastian)
  (use 'lisb.translation.util)
  (def ss (state-space! (ir->ast generic-timer-mc)))
  ; (print-table (:members (reflect ss)))
  ; (.getValues (.getRoot ss))
  (import 'de.prob.animator.domainobjects.EvalOptions)
  (import 'de.prob.animator.domainobjects.FormulaExpand)
  ; (.getVariableValues (.getRoot ss) EvalOptions/DEFAULT)
  ; (clojure.repl/pst)
  (.explore (.getRoot ss))
  ; (rr (root-state ss))
  (.getTransitions (.getRoot ss))
  ; (rr (first (get-transitions (root-state ss))))
  (def stat (successor (root-state ss) :$initialise_machine)))
  (.getVariableValues stat FormulaExpand/EXPAND)
  (type (first (keys (.getVariableValues stat FormulaExpand/EXPAND))))
  (rr stat)
  (do stat)

  (clojure.lang.Reflector/invokeConstructor f (into-array [(ir->ast :x)]))
  (get (reify-mappy stat) :curDeadlines)
  
  (retranslate (.getValue (.translate vv)))
  [key-var m]

  (do stat)

  (def ss2 (state-space! (b->ast (slurp "/home/philipp/CAN_BUS_tlc.mch"))))

  (successor (.getRoot ss2) :$setup_constants)
  (def st (-> (.getRoot ss2)
      (successor :$setup_constants)
      (successor :$initialise_machine)
      ;(successor :Update "0")
      ;get-transitions
  ;    reify-mappy
  ;    :T1_state
      ))
  (get-transitions st)
  (.perform st "Update" (into-array String ["pmax=0"]))
  (successor st "Update" {:pmax 0})

  ;; TODO: how much translation of values is desirable?
  ;; TODO: what is a good way to make states (accessing values) and transtions (getting successors) feel clojure-y?

  )
