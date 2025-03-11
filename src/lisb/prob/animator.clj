(ns lisb.prob.animator
  (:require [lisb.prob.java-api :refer :all])
  (:require [lisb.prob.retranslate :refer [retranslate]])
  (:require [lisb.translation.lisb2ir :refer [b= band bmember? bnot=]])
  (:require [lisb.translation.util :refer [ir->b ir->ast 
                                           b-expression->lisb b-expression->ir 
                                           b-predicate->ast]]) ;; TODO: change this to avoid cyclic dependencies
  (:require [clojure.pprint :refer [pprint]])
  (:import 
           de.prob.animator.command.EvaluateFormulasCommand
           de.prob.animator.command.FindStateCommand
           de.prob.animator.domainobjects.ClassicalB
           de.prob.animator.domainobjects.FormulaExpand
           de.prob.animator.domainobjects.EvalResult
           de.prob.animator.domainobjects.ComputationNotCompletedResult
           de.prob.animator.domainobjects.EnumerationWarning
           de.prob.statespace.State
           ))


; XXX load an instance of MainModule.class to ensure Prob 2.0 is properly loaded.
; Among other things this sets prob.home to load files from the ProB stdlib.

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


(defn bfile->b
  "Use this instead of slurping a machine file.
  Will load an .mch file with ProB and obtain an internal representation.
  Mainly used to circumvent definitions."
  [filename]
  (let [animator (state-space! filename)
        cmd (de.prob.animator.command.GetInternalRepresentationPrettyPrintCommand.)]
    (.execute animator cmd)
    (.getPrettyPrint cmd)))




(declare eval-formula' try-get-solutions)

(def default-eval-settings 
  {:vars-fn keyword
   :val-output :value
   :val-aggression 10 #_:lazy
   :tuples :lisb   ;; nothing implemented here
   :sequences set  ;; nothing implemented here
   :sets set       ;; nothing implemented here
   :functions set  ;; nothing implemented here
   :relations set  ;; nothing implemented here
   })


;; TODO: handle timeouts
(defn try-get-solutions 
  ;; TODO: n solutions can be obtained by using the external function CHOOSE_n
  ([bset ss] (try-get-solutions bset ss #{}))
  ([bset ss seen]
   (try-get-solutions bset ss seen default-eval-settings))
  ([bset ss seen opts]
   (lazy-seq 
     (let [freshkw (keyword (gensym "lisb__internal"))
           formula (apply band (bmember? freshkw bset)
                          (map #(bnot= freshkw %) seen))
           res (eval-formula' ss (ir->ast formula) opts)
;; TODO: the solution map maps *strings* to solutions
;;       even though the variables are keywords.
;;       I do not particularly like this. (pk, 05.06.2024)
           element (get res freshkw)] 
       ;(println :ele element)
       (if res
         (cons element (try-get-solutions bset ss (conj seen element) opts))
         ())))))


(defmulti handle-val-output (fn [kw ss opts] kw))
(defmethod handle-val-output :bstr [_ _ _opts] identity)
(defmethod handle-val-output :lisb [_ _ _opts] b-expression->lisb)
(defmethod handle-val-output :ir [_ _ _opts] b-expression->ir)
(defmethod handle-val-output :value [_ ss opts]
  (fn [v]
    (try (retranslate (de.hhu.stups.prob.translator.Translator/translate v))
         (catch Exception e
           (let [ir (b-expression->ir v)]
             (if (integer? (:val-aggression opts))
               (set (take (:val-aggression opts) (try-get-solutions ir ss)))
               (try-get-solutions ir ss)))))))

(defmulti get-result' (fn [res ss opts] (type res)))


(defmethod get-result' EvalResult [v ss opts]
  (case (.getValue v) 
    "time_out" :timeout
    "FALSE" nil
    "TRUE"
      (let [sol-map (.getSolutions v)]
        (into {} 
              (for [[k v] (remove (comp (:exclude opts #{}) #(.getKey %)) sol-map)] 
                [((:vars-fn opts) k) 
                 ((handle-val-output (:val-output opts) ss opts) v)])))
      ((handle-val-output (:val-output opts) ss opts)  (.getValue v))
      ))

(defmethod get-result' ComputationNotCompletedResult [v _ opts]
  (let [reason (.getReason v)]
    (when (not= reason "contradiction found")
      (throw (Exception. reason)))))

(defmethod get-result' EnumerationWarning [v _ opts]
  (let [message (.toString v)]
    (throw (Exception. ^String message))))

(defn eval-formula'
  ([state-space formula-ast]
   (eval-formula' state-space formula-ast default-eval-settings))
  ([state-space formula-ast settings]
   (let [settings (merge default-eval-settings settings)
         eval-element (ClassicalB. formula-ast FormulaExpand/EXPAND "")
         cmd (EvaluateFormulasCommand. (list eval-element) "root")
         _ (.execute state-space cmd)]
     (get-result' (first (.getValues cmd)) state-space settings))))




(declare wrap-state successor)




(defn successor 
  ([state op-kw]
   #_(successor state op-kw {})
   ;; TODO: what happens if findTransition has multiple possible successor states?
   (wrap-state (.getDestination (.findTransition state (name op-kw) []))))
  ([state op-kw parameter-map]
   (wrap-state (.perform state (name op-kw)
                         (into-array String
                                     (map (fn [[k v]] (ir->b (b= k v)))
                                          parameter-map))))))


(defn to-state [statespace bindings]
  (let [bindings' (filter (fn [[k _]] (and (keyword k) (not= "op" (namespace k)))) bindings)
        formula (apply band (map (fn [[id v]] (b= id v)) bindings'))
        formula-str (ir->b formula)
        fsc (FindStateCommand. statespace (ClassicalB. (b-predicate->ast formula-str) FormulaExpand/EXPAND "") false)
        _ (.execute statespace fsc)
        newstate (.getDestination (.getOperation fsc))]
    (wrap-state newstate)) )


(defn translate-transition 
  ([trans] (translate-transition trans false))
  ([trans add-op-namespace-to-kw]
   (if (seq (.getParameterValues trans))
     [(keyword (when add-op-namespace-to-kw "op") (.getName trans)) 
      (zipmap (.getParameterNames trans) (.getParameterValues trans))]
     (keyword (when add-op-namespace-to-kw "op") (.getName trans)))))

(defn get-transitions 
  ([state] (get-transitions false))
  ([state add-op-namespace-to-kw] (.explore state)
   (let [transs (.getTransitions state)]
     (map #(translate-transition % add-op-namespace-to-kw) transs))))

(defn wrap-state [state] 
  (proxy [clojure.lang.APersistentMap clojure.lang.IMeta] []
    (valAt [k & not-found]
      (cond (vector? k)
        ;; vector: assume operation
        (apply successor state (update k 0 (comp keyword name)))
        (and (keyword? k) (= "op" (namespace k)))
        (successor state (keyword (name k)))
        :otherwise
        ;; not vector: assume variable 
        (let [m (merge (into {} (.getConstantValues state FormulaExpand/TRUNCATE)) (into {} (.getVariableValues state FormulaExpand/TRUNCATE)))
              formalism (type (first (keys m)))
              key-var (first (filter #(= (.getCode %) (name k)) (keys m)))
              ;; TODO use this line instead once the AbstractEvalElement is fixed
              ;key-var (clojure.lang.Reflector/invokeConstructor formalism (into-array [(name k)]))
              ]
          (if key-var
            ((handle-val-output :value (.getStateSpace state) default-eval-settings #_de.hhu.stups.prob.translator.Translator/translate) (.getValue (get m key-var)))
            not-found))))
    (containsKey [k]
      (let [constant-ids (map #(.getCode %) (keys (.getConstantValues state FormulaExpand/EXPAND)))
            variable-ids (map #(.getCode %) (keys (.getVariableValues state FormulaExpand/EXPAND)))]
        (contains? (set (map keyword (concat constant-ids variable-ids))) k)))
    (entryAt [k]
      (clojure.lang.MapEntry. k (.valAt this k)))
    (assoc [k v]
      (let [constants (.getConstantValues state FormulaExpand/EXPAND)
            variables (.getVariableValues state FormulaExpand/EXPAND)
            bindings (map (fn [[k v]] [(keyword (.getCode k)) (b-expression->ir (.getValue v))]) (merge (into {} constants) (into {} variables)))
            bindingsmap (assoc (into {} bindings) k v)
            statespace (.getStateSpace state) ]
        (to-state statespace bindingsmap)))
    (cons [v] (conj this v))
    (count [] (count (concat (.getConstantValues state FormulaExpand/EXPAND) (.getVariableValues state FormulaExpand/EXPAND))))
    (without [k] (throw (Exception.)))
    (iterator []
      (clojure.lang.RT/iter (seq this)))
    (seq [] ;; TODO: do I really need seq / Seqable?
      (let [constants (.getConstantValues state FormulaExpand/EXPAND)
            variables (.getVariableValues state FormulaExpand/EXPAND)
            bindings (map (fn [[k v]] (clojure.lang.MapEntry. (keyword (.getCode k)) (if-not (instance? de.prob.animator.domainobjects.IdentifierNotInitialised v)
                                                                                       (b-expression->ir (.getValue v))
                                                                                       :prob/uninitialised)))
                          (merge (into {} constants) (into {} variables)))
            transs (map (fn [op] (clojure.lang.MapEntry. op '...)) (get-transitions state true))]
        (concat bindings transs)))
    (meta [] {:state state})))


(defn root-state [state-space]
  (wrap-state (.getRoot state-space)))




(comment
  (do
  (use 'clojure.reflect)
  (use 'clojure.pprint)
  (def rr (comp print-table :members reflect))
  (use 'lisb.examples.sebastian)
  (require '[lisb.translation.util :refer [ir->ast b->ast]])
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

  (def ss2 (state-space! (b->ast (slurp (clojure.java.io/resource "machines/b/CAN_BUS_tlc.mch")))))

  (root-state ss2)
  (pprint (-> ss2 root-state :op/$setup_constants :op/$initialise_machine ))
  ;; (-> ss2 root-state :op/$setup_constants :op/$initialise_machine (.findTransition "Update" []) (.getDestination))
  (class (-> ss2 root-state :op/$setup_constants :op/$initialise_machine ))
  (-> ss2 root-state :op/$setup_constants :op/$initialise_machine :op/Update :op/T3Initiate :op/T3writebus)
  (get-in (root-state ss2) [:op/$setup_constants :op/$initialise_machine [:Update {:pmax 0}] :op/T3Initiate])
  (to-state ss2 (get-in (root-state ss2) [:op/$setup_constants :op/$initialise_machine :op/Update] :op/T3Initiate))
  (successor (.getRoot ss2) :$setup_constants)
  (def st (-> (.getRoot ss2)
      (successor :$setup_constants)
      (successor :$initialise_machine)
      ;(successor :Update "0")
      ;get-transitions
  ;    reify-mappy
  ;    :T1_state
      ))
  (do st)
  (-> st wrap-state (get [:Update {:pmax 0}]))
  (get (reify-mappy st) :NATSET)

  (get-transitions st)
  (.perform st "Update" (into-array String ["pmax=0"]))
  (successor st "Update" {:pmax 0})

  ;; TODO: how much translation of values is desirable?
  ;; TODO: what is a good way to make states (accessing values) and transtions (getting successors) feel clojure-y?

  (def ss3 (state-space! (b->ast (slurp "/home/philipp/repos/prob_prolog/distribution_examples/Simple/Lift.mch"))))
  (map #(.getCode %) (keys (.getVariableValues (root-state ss3) FormulaExpand/EXPAND)))
  (.getValue (first (vals (.getVariableValues (-> (root-state ss3) :op/$initialise_machine) FormulaExpand/EXPAND))))
  (.entryAt (-> (root-state ss3) :op/$initialise_machine) :curfloor)
  (seq (update (assoc (-> (root-state ss3) :op/$initialise_machine) :curfloor 99) :curfloor dec))
  (use 'clojure.repl)
  (source update)

  (seq (to-state ss3 {:curfloor 42}))

  (source merge)
  )
