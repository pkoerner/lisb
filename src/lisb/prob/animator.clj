(ns lisb.prob.animator
  (:require [lisb.prob.retranslate :refer [retranslate]])
  (:import com.google.inject.Guice
           com.google.inject.Stage
           de.prob.MainModule
           de.prob.scripting.Api
           de.prob.animator.command.EvaluateFormulasCommand
           de.prob.animator.domainobjects.ClassicalB
           de.prob.animator.domainobjects.FormulaExpand
           de.prob.animator.domainobjects.EvalResult
           de.prob.animator.domainobjects.ComputationNotCompletedResult
           de.prob.animator.domainobjects.EnumerationWarning))


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



(defn bfile->b
  "Use this instead of slurping a machine file.
  Will load an .mch file with ProB and obtain an internal representation.
  Mainly used to circumvent definitions."
  [filename]
  (let [animator (state-space! filename)
        cmd (de.prob.animator.command.GetInternalRepresentationPrettyPrintCommand.)]
    (.execute animator cmd)
    (.getPrettyPrint cmd)))




(use 'lisb.translation.util)
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


(use 'lisb.translation.util)
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


