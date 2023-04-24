(ns lisb.translation.eventb.util
  (:require [lisb.translation.eventb.ir2eventb :refer [ir-expr->str ir->prob-machine ir->prob-context]]
            [lisb.prob.animator :refer [injector]]
            [lisb.translation.util :refer [b]]
            [lisb.translation.lisb2ir :refer [boperations]]
            [clojure.walk :refer [walk]])
  (:import
   de.prob.model.eventb.translate.ModelToXML
   de.prob.model.eventb.EventBModel
   (de.prob.animator.domainobjects
    FormulaExpand
    EventB)
   ))

(def modelCreator (.getProvider injector EventBModel))

(defn prob-model [& machines-or-contexts]
  (reduce (fn [model value]
            (condp = (type value)
              de.prob.model.eventb.Context (.addContext model value)
              de.prob.model.eventb.EventBMachine (.addMachine model value)))
          (.get modelCreator) machines-or-contexts))

(defn prob-model->rodin [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn get-type
   [ss ir-formula]
    (let [formula (ir-expr->str ir-formula)
        ee (EventB. formula FormulaExpand/EXPAND)]
      (.getType (.typeCheck ss ee))))

(def get-statespace
  (memoize (fn [ir]
             (let [machine (ir->prob-machine ir)
                   model (prob-model machine)]
               (.load model machine {})))))

(defn ir->prob-model [ir] (-> ir ir->prob-machine prob-model))

(defn pre-process-lisb [lisb]
  (cond
    (and (seq? lisb) (= 'events (first lisb))) (list* 'operations (rest lisb))
    (seqable? lisb) (walk pre-process-lisb identity lisb)
    :else lisb))

(defn eventb-where [preds body]
  {:tag :select
   :clauses [preds body]})

;; let inside b macro to override existing symbols
(defmacro eventb [lisb]
  `(b (let [~'where eventb-where
            ~'axioms ~'properties]
        ~(pre-process-lisb lisb))))

(comment
  (def ir (eventb (machine :hello-world
                                (constants :z)
                                (sets :tracks)
                                (axioms (in :z nat-set)
                                            (< :z 100))
                                (variables :x :y :hello :s :t)
                                (invariants
                                 (subset? :s (cartesian-product nat-set bool-set))
                                 (subset? :t nat-set)
                                 (in :hello bool-set)
                                 (<= :x 10)
                                 (in :y nat-set))
                                (init
                                 (assign :s #{})
                                 (assign :t #{1, 2})
                                 (assign :x 0 :y 50)
                                 (assign :hello true))
                                (events
                                 (:inc [] (where (< :x 10) (assign :x (+ :x 1))))
                                 (:say-hallo [] (assign :hello true))))))

  (clojure.pprint/pprint ir)

  (get-type (get-statespace ir) (b (cartesian-product :s :t)))

  (def model (prob-model (ir->prob-machine ir) (ir->prob-context ir)))

  (prob-model->rodin model "hello" "./resources/eventb")
  )

