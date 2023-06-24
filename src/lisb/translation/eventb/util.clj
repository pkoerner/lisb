(ns lisb.translation.eventb.util
  (:require [lisb.translation.eventb.ir2eventb :refer [ir-expr->str ir->prob]]
            [lisb.prob.animator :refer [injector]]
            [lisb.translation.eventb.dsl :refer [eventb]]
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

(defn ir->prob-model [& ir] (->> ir (map ir->prob) (apply prob-model)))

(defn prob-model->rodin [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn get-type
   [ss ir-formula]
    (let [formula (ir-expr->str ir-formula)
        ee (EventB. formula FormulaExpand/EXPAND)]
      (.getType (.typeCheck ss ee))))

(def get-statespace
  (memoize (fn [ir]
             (let [machine (ir->prob ir)
                   model (prob-model machine)]
               (.load model machine {})))))

(comment
  (def ir (eventb (machine :foo
                           (variables :s :t)
                           (invariants
                            (subset? :s (cartesian-product nat-set bool-set))
                            (subset? :t nat-set))
                           (init
                            (assign
                             :s #{}
                             :t #{1, 2})))))

  (get-type (get-statespace ir) (eventb (cartesian-product :s :t)))
  )

