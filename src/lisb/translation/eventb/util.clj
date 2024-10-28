(ns lisb.translation.eventb.util
  (:require [potemkin :refer [import-vars]]
            [lisb.prob.animator :refer [api injector]]
            [lisb.translation.eventb.b2eventb :as b2eventb]
            [lisb.translation.eventb dsl eventb2lisb]
            [lisb.translation.eventb.ir2eventb :refer [ir->prob-with-label]]
            )
  (:import
   de.prob.model.eventb.translate.ModelToXML
   (de.prob.model.eventb
    EventBModel)
   (de.prob.model.representation 
    DependencyGraph$ERefType)
   (de.prob.animator.domainobjects
    FormulaExpand
    EventB)
   ))

(import-vars [lisb.translation.eventb.dsl eventb])
(import-vars [lisb.translation.eventb.ir2eventb ir->prob ir-pred->str ir-expr->str])
(import-vars [lisb.translation.eventb.eventb2lisb prob->lisb])

(def modelCreator (delay (.getProvider @injector EventBModel)))

(defn lisb->ir [lisb]
  (eval `(eventb ~lisb)))

(defn model-with-context [model context]
  (reduce (fn [model extended]
            (.addRelationship model (.getName context) (.getName extended) DependencyGraph$ERefType/EXTENDS))
          (.addContext model context)
          (.getExtends context)))

(defn model-with-machine [model machine]
  (let [refined (.getRefinesMachine machine)
        model (reduce (fn [model context]
                        (.addRelationship model (.getName machine) (.getName context) DependencyGraph$ERefType/SEES))
                      (.addMachine model machine)
                      (.getSees machine))]
    (if refined
      (.addRelationship model (.getName machine) (.getName refined) DependencyGraph$ERefType/REFINES)
      model)))

(defn prob-model [& machines-or-contexts]
  (reduce (fn [model value]
            (condp = (type value)
              de.prob.model.eventb.Context (model-with-context model value)
              de.prob.model.eventb.EventBMachine (model-with-machine model value)))
          (.get @modelCreator) machines-or-contexts))

(defn ir->prob-model [& ir] (->> ir
                                 (map  b2eventb/transform-all-expressions)
                                 (map-indexed (fn [n ir] (ir->prob-with-label ir (str n "-"))))
                                 (apply prob-model)))


(defn prob-model->rodin [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn rodin->prob-model [filename]
  (-> (.eventb_load @api filename)
      .getModel))


(defn rodin->lisb [filename] (-> filename rodin->prob-model prob->lisb))

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
  (def xx (rodin->lisb "/home/philipp/tmp/rodin/workspace/NewProject/ClockDeepInstance.buc"))
  (def xx (rodin->lisb "/home/philipp/tmp/rodin/workspace/NewProject/Clock.bum"))
  xx
  (def ir (lisb->ir xx))

  (prob-model->rodin (apply ir->prob-model ir) "MyModel" "/home/philipp/tmp/rodin/workspace/")
  )
