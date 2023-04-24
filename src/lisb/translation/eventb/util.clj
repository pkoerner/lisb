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

(defn prob-model [& machines]
  (reduce (fn [model machine] (.addMachine model machine )) (.get modelCreator) machines))

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
    (and (seq? lisb) (= 'events (first lisb))) (list* 'operations (rest events-clause))
    (seqable? lisb) (walk pre-process-lisb identity lisb)
    :else lisb))


(defmacro eventb [lisb]
  `(b ~(pre-process-lisb lisb)))

(comment
  (def machine (eventb (machine :hello-world
                                (constants :z)
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
                                 (:inc [] (pre (< :x 10) (assign :x (+ :x 1))))
                                 (:hello [] (assign :hello true))))))


  (ir->prob-machine machine)

  (get-type (get-statespace machine) (b (cartesian-product :s :t)))

  (-> machine
      ir->prob-machine
      prob-model
      (prob-model->rodin "hello" "./resources/eventb"))

  )

