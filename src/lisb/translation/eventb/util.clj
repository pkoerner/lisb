(ns lisb.translation.eventb.util
  (:require [lisb.translation.eventb.ir2eventb :refer [ir->eventb-machine]]
            [lisb.prob.animator :refer [injector]]
            [lisb.translation.util :refer [b]])
  (:import
   de.prob.model.eventb.translate.ModelToXML
   de.prob.model.eventb.EventBModel
   ))

(def modelCreator (.getProvider injector EventBModel))

(defn prob-model [& machines]
  (reduce (fn [model machine] (.addMachine model machine )) (.get modelCreator) machines))

(defn prob-model->rodin [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(comment
  (def machine (b (machine :hello-world
                       (variables :x :y :hello)
                       (invariants
                                 (in :hello bool-set)
                                 (<= :x 10)
                                 (in :y nat-set))
                       (init
                           (assign :x 0 :y 50)
                           (assign :hello true))
                       (operations
                        (:inc [] (pre (< :x 10) (assign :x (+ :x 1))))
                        (:hello [] (assign :hello true))))))

  (-> machine
      ir->eventb-machine
      prob-model
      (prob-model->rodin "hello" "./resources/eventb"))

  )

