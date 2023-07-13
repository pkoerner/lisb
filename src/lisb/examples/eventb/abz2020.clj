(ns lisb.examples.eventb.abz2020
  (:require [lisb.translation.eventb.util :refer [eventb ir->prob-model prob-model->rodin]]
            [lisb.translation.util :refer [lisb->ir] :as butil]
            [lisb.translation.eventb.b2eventb :as b2eventb]
            [lisb.translation.eventb.ir2eventb :refer [rodin-name]]))


(defn- load-edn [lisb-name]
  (lisb->ir (read-string (slurp (clojure.java.io/resource (str "machines/lisb/abz2020/" lisb-name ".edn"))))))

(def Sensors (load-edn "Sensors"))

(def BlinkLamps (load-edn "BlinkLamps_v3"))

(def PitmanController (load-edn "PitmanController_v6"))

(comment
  ;; Resolve all include and extract as Rodin Project
  (as-> PitmanController x
    (b2eventb/includes->inline x Sensors)
    (b2eventb/includes->refinement x BlinkLamps)
    ((juxt b2eventb/extract-machine b2eventb/extract-context) x)
    (apply ir->prob-model (b2eventb/extract-machine BlinkLamps) (b2eventb/extract-context BlinkLamps) x)
    (prob-model->rodin x "abz2020" "./resources/eventb/" )))

