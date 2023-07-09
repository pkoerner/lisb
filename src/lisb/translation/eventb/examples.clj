(ns lisb.translation.eventb.examples
  (:require
   [lisb.translation.eventb.util :refer [prob-model prob-model->rodin]]
   [lisb.examples.simple :refer :all]))

(comment
  (as-> [lift a-counter] x
  (apply prob-model x)
  (prob-model->rodin x "simple" "./resources/eventb")))
