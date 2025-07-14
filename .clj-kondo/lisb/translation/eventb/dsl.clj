(ns lisb.translation.eventb.dsl
  (:require [clojure.walk :as walk]))

(defmacro eventb [lisb]
  (list 'lisb.translation.lisb2ir/b
        (walk/postwalk
         (fn [form]
           (cond
             (and (list? form)
                  (symbol? (first form)))
             (cons (get {'axioms 'properties
                         'theorems 'assertions
                         'context 'lisb.translation.eventb.dsl/eventb-context
                         'machine 'lisb.translation.eventb.dsl/eventb-machine
                         'variant 'lisb.translation.eventb.dsl/eventb-variant
                         'partition 'lisb.translation.eventb.dsl/eventb-partition
                         'events 'lisb.translation.eventb.dsl/eventb-events
                         'event 'lisb.translation.eventb.dsl/eventb-event
                         'when 'lisb.translation.eventb.dsl/event-when
                         'any 'lisb.translation.eventb.dsl/event-any
                         'then 'lisb.translation.eventb.dsl/event-then
                         'event-refines 'lisb.translation.eventb.dsl/event-refines
                         'event-extends 'lisb.translation.eventb.dsl/event-extends
                         'status 'lisb.translation.eventb.dsl/event-status
                         'with 'lisb.translation.eventb.dsl/event-with
                         'finite 'lisb.translation.eventb.dsl/eventb-finite
                         'prj1 'lisb.translation.lisb2ir/beventb-prj1
                         'prj2 'lisb.translation.lisb2ir/beventb-prj2}
                        (first form)
                        (first form))
                   (rest form))
             :else form))
         lisb)))