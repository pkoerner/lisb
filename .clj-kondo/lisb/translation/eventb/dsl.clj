(ns lisb.translation.eventb.dsl
  (:require [lisb.translation.lisb2ir :as l2ir]
            [clojure.walk :as walk]))

#_(defmacro eventb [lisb]
  `(l2ir/b (let [~'axioms ~'properties
                 ~'theorems ~'assertions
                 ~'context eventb-context
                 ~'machine eventb-machine
                 ~'variant eventb-variant
                 ~'partition eventb-partition
                 ~'events eventb-events
                 ~'event eventb-event
                 ~'when event-when
                 ~'any event-any
                 ~'then event-then
                 ~'event-refines event-refines
                 ~'event-extends event-extends
                 ~'status event-status
                 ~'with event-with
                 ~'finite eventb-finite]
             ~lisb)))

(defmacro eventb [lisb]
  (list 'lisb.translation.lisb2ir/b
        (walk/postwalk
         (fn [form]
           (cond
             (and (list? form)
                  (symbol? (first form)))
             (cons (get {'axioms 'lisb.translation.lisb2ir/properties
                         'theorems 'lisb.translation.lisb2ir/assertions
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
                         'finite 'lisb.translation.eventb.dsl/eventb-finite}
                        (first form)
                        (first form))
                   (rest form))
             :else form))
         lisb)))