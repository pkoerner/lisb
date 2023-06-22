(ns lisb.translation.eventb.dsl
  (:require [clojure.walk :refer [walk]]
            [lisb.translation.lisb2ir :refer [b band bparallel-sub]]
            [clojure.spec.alpha :as s]))

(defn eventb-context [name & clauses]
  {:tag :context
   :name name
   :machine-clauses clauses})
(s/fdef eventb-context
  :args (s/cat :machine-clauses ::machine-clauses)
  :ret (s/and (s/keys :req-un (::tag) :req (::machine-clauses))
              #(= :machine (:tag %))))             ; TODO: concretize spec

(defn eventb-machine [name & clauses]
  {:tag :machine
   :name name
   :machine-clauses clauses})

;; Machine Clauses

(defn eventb-variant [expr]
  {:tag :variant
   :expr expr})

(defn eventb-events [& events]
  {:tag :events
   :values events})

;; Event Clauses

(defn event-then [& actions]
  {:tag :actions
   :values actions})

(defn event-with [& clauses]
  {:tag :witnesses
   :values (map (fn [[name pred]]
                     {:tag  :witness
                      :name name
                      :pred pred})
                   (partition 2 clauses))})

(defn event-when [& gurads]
  {:tag :guards
   :values gurads})

(defn event-status [status]
  {:tag :status
   :value status})

(defn event-any [& args]
  {:tag :args
   :values args})

(defn event-refines [event-name]
  {:tag :event-reference
   :type :refines
   :value event-name})

(defn event-extends [event-name]
  {:tag :event-reference
   :type :extends
   :value event-name})

(defn eventb-event
  ([name & clauses]
  {:tag :event
   :name name
   :event-clauses clauses}))

;; Other

(defn eventb-partition [set & partitions]
  {:tag :partition
   :set set
   :partitions partitions})

(defmacro eventb [lisb]
  `(b (let [~'axioms ~'properties
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
            ~'with event-with]
        ~lisb)))

(defn lisb->ir [lisb]
  (eval `(eventb ~lisb)))

(comment
  (eventb (machine :machine-foo
                   (variables :x :y :z)
                   (variant (+ :x :y :z))
                   (events
                    (event :foo1
                           (any :t)
                           (then
                            (assign :x :t)
                            (becomes-such :y (> :y :t))
                            (becomes-element-of :z :nat)))
                    (event :foo2
                           (when (< 0 :x 10))
                           (then
                            (assign :x :y)
                            (becomes-such :y (> :y 10))
                            (becomes-element-of :z :nat)))
                    (event :foo3
                           (refines :foo1)
                           (when (< 0 :x 10))
                           (with :t (in :t :nat))
                           (then
                            (assign :x :y)
                            (becomes-such :y (> :y 10))
                            (becomes-element-of :z :nat)))))
    )
  )
