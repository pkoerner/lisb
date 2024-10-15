(ns lisb.translation.eventb.dsl
  (:require [lisb.translation.lisb2ir :refer [b]]
            [clojure.spec.alpha :as s]))


(comment
  ;;TODO: flatten event ir, by adding a top level key for all event-clauses.
  "{:tag :event
   :args [:x :y]
   :witnesses [...]
   :status :convergent
   :reference {:tag :event-reference ...}
   :guards [...]
   :actions [...]}")


(defn eventb-context [name & clauses]
  {:tag :context
   :name name
   :machine-clauses clauses})
(s/fdef eventb-context
  :args (s/cat :machine-clauses ::machine-clauses)
  :ret (s/and (s/keys :req-un [::tag] :req [::machine-clauses])
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

(defn event-when [& guards]
  {:tag :guards
   :values guards})

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

(defn eventb-event [name & clauses]
  {:tag :event
   :name name
   :clauses clauses})

;; Other

(defn eventb-partition [set & partitions]
  {:tag :partition
   :set set
   :partitions partitions})

(defn eventb-finite [set]
  {:tag :finite
   :set set})

(defn eventb-extended-expr [identifier exprs preds]
  {:tag :extended-expr
   :identifier identifier
   :exprs exprs
   :preds preds})

(defn eventb-extended-pred [identifier exprs preds]
  {:tag :extended-pred
   :identifier identifier
   :exprs exprs
   :preds preds})

(defn eventb-theorem [pred]
  {:tag :theorem
   :pred pred})

(defmacro eventb [lisb]
  `(b (let [~'axioms ~'properties
            ~'theorems ~'assertions
            ~'theorem eventb-theorem
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
            ~'finite eventb-finite
            ~'extended-expr eventb-extended-expr
            ~'extended-pred eventb-extended-pred] 
        ~lisb)))

