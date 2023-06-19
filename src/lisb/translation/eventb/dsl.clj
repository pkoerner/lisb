(ns lisb.translation.eventb.dsl
  (:require [clojure.walk :refer [walk]]
            [lisb.translation.lisb2ir :refer [b]]))

(defn remove-nil [m] 
  (into {} (remove (fn [item] (nil? (val item)))) m))

(defn eventb-context [name & clauses]
  {:tag :context 
   :name name 
   :machine-clauses clauses})

(defn eventb-events [& events]
  {:tag :events
   :values events})

(defn eventb-event 
  ([name args guard actions]
   (remove-nil
    {:tag :event
     :name name
     :args (or args [])
     :guard (or guard true)
     :actions (or actions [])}))
  ([name refines args guard witness actions]
   (remove-nil 
    {:tag     :event
     :name    name
     :refines refines
     :args    (or args [])
     :guard   (or guard true)
     :witness witness
     :actions (or actions [])})))

(defn process-event-definitions [events-clause]
  (list* 'events 
        (map (fn [e] (list* 'event e)) (rest events-clause))))

(defn pre-process-lisb [lisb]
  (cond
    (and (seq? lisb) (= 'events (first lisb))) (process-event-definitions lisb)
    (seqable? lisb) (walk pre-process-lisb identity lisb)
    :else lisb))

(defmacro eventb [lisb]
  {:clj-kondo/ignore [:unresolved-symbol]}
  (let [pre-processed-lisb (pre-process-lisb lisb)]
    `(b (let [~'axioms ~'properties
              ~'theorems ~'assertions
              ~'context eventb-context
             ~'events eventb-events
             ~'event eventb-event] 
         ~pre-processed-lisb))))

(comment 
  (eventb 
   [(context :ctx 
      (constants :a)
      (axioms (in :a :nat))
      (theorems (= 1 2)))
    (machine :foo
             (events 
              (:name [] nil []) 
              (:name2 [] nil [])
              ))])
  )


