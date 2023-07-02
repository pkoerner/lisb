(ns lisb.translation.eventb.b2eventb
  (:require [lisb.translation.eventb.util :refer [eventb]]
            [lisb.translation.util :refer [b]]
            [lisb.translation.lisb2ir :refer [bnot]]
            [lisb.translation.eventb.dsl :refer [eventb-event] :as dsl]))

(defn- is-action? [ir]
  (#{:assignment :becomes-element-of :becomes-such} (:tag ir)))

(defn- tag= [tag] (fn [ir] (= (:tag ir) tag)))

(defn- with-guards [event & guards]
  (let [old (map :values (filter (tag= :guards) (:event-clauses event)))
        updated (apply dsl/event-when (apply concat guards old))
        other (remove (tag= :guards) (:event-clauses event))]
    (assoc event :event-clauses (conj other updated))))

(defn- with-actions [event & actions]
  (let [old (map :values (filter (tag= :actions) (:event-clauses event)))
        updated (apply dsl/event-then (apply concat actions old))
        other (remove (tag= :actions) (:event-clauses event))]
    (assoc event :event-clauses (conj other updated))) )

(defn- append-name [event & postfixes]
  (update event :name (fn [n] (keyword (apply str (name n) postfixes)))))

;; Substitutions

(declare sub->events)

(defn recur-until-action 
  "resolve all possible substitutions and adds the guard"
  [event sub] 
  (if (is-action? sub)
    [(with-actions event sub)] 
    (sub->events event sub)))

(defmulti sub->events
  "Expects the substitutions and a base-event and returns a translated as a list of events based on the base-event"
  (fn [base-event ir & args] (:tag ir)))

(defmethod sub->events :if-sub [base-event ir]
  (concat 
   (recur-until-action 
    (-> base-event (append-name "-then") (with-guards (:cond ir))) 
    (:then ir))
   (recur-until-action 
    (-> base-event (append-name "-else") (with-guards (bnot (:cond ir)))) 
    (:else ir))))

(defmethod sub->events :select [base-event {:keys [clauses]}]
  (concat (apply concat
                 (map-indexed 
                  (fn [i [guard sub]]
                    (recur-until-action
                     (-> base-event
                         (append-name  "-select" i)
                         (with-guards guard))
                     sub))
                  (partition 2 clauses)))
          (when (odd? (count clauses))
            (recur-until-action 
             (apply with-guards 
                    (append-name base-event "-selectelse")
                    (map bnot (take-nth 2 (butlast clauses))))
             (last clauses))))
  )

(comment
  (clojure.pprint/pp)
  
  (sub->events (eventb-event :foo)
               (eventb (if-sub :test (assign :then 1)
                               (assign :else 1))))
  (sub->events (eventb-event :foo)
               (eventb (select :cond1 (assign :a 1)
                               :cond2 (assign :a 1)
                               (assign :else 42))))
  (sub->events (eventb-event :foo)
               (eventb (select :cond1 (assign :a 1)
                               :cond2 (assign :a 1)
                               :cond3 (assign :a 1)
                               :cond4 (if-sub :cond
                                              (assign :then 1)
                                              (if-sub :cond-2
                                                      (assign :else 1)
                                                      (assign :else 2))))))
  )

