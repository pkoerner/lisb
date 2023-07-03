(ns lisb.translation.eventb.b2eventb
  (:require [lisb.translation.eventb.util :refer [eventb]]
            [lisb.translation.util :refer [b] :as butil]
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

(defmethod sub->events :parallel-sub [base-event ir]
    (let [event (apply with-actions base-event (filter is-action? (:subs ir)))]
      (reduce (fn [events sub] (mapcat #(sub->events % sub) events))
              [event] (remove is-action? (:subs ir)))))

(defmethod sub->events :precondition [base-event ir]
    (-> (apply with-guards base-event
             (if (= (-> ir :pred :tag) :and)
                     (-> ir :pred :preds)
                     [(:pred ir)]))
      (recur-until-action (first (:subs ir))))) ;; there should be only one sub

(defmethod sub->events :if-sub [base-event ir]
  (concat
    (-> base-event
        (append-name "-then")
        (with-guards (:cond ir))
        (recur-until-action (:then ir)))
    (when (:else ir)
      (-> base-event (append-name "-else")
        (with-guards (bnot (:cond ir)))
        (recur-until-action (:else ir)))))

(defmethod sub->events :select [base-event {:keys [clauses]}]
  (concat (apply concat
                 (map-indexed 
                  (fn [i [guard sub]]
                     (-> base-event
                         (append-name  "-select" i)
                         (with-guards guard)
                         (recur-until-action sub)))
                  (partition 2 clauses)))
          (when (odd? (count clauses))
            (recur-until-action
             (apply with-guards
                    (append-name base-event "-selectelse")
                    (map bnot (take-nth 2 (butlast clauses))))
             (last clauses))))
  )

(defn literal-name [literal]
  (cond
    (keyword? literal) (name literal)
    :default (str literal)))

(defmethod sub->events :case [base-event {:keys [expr cases]}]
  (mapcat
   (fn [[case-literal sub]]
     (-> base-event
         (append-name "-" (literal-name case-literal))
         (with-guards (butil/b= case-literal expr))
         (recur-until-action sub)))
   (partition 2 cases))
  )

(defmethod sub->events nil [base-event ir]
  (throw (ex-info "Not Implemented for" {:ir ir})))

(defn op->events [ir]
  (sub->events
    (eventb-event (:name ir) (apply dsl/event-any (:args ir)))
    (:body ir)))

(comment
  (clojure.pprint/pp)

  (sub->events (eventb-event :foo)
               (b (case :x
                    1 (assign :y 2)
                    2 (assign :y 3)
                    :eof (assign :y 4)
                    4 (assign :y 5)
                    )))

  (sub->events (eventb-event :foo)
               (b (|| (assign :a 1)
                      (assign :b 2)
                      (if-sub :p
                              (assign :c 3)
                              (assign :d 4))
                      (if-sub :q
                              (assign :e 3)
                              (assign :f 4)))))

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

