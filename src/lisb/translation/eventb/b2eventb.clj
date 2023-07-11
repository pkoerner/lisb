(ns lisb.translation.eventb.b2eventb
  (:require [com.rpl.specter :as s]
            [lisb.translation.eventb.util :refer [eventb]]
            [lisb.translation.util :refer [b] :as butil]
            [lisb.translation.lisb2ir :refer [bnot]]
            [lisb.translation.eventb.dsl :refer [eventb-event] :as dsl]
            [lisb.translation.eventb.specter-util :refer [CLAUSE NAME TAG INCLUDES]])
  (:import [de.prob.model.eventb Event]))


(defn- with-guards
  "Adds *guards* to *event*"
  [event & guards]
  (let [old (s/select [:event-clauses s/ALL (TAG :guards) :values s/ALL] event)
        updated (apply dsl/event-when (concat guards old))
        other (s/setval [s/ALL (TAG :guards)] s/NONE (:event-clauses event))]
    (assoc event :event-clauses (conj other updated))))

(defn- with-actions
  "Adds *actions* to *event*"
  [event & actions]
  (let [old (s/select [:event-clauses s/ALL (TAG :actions) :values s/ALL] event)
        updated (apply dsl/event-then (concat actions old))
        other (s/setval [s/ALL (TAG :actions)] s/NONE (:event-clauses event))]
    (assoc event :event-clauses (conj other updated))))

(defn- append-name
  "Append *event* name with all *postfixes*"
  [event & postfixes]
  (update event :name (fn [n] (keyword (apply str (name n) postfixes)))))

;; Substitutions

(declare sub->events)

(defmulti sub->events
  "Expects the substitutions and a base-event and returns a translated as a list of events based on the base-event"
  (fn [base-event ir & args] (:tag ir)))

(defmethod sub->events :assignment [base-event ir]
  [(with-actions base-event ir)])

(defmethod sub->events :becomes-element-of [base-event ir]
  [(with-actions base-event ir)])

(defmethod sub->events :becomes-such [base-event ir]
  [(with-actions base-event ir)])

(defmethod sub->events :parallel-sub [base-event ir]
  (reduce (fn [events sub] (mapcat #(sub->events % sub) events))
          [base-event]  (:subs ir)))

(defmethod sub->events :precondition [base-event ir]
    (-> (apply with-guards base-event
             (if (= (-> ir :pred :tag) :and)
                     (-> ir :pred :preds)
                     [(:pred ir)]))
      (sub->events (first (:subs ir))))) ;; there should be only one sub

(defmethod sub->events :op-call->extends [base-event {:keys [event-names arg-names arg-vals]}]
  (assert (not (s/selected-any? [:event-clauses s/ALL (TAG :event-reference)] base-event))
          "An event can only refine one event")
  (map-indexed (fn [i event-name]
         (-> (apply with-guards base-event (map butil/b= arg-names arg-vals))
             (append-name "-" i)
            (update :event-clauses conj (dsl/event-extends event-name))))
       event-names))

(defmethod sub->events :if-sub [base-event ir]
  (concat
   (-> base-event
       (append-name "-then")
       (with-guards (:cond ir))
       (sub->events (:then ir)))
    (when (:else ir)
      (-> base-event (append-name "-else")
        (with-guards (bnot (:cond ir)))
        (sub->events (:else ir))))))

(defmethod sub->events :cond [base-event {:keys [clauses]}]
  (let [guards  (reduce (fn [acc cur]
                          (conj acc (conj
                                     (update (last acc) (- (count acc) 1) butil/bnot)
                                     cur)))
                        [[(first clauses)]] (rest (take-nth 2 clauses)))
        pairs (map vector guards (take-nth 2 (rest clauses)))]
    (concat (apply concat
                 (map-indexed
                  (fn [i [guards sub]]
                      (-> (apply with-guards base-event guards)
                         (append-name "-cond" i)
                         (sub->events sub)))
                  pairs))
          (when (odd? (count clauses))
            (sub->events
             (apply with-guards
                    (append-name base-event "-condelse")
                    (map bnot (take-nth 2 (butlast clauses))))
             (last clauses))))))

(defmethod sub->events :select [base-event {:keys [clauses]}]
  (if (= (count clauses) 2)
    (-> base-event
        (with-guards (first clauses))
        (sub->events (second clauses)))
    (concat (apply concat
                   (map-indexed
                    (fn [i [guard sub]]
                      (-> base-event
                          (append-name  "-select" i)
                          (with-guards guard)
                          (sub->events sub)))
                    (partition 2 clauses)))
            (when (odd? (count clauses))
              (sub->events
               (apply with-guards
                      (append-name base-event "-selectelse")
                      (map bnot (take-nth 2 (butlast clauses))))
               (last clauses)))))
  )


(defn literal? [x] (or (keyword? x) (number? x)))

(defn literal-name [literal]
  (cond
    (keyword? literal) (name literal)
    :default (str literal)))

(defmethod sub->events :case [base-event {:keys [expr cases]}]
  (mapcat
   (fn [[case-literal sub]]
     (assert (literal? case-literal))
     (-> base-event
         (append-name "-" (literal-name case-literal))
         (with-guards (butil/b= case-literal expr))
         (sub->events sub)))
   (partition 2 cases))
  )

(defn op->events [ir]
  (sub->events
   (if (seq (:args ir))
     (eventb-event (:name ir) (apply dsl/event-any (:args ir)))
     (eventb-event (:name ir)))
    (:body ir)))

(def MAP-NODES
   (s/recursive-path [] p
     (s/if-path map?
       (s/continue-then-stay s/MAP-VALS p))))

(defn replace-args-in-body [op values]
  "Replaces all ouccurences of arguments with the values"
  (let [replacement (into {} (map vector (:args op) values))]
    (:body (s/transform [:body (s/walker replacement)] replacement op)))) ;;FIXME: keys and tag value can also be replaced

(defn extend-op-call
  "Generates an extend reference for an op-call"
  [base-machine included-machine])

(defn merge-clause
  "Merges the *clause* of *m2* into *m1*"
  [m1 m2 clause]
  (let [this (s/select [(CLAUSE clause) :values s/ALL] m1)
        other (s/select [(CLAUSE clause) :values s/ALL] m2)
        values (list* (set (concat this other)))
        m1 (s/setval (CLAUSE clause) s/NONE m1)]
    (if (seq values)
      (update m1 :machine-clauses conj {:tag clause :values values})
      m1)))

(defn includes->inline
  "Inline all op-calls to included-machine in the base-machine only.
  Remove included-machine from the machine references"
  [base-machine included-machine]
  (let [operations (into {} (map (fn [op] [(:name op) op]))
                         (s/select [(CLAUSE :operations) :values s/ALL] included-machine))]
    (if
     (s/selected-any? [(INCLUDES (:name included-machine))] base-machine)
      (->> (-> base-machine
               (merge-clause included-machine :variables)
               (merge-clause included-machine :sets)
               (merge-clause included-machine :invariants)
               (merge-clause included-machine :constants)
               (merge-clause included-machine :properties)
               (merge-clause included-machine :init)
               )
           (s/setval [(INCLUDES (:name included-machine))] s/NONE)
           (s/transform (s/walker (and (TAG :op-call) #(operations (:op %))))
                        (fn [call]
                          (replace-args-in-body
                           (operations (:op call))
                           (:args call))
                          )))
      base-machine)))

(defn OP [n] (s/path [(CLAUSE :operations) :values s/ALL (NAME n)]))

(defn op-call->extends [included-machine {:keys [op args] :as call}]
  (let [operation (s/select-any (OP op) included-machine)]
    (if (= operation s/NONE)
      call
      {:tag :op-call->extends ;;this a special tag which is resolved when running op->events
       :arg-vals args
       :arg-names (:args operation)
       :event-names (map :name (op->events operation))})))


(defn includes->refinement
  "If the machine only includes one other convert the inclusion into a refinement.
  All operation calls are converted into extends"
  [base-machine included-machine]
  (assert (= 1 (count (s/select INCLUDES base-machine))))
    (if (s/selected-any? [(INCLUDES (:name included-machine))] base-machine)
      (->> (-> base-machine
               (assoc :tag :refinement :abstract-machine-name (:name included-machine))
               (merge-clause included-machine :variables))
           ;;TODO: copy variables
           (s/setval [(INCLUDES (:name included-machine))] s/NONE)
           (s/transform (s/walker (TAG :op-call)) (partial op-call->extends included-machine)))
      base-machine))

(defn enumerated-set->partition [ir]
  (apply dsl/eventb-partition (:id ir) (map (fn [x] #{x}) (:elems ir))))

(defn update-enumerated-set->partitions [ir]
  "Changes all enumerated set definition into deffered sets, and adds the definition in the properties as partition"
  (let [partitions (map enumerated-set->partition
                        (s/select [(CLAUSE :sets) :values s/ALL (TAG :enumerated-set)] ir))
        constants (s/select [(CLAUSE :sets) :values s/ALL (TAG :enumerated-set) :elems s/ALL] ir)]
    (->> ir
         (s/transform [(CLAUSE :sets) :values s/ALL (TAG :enumerated-set)]
                      (fn [s] (butil/bdeferred-set (:id s))))
         (s/transform [(CLAUSE :properties) :values ] #(concat partitions %))
         (s/transform [(CLAUSE :constants) :values ] #(distinct (concat constants %))))))

(defn context-name [machine-name] (keyword (str (name machine-name) "-ctx")))

(defn extract-context [ir]
  "Extracts an Event-B context from an classical B machine"
    (->> ir
       update-enumerated-set->partitions
       (s/select [(s/multi-path (CLAUSE :sets)
                                (CLAUSE :constants)
                                (CLAUSE :properties))])
         (apply dsl/eventb-context (context-name (:name ir)))))

(defn extract-machine [ir]
  "Extracts an Event-B machine from an classical B machine. Operations are converted to Events"
  (let [events (apply dsl/eventb-events
                      (mapcat op->events (s/select [(CLAUSE :operations) :values s/ALL] ir)))
        sees (butil/bsees (context-name (:name ir)))]
    (->> (update ir :machine-clauses concat [events sees])
         (s/setval [(s/multi-path (CLAUSE :sets)
                                  (CLAUSE :constants)
                                  (CLAUSE :properties)
                                  (CLAUSE :operations))] s/NONE))))

(comment
  (require '[lisb.examples.sebastian :as seb])

  (def m0 (b (machine :m0
                      (sees :c0)
                      (variables :b)
                      (operations
                        (:foo [:a] (if-sub :cond (assign :b :c) (assign :b :a)))
                        (:SET_EngineOn
                          []
                          (select
                            (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
                            (assign
                              :engineOn true)))
                        ))))

  (def m1 (b (machine :m1
                      (sets :A :SET #{:a :d :b :c})
                      (constants :d)
                      (properties (= :a 3))
                      (includes :m0)
                      (operations
                       (:bar [] (|| (op-call :foo 3)
                                    (assign :c 123)))
                       (:ENV_Turn_EngineOn []
                        (parallel-sub
                         (op-call :SET_EngineOn)
                         (if-sub
                          (and
                           (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
                           (= :hazardWarningSwitchOn :switch_off))
                          (assign :SET_BlinkersOn :extends)
                          )))))))

  (clojure.pprint/pprint (extract-machine (includes->inline m1 m0)))
  (clojure.pprint/pprint (extract-context (includes->inline m1 m0)))

  (extract-machine (includes->refinement m1 m0))
  (extract-context (includes->refinement m1 m0))

  (clojure.pprint/pp)

  (sub->events (eventb-event :foo)
               (b (|| (assign :a 1)
                      (assign :b 2)
                      (if-sub :cond1
                              (assign :c 1)
                              (assign :d 1))
                      (if-sub :cond2
                              (assign :e 1)
                              (assign :f 1)))))

  (sub->events (eventb-event :foo)
               (b (case :x
                    1 (assign :y 2)
                    2 (assign :y 3)
                    :eof (assign :y 4)
                    4 (assign :y 5)
                    )))

  (sub->events (eventb-event :foo)
               (eventb (if-sub :test
                               (assign :then 1)
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

