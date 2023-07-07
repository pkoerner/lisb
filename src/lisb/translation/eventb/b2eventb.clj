(ns lisb.translation.eventb.b2eventb
  (:require [com.rpl.specter :as s]
            [lisb.translation.eventb.util :refer [eventb]]
            [lisb.translation.util :refer [b] :as butil]
            [lisb.translation.lisb2ir :refer [bnot]]
            [lisb.translation.eventb.dsl :refer [eventb-event] :as dsl]
            [lisb.translation.eventb.specter-util :refer [CLAUSE NAME TAG INCLUDES]])
  (:import [de.prob.model.eventb Event]))


(defn- is-action? [ir]
  (#{:assignment :becomes-element-of :becomes-such} (:tag ir)))

(defn- with-guards [event & guards]
  (let [old (s/select [:event-clauses s/ALL (TAG :guards) :values s/ALL] event)
        updated (apply dsl/event-when (concat guards old))
        other (s/setval [s/ALL (TAG :guards)] s/NONE (:event-clauses event))]
    (assoc event :event-clauses (conj other updated))))

(defn- with-actions [event & actions]
  (let [old (s/select [:event-clauses s/ALL (TAG :actions) :values s/ALL] event)
        updated (apply dsl/event-then (concat actions old))
        other (s/setval [s/ALL (TAG :actions)] s/NONE (:event-clauses event))]
    (assoc event :event-clauses (conj other updated))))

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

(defmethod sub->events :op-call->extends [base-event {:keys [op-name arg-names arg-vals]}]
  (assert (not (s/selected-any? [:event-clauses s/ALL (TAG :event-reference)] base-event))
          "An event can only refine one event")
  [(-> base-event
       (with-guards (map butil/b= arg-names arg-vals))
       (update :event-clauses conj (dsl/event-extends op-name)))])

(defmethod sub->events :if-sub [base-event ir]
  (concat
    (-> base-event
        (append-name "-then")
        (with-guards (:cond ir))
        (recur-until-action (:then ir)))
    (when (:else ir)
      (-> base-event (append-name "-else")
        (with-guards (bnot (:cond ir)))
        (recur-until-action (:else ir))))))

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
                         (recur-until-action sub)))
                  pairs))
          (when (odd? (count clauses))
            (recur-until-action
             (apply with-guards
                    (append-name base-event "-condelse")
                    (map bnot (take-nth 2 (butlast clauses))))
             (last clauses))))))

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

(defmethod sub->events :case [base-event {:keys [expr cases]}] (mapcat
   (fn [[case-literal sub]]
     (-> base-event
         (append-name "-" (literal-name case-literal))
         (with-guards (butil/b= case-literal expr))
         (recur-until-action sub)))
   (partition 2 cases))
  )

#_(defmethod sub->events nil [base-event ir]
  (throw (ex-info "Not Implemented for" {:ir ir})))

(defn op->events [ir]
  (sub->events
   (if (empty (:args ir))
     (eventb-event (:name ir))
     (eventb-event (:name ir) (apply dsl/event-any (:args ir))))
    (:body ir)))

(def MAP-NODES
   (s/recursive-path [] p
     (s/if-path map?
       (s/continue-then-stay s/MAP-VALS p))))

(defn replace-args-in-body [op values]
  "Replaces all ouccurences of arguments with the values"
  (let [replacement (into {} (map vector (:args op) values))]
    (s/transform [:body (s/walker replacement)] replacement op))) ;;FIXME: keys and tag value can also be replaced

(defn extend-op-call
  "Generates an extend reference for an op-call"
  [base-machine included-machine])

(defn merge-clause
  ""
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
               )
           ;;TODO: copy sets, constants, variables, invariants, properties
           (s/setval [(INCLUDES (:name included-machine))] s/NONE)
           (s/transform (s/walker (TAG :op-call))
                        (fn [call]
                          (if (operations (:op call))
                            (:body (replace-args-in-body
                                    (operations (:op call))
                                    (:args call)))
                            call))))
      base-machine)))

(defn includes->refinement
  "If the machine only includes one other convert the inclusion into a refinement.
  All operation calls are converted into extends"
  [base-machine included-machine]
  (assert (= 1 (count (s/select INCLUDES base-machine))))
  (let [operations (into {} (map (fn [op] [(:name op) op]))
                         (s/select [(CLAUSE :operations) :values s/ALL] included-machine))]
    (if (s/selected-any? [(INCLUDES (:name included-machine))] base-machine)
      (->> (-> base-machine
               (assoc :tag :refinement :abstract-machine-name (:name included-machine))
               (merge-clause included-machine :variables))
           ;;TODO: copy variables
           (s/setval [(INCLUDES (:name included-machine))] s/NONE)
           (s/transform (s/walker (TAG :op-call))
                        (fn [{:keys [op args] :as call}]
                          (if (operations op)
                            {:tag :op-call->extends
                             :arg-vals args
                             :arg-names (:args (operations op))
                             :op-name op}
                            call))))
      base-machine)))

(defn extract-context [ir]
    (apply dsl/eventb-context (:name ir)
           (s/select [(s/multi-path (CLAUSE :sets)
                                    (CLAUSE :constants)
                                    (CLAUSE :properties))] ir)))

(comment
  (require '[lisb.examples.sebastian :as seb])

  (def m0 (b (machine :m0
                      (variables :b)
                      (operations
                        (:foo [:a] (assign :b :a))))))

  (def m1 (b (machine :m1
                      (sets :A :SET #{:a :d :b :c})
                      (constants :d)
                      (properties (= :a 3))
                      (includes :m0)
                      (operations
                       (:bar [] (|| (op-call :foo 3)
                                    (assign :c 123)))))))

  (s/select [(CLAUSE :sets) :values s/ALL :id] m1)

  (ModelElementList. (map (fn [c] (Context. (rodin-name c)))
                          (distinct (s/select (s/multi-path
                                                [(CLAUSE :constants) :values s/ALL]
                                                [(CLAUSE :sets) :values s/ALL (TAG :enumerated-set) :elems s/ALL]) m1))))


  (s/transform [(CLAUSE :operations) :values s/ALL] op->events (includes->inline m1 m0))
  (s/transform [(CLAUSE :operations) :values s/ALL] op->events (includes->refinement m1 m0))

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

