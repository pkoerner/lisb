(ns lisb.translation.eventb.b2eventb
  (:require [com.rpl.specter :as s]
            [lisb.translation.util :as butil]
            [lisb.translation.lisb2ir :refer [bnot]]
            [lisb.translation.eventb.dsl :refer [eventb eventb-event] :as dsl]
            [lisb.translation.irtools :refer [CLAUSES CLAUSE TAG IR-WALKER ALL-KEYWORDS IR-NODE]]
            [lisb.translation.eventb.specter-util :refer [NAME INCLUDES]]))


(defn- add-guards
  "Adds *guards* to *event*"
  [event & guards]
  (let [old (s/select [:clauses s/ALL (TAG :guards) :values s/ALL] event)
        updated (apply dsl/event-when (concat old guards))
        other (s/setval [s/ALL (TAG :guards)] s/NONE (:clauses event))]
    (assoc event :clauses (conj other updated))))

(defn- add-actions
  "Adds *actions* to *event*"
  [event & actions]
  (let [old (s/select [:clauses s/ALL (TAG :actions) :values s/ALL] event)
        updated (apply dsl/event-then (concat old actions))
        other (s/setval [s/ALL (TAG :actions)] s/NONE (:clauses event))]
    (assoc event :clauses (conj other updated))))

(defn- add-args
  "Adds *args* to *event*"
  [event args]
  (let [old (s/select [:clauses s/ALL (TAG :args) :values s/ALL] event)
        updated (apply dsl/event-any (concat old args))
        other (s/setval [s/ALL (TAG :args)] s/NONE (:clauses event))]
    (assoc event :clauses (conj other updated))))


(defn- append-name
  "Append *event* name with all *postfixes*"
  [event & postfixes]
  (update event :name (fn [n] (keyword (apply str (name n) postfixes)))))

;; Substitutions

(declare sub->events)

(defmulti sub->events
  "Expects the substitutions and a base-event and returns a translated as a list of events based on the base-event"
  (fn [_base-event ir & _args] (:tag ir)))

(defmethod sub->events :assignment [base-event ir]
  [(add-actions base-event ir)])

(defmethod sub->events :becomes-element-of [base-event ir]
  [(add-actions base-event ir)])

(defmethod sub->events :becomes-such [base-event ir]
  [(add-actions base-event ir)])

(defmethod sub->events :skip [base-event _ir]
  [base-event])

(defn- parallel-subs->sequential-subs [subs]
  `(~(apply butil/bsequential-sub subs)))

(defn- replace-subs-with-sequential-sub [ir key]
  (update ir key parallel-subs->sequential-subs))

(defn- parallel-subs->events [base-event subs]
  (reduce (fn [events sub] (mapcat #(sub->events % sub) events)) [base-event] subs))

(defn- sequential-subs->events [base-event subs]
  (parallel-subs->events base-event (parallel-subs->sequential-subs subs)))

(defmethod sub->events :any [base-event ir]
  (-> base-event
      (add-args (:ids ir))
      (add-guards (:pred ir))
      (sequential-subs->events (:subs ir))))

(defmethod sub->events :parallel-sub [base-event ir]
  (parallel-subs->events base-event (:subs ir)))

(defmethod sub->events :precondition [base-event ir]
  (-> (apply add-guards base-event
             (if (= (-> ir :pred :tag) :and)
               (-> ir :pred :preds)
               [(:pred ir)]))
      (sequential-subs->events (:subs ir))))

(defmethod sub->events :op-call->extends [base-event {:keys [event-names arg-names arg-vals]}]
  (assert (not (s/selected-any? [:clauses s/ALL (TAG :event-reference)] base-event))
          "An event can only refine one event")
  (map-indexed (fn [i event-name]
                 (-> (apply add-guards base-event (map butil/b= arg-names arg-vals))
                     (append-name "-" i)
                     (update :clauses conj (dsl/event-extends event-name))))
               event-names))

(defmethod sub->events :if-sub [base-event ir]
  (concat
   (-> base-event
       (append-name "-then")
       (add-guards (:cond ir))
       (sub->events (:then ir)))
   (-> base-event (append-name "-else")
       (add-guards (bnot (:cond ir)))
       (sub->events (or (:else ir) (butil/b skip))))))

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
                      (-> (apply add-guards base-event guards)
                          (append-name "-cond" i)
                          (sub->events sub)))
                    pairs))
            (when (odd? (count clauses))
              (sub->events
               (apply add-guards
                      (append-name base-event "-condelse")
                      (map bnot (take-nth 2 (butlast clauses))))
               (last clauses))))))

(defmethod sub->events :select [base-event {:keys [clauses]}]
  (if (= (count clauses) 2)
    (-> base-event
        (add-guards (first clauses))
        (sub->events (second clauses)))
    (concat (apply concat
                   (map-indexed
                    (fn [i [guard sub]]
                      (-> base-event
                          (append-name  "-select" i)
                          (add-guards guard)
                          (sub->events sub)))
                    (partition 2 clauses)))
            (when (odd? (count clauses))
              (sub->events
               (apply add-guards
                      (append-name base-event "-selectelse")
                      (map bnot (take-nth 2 (butlast clauses))))
               (last clauses))))))


(defn literal? [x] (or (keyword? x) (number? x)))

(defn literal-name [literal]
  (if (keyword? literal)
    (name literal)
    (str literal)))

(defmethod sub->events :case [base-event {:keys [expr cases]}]
  (concat
   (mapcat
    (fn [[case-literal sub]]
      (assert (literal? case-literal))
      (-> base-event
          (append-name "-" (literal-name case-literal))
          (add-guards (butil/b= case-literal expr))
          (sub->events sub)))
    (partition 2 cases))
   (when (odd? (count cases))
     (-> base-event
         (append-name "-caseelse")
         (add-guards (butil/bnot (butil/bmember? expr (set (take-nth 2 (butlast cases))))))
         (sub->events (last cases))))))

(defn- replace-vars-with-vals [ir id-vals]
  (let [replacement (apply hash-map id-vals)]
    (s/transform [ALL-KEYWORDS #(contains? replacement %)] replacement ir))) ;;FIXME: scoping

(defmethod sub->events :let-sub [base-event {:keys [id-vals subs]}]
  [(-> base-event
       (sequential-subs->events (map #(replace-vars-with-vals % id-vals) subs)))])

(defn op->events [ir]
  (sub->events
   (if (seq (:args ir))
     (eventb-event (:name ir) (apply dsl/event-any (:args ir)))
     (eventb-event (:name ir)))
   (:body ir)))

(comment
  (def MAP-NODES
    (s/recursive-path [] p
                      (s/if-path map?
                                 (s/continue-then-stay s/MAP-VALS p)))))

(defn replace-args-in-body
  "Replaces all occurrences of arguments with the values"
  [op values]
  (let [replacement (into {} (map vector (:args op) values))]
    (:body (s/transform [:body ALL-KEYWORDS #(contains? replacement %)] replacement op)))) ;;FIXME: scoping

(defn update-clause-values [ir clause f & args]
  (update
   (s/setval [(CLAUSE clause)] s/NONE ir)
   :machine-clauses
   conj
   {:tag clause :values (apply f (s/select [(CLAUSE clause) :values s/ALL] ir) args)}))

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

(defn remove-empty-clauses [ir] (s/setval [CLAUSES s/ALL #(empty? (:values %))] s/NONE ir))

(defn includes->inline
  "Inline all op-calls to included-machine in the base-machine only.
  Remove included-machine from the machine references"
  [base-machine included-machine]
  (let [operations (into {} (map (fn [op] [(:name op) op]))
                         (s/select [(CLAUSE :operations) :values s/ALL] included-machine))
        promoted (set (s/select [(CLAUSE :promotes) :values s/ALL operations] base-machine))]
    (if
     (s/selected-any? [(INCLUDES (:name included-machine))] base-machine)
      (-> (->> base-machine
               (s/setval [(INCLUDES (:name included-machine))] s/NONE)
               (s/setval [(CLAUSE :promotes) :values s/ALL promoted] s/NONE)
               (s/transform [(IR-NODE :op-call) #(operations (:op %))]
                            (fn [call]
                              (replace-args-in-body
                               (operations (:op call))
                               (:args call)))))
          (merge-clause included-machine :variables)
          (merge-clause included-machine :sets)
          (merge-clause included-machine :invariants)
          (merge-clause included-machine :constants)
          (merge-clause included-machine :properties)
          (merge-clause included-machine :init)
          (update-clause-values :operations concat (map operations promoted))
          remove-empty-clauses)
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
         (s/transform (IR-NODE :op-call) (partial op-call->extends included-machine)))
    base-machine))

(defn context-name [machine-name] (keyword (str (name machine-name) "-ctx")))

(defn extract-context
  "Extracts an Event-B context from an classical B machine"
  [ir]
  (let [partitions (map (fn [ir]
                          (apply dsl/eventb-partition (:id ir) (map (fn [x] #{x}) (:elems ir))))
                        (s/select [(CLAUSE :sets) :values s/ALL (TAG :enumerated-set)] ir))
        properties (s/select [(CLAUSE :properties) :values s/ALL] ir)
        constants (s/select [(s/multi-path
                              [(CLAUSE :constants) :values]
                              [(CLAUSE :sets) :values s/ALL (TAG :enumerated-set) :elems]) s/ALL]
                            ir)]
    (->> (dsl/eventb-context (context-name (:name ir))
                             (if (= :refinement (:tag ir))
                               (butil/bextends (context-name (:abstract-machine-name ir)))
                               {:tag :extends :values nil})
                             (apply butil/bsets (map butil/bdeferred-set (s/select [(CLAUSE :sets) :values s/ALL :id] ir)))
                             (apply butil/bconstants constants)
                             (apply butil/bproperties (concat partitions properties)))
         remove-empty-clauses)))


(defn extract-machine
  "Extracts an Event-B machine from an classical B machine. Operations are converted to Events"
  [ir]
  (let [events (apply dsl/eventb-events
                      (mapcat op->events (s/select [(CLAUSE :operations) :values s/ALL] ir)))
        inits (map #(replace-subs-with-sequential-sub % :values)
                   (s/select (CLAUSE :init) ir))
        sees (butil/bsees (context-name (:name ir)))
        ;; only keep "dynamic" machine-clauses
        machine (->> ir
                     (s/select [(s/multi-path
                                 (CLAUSE :invariants)
                                 (CLAUSE :variables)
                                 (CLAUSE :assertions))])
                     (concat inits)
                     (apply dsl/eventb-machine (:name ir) sees events)
                     remove-empty-clauses)]
    (if (= :refinement (:tag ir))
      (assoc machine :tag :refinement :abstract-machine-name (:abstract-machine-name ir))
      machine)))

;; Transform IR

(defmulti transform-ir
  #(or (:tag %) (type %))
  :default nil)

(defmethod transform-ir :fin [ir]
  (let [set-keyword (keyword (gensym "lisbset"))]
    (eventb
     (comprehension-set
      set-keyword (and (subset? set-keyword (:set ir))
                       (finite set-keyword))))))

(defmethod transform-ir :fin1 [ir]
  (let [set-keyword (keyword (gensym "lisbset"))]
    (eventb
     (comprehension-set
      set-keyword (and (subset? set-keyword (:set ir))
                       (finite set-keyword)
                       (not= set-keyword #{}))))))

(defmethod transform-ir :empty-sequence [_]
  (eventb #{}))

(defmethod transform-ir :sequence [ir]
  (let [tuples (set (map-indexed (fn [i v]
                                   `[~(inc i) -> ~v])
                                 (:elems ir)))]
    (lisb.translation.lisb2ir/bb tuples)))

(defmethod transform-ir :size [ir]
  (eventb (card (:seq ir))))

(defmethod transform-ir :seq [ir]
  (let [ident (keyword (gensym "n"))]
    (eventb (union-pe [ident]
                      (member? ident nat-set)
                      (total-function (interval 1 ident) (:set ir))))))

; default case
(defmethod transform-ir nil [ir]
  ir)

(defn transform-all-expressions
  "Replaces the B expression which are implemented by *transform-ir*
  with an equivalent Event-B construct"
  [ir]
  (s/transform IR-WALKER transform-ir ir))
