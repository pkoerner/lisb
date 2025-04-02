(ns lisb.translation.eventb.b2eventb
  (:require
   [com.rpl.specter :as s]
   [lisb.translation.eventb.dsl :refer [eventb-event eventb-finite] :as dsl]
   [lisb.translation.eventb.specter-util :refer [INCLUDES NAME]]
   [lisb.translation.irtools :refer [ALL-KEYWORDS CLAUSE CLAUSES IR-NODE
                                     IR-WALKER TAG]]
   [lisb.translation.types :refer [->Tuple]]
   [lisb.translation.util :as butil]))


(defn- add-guards
  "Adds *guards* to *event*"
  [event & guards]
  (let [old (s/select [:clauses s/ALL (TAG :guards) :values s/ALL] event)
        updated (apply dsl/event-when (concat old guards))
        other (s/setval [s/ALL (TAG :guards)] s/NONE (:clauses event))]
    (assoc event :clauses (conj other updated))))

(defn- add-multi-guard [event guard]
  (if (= (:tag guard) :and)
    (apply add-guards event (:preds guard))
    (add-guards event guard)))

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

;; Build events

(declare sub->events)
(declare expr->events)
(declare pred->events)

(defn preds-exprs-reducer [event preds exprs]
  (reduce (fn [event-exprs-preds [type pred-or-expr]]
            (mapcat (fn [[event preds exprs]]
                      (case type
                        :expr (map (fn [[new-event new-expr]]
                                     [new-event preds (conj exprs new-expr)])
                                   (expr->events event pred-or-expr))
                        :pred (map (fn [[new-event new-pred]]
                                     [new-event (conj preds new-pred) exprs])
                                   (pred->events event pred-or-expr))))
                    event-exprs-preds))
          [[event [] []]]
          (concat (map vector (repeat :pred) preds)
                  (map vector (repeat :expr) exprs))))

(defn preds-reducer [event preds]
  (->> (preds-exprs-reducer event preds [])
       (map (fn [[event preds _exprs]]
              [event preds]))))

(defn pred->ir [event ir & keys]
  (->> ir
       ((apply juxt keys))
       (preds-reducer event)
       (map (fn [[event preds]]
              (assert (= (count keys) (count preds)))
              [event (apply assoc ir (interleave keys preds))]))))

(defn preds->ir [event ir key]
  (->> ir
       key
       (preds-reducer event)
       (map (fn [[event preds]]
              [event (assoc ir key preds)]))))

(defn exprs-reducer [event exprs]
  (->> (preds-exprs-reducer event [] exprs)
       (map (fn [[event _preds exprs]]
              [event exprs]))))

(defn expr->ir [event ir & keys]
  (->> ir
       ((apply juxt keys))
       (exprs-reducer event)
       (map (fn [[event exprs]]
              (assert (= (count keys) (count exprs)))
              [event (apply assoc ir (interleave keys exprs))]))))

(defn exprs->ir [event ir key]
  (->> ir
       key
       (exprs-reducer event)
       (map (fn [[event exprs]]
              [event (assoc ir key exprs)]))))

(defn- parallel-subs->sequential-subs [subs]
  [(apply butil/bsequential-sub subs)])

(defn- replace-subs-with-sequential-sub [ir key]
  (update ir key parallel-subs->sequential-subs))

(defn- parallel-subs->events [base-event subs]
  (reduce (fn [events sub]
            (mapcat #(sub->events % sub) events))
          [base-event]
          subs))

(defn- sequential-subs->events [base-event subs]
  (parallel-subs->events base-event (parallel-subs->sequential-subs subs)))

(defmulti expr->events
  "Expects the expression and a base-event and returns a list of pairs of events based on the base-event and expr"
  (fn [_base-event ir & _args] (or (:tag ir) (type ir)))
  :default nil)

(defmethod expr->events nil [base-event ir]
  (cond
    (map? ir) (case (-> ir keys set (disj :tag))
                #{} [[base-event ir]]
                #{:expr} (expr->ir base-event ir :expr)
                #{:from :to} (expr->ir base-event ir :from :to)
                #{:num} (expr->ir base-event ir :num)
                (#{:pred} #{:ids :pred}) (pred->ir base-event ir :pred)
                #{:rel} (expr->ir base-event ir :rel)
                #{:seq} (expr->ir base-event ir :seq)
                #{:set} (expr->ir base-event ir :set)
                #{:rel :set} (expr->ir base-event ir :rel :set)
                #{:set1 :set2} (expr->ir base-event ir :set1 :set2)
                #{:elems} (exprs->ir base-event ir :elems)
                #{:nums} (exprs->ir base-event ir :nums)
                #{:nums-or-sets} (exprs->ir base-event ir :nums-or-sets)
                #{:rels} (exprs->ir base-event ir :rels)
                #{:sets} (exprs->ir base-event ir :sets)
                #{:sets-of-sets} (exprs->ir base-event ir :sets-of-sets)
                #{:ids :pred :expr} (->> (preds-exprs-reducer base-event [(:pred ir)] [(:expr ir)])
                                         (map (fn [[event [pred] [expr]]]
                                                [event (assoc ir :pred pred :expr expr)])))
                #{:elems :seq} (->> (apply vector (:seq ir) (:elems ir))
                                    (exprs-reducer base-event)
                                    (map (fn [[event [seq & elems]]]
                                           [event (assoc ir :seq seq :elems elems)])))
                #{:f :args} (->> (apply vector (:f ir) (:args ir))
                                 (exprs-reducer base-event)
                                 (map (fn [[event [f & args]]]
                                        [event (assoc ir :f f :args args)]))))
    (set? ir) (->> ir
                   (exprs-reducer base-event)
                   (map (fn [[event exprs]]
                          [event (set exprs)])))
    (instance? lisb.translation.types.Tuple ir) (->> ir
                                                     (exprs-reducer base-event)
                                                     (map (fn [[event exprs]]
                                                            [event (->Tuple exprs)])))
    (or (keyword? ir)
        (string? ir)
        (integer? ir)
        (float? ir)
        (boolean? ir)
        (nil? ir)) [[base-event ir]]
    :else (throw (IllegalArgumentException. (str "unsupported ir: " ir)))))

(defmethod expr->events :if [base-event {:keys [cond then else]}]
  (-> base-event
      (preds-exprs-reducer [cond] [then else])
      (->>
       (map (fn [[event [cond] [then else]]]
              [event
               (butil/bfn-call
                (butil/bunion (let [sym (keyword (gensym "t"))]
                                (butil/blambda [sym]
                                               (butil/band (butil/bmember? sym #{true})
                                                           cond)
                                               then))
                              (let [sym (keyword (gensym "t"))]
                                (butil/blambda [sym]
                                               (butil/band (butil/bmember? sym #{true})
                                                           (butil/bnot cond))
                                               else)))
                true)])))))

(defmethod expr->events :let [base-event ir]
  (-> base-event
      (exprs-reducer (concat (:id-vals ir) [(:expr-or-pred ir)]))
      (->> (map (fn [[event exprs]]
                  [event (assoc ir :id-vals (butlast exprs) :expr-or-pred (last exprs))])))))

(defmulti pred->events
  "Expects the predicate and a base-event and returns a list of pairs of events based on the base-event and pred"
  (fn [_base-event ir & _args] (or (:tag ir) (type ir)))
  :default nil)

(defmethod pred->events nil [base-event ir]
  (cond
    (map? ir) (case (-> ir keys set (disj :tag))
                #{} [[base-event ir]]
                #{:elem :set} (expr->ir base-event ir :elem :set)
                #{:implication} (pred->ir base-event ir :implication)
                #{:left :right} (expr->ir base-event ir :left :right)
                #{:pred} (pred->ir base-event ir :pred)
                #{:nums} (exprs->ir base-event ir :nums)
                #{:preds} (preds->ir base-event ir :preds)
                #{:sets} (exprs->ir base-event ir :sets))
    (or (keyword? ir)
        (string? ir)
        (integer? ir)
        (float? ir)
        (boolean? ir)
        (nil? ir)) [[base-event ir]]
    :else (throw (IllegalArgumentException. (str "unsupported ir: " ir)))))

(defmethod pred->events :if [base-event {:keys [cond then else]}]
  (-> base-event
      (preds-reducer [cond then else])
      (->>
       (map (fn [[event [cond then else]]]
              [event
               (butil/band
                (butil/bimplication cond then)
                (butil/bimplication (butil/bnot cond) else))])))))

(defmethod pred->events :let [base-event ir]
  (-> base-event
      (preds-exprs-reducer [(:expr-or-pred ir)] (:id-vals ir)) ; might want to have custom logic where the exprs are processed first
      (->> (map (fn [[event [pred] id-vals]]
                  [event (assoc ir :id-vals id-vals :expr-or-pred pred)])))))

(defmulti sub->events
  "Expects the substitutions and a base-event and returns a list of events based on the base-event"
  (fn [_base-event ir & _args] (or (:tag ir) (type ir)))
  :default nil)

(defmethod sub->events :assignment [base-event ir]
  (-> base-event
      (exprs-reducer (:id-vals ir))
      (->>
       (map (fn [[event id-vals]]
              (-> event
                  (add-actions (assoc ir :id-vals id-vals))))))))

(defmethod sub->events :becomes-element-of [base-event ir]
  (-> base-event
      (exprs-reducer (concat (:ids ir) [(:set ir)]))
      (->>
       (map (fn [[event exprs]]
              (-> event
                  (add-actions (assoc ir :ids (butlast exprs) :set (last exprs)))))))))

(defmethod sub->events :becomes-such [base-event ir]
  (-> base-event
      (preds-exprs-reducer [(:pred ir)] (:ids ir)) ; might want to have custom logic where the exprs are processed first
      (->>
       (map (fn [[event [pred] ids]]
              (-> event
                  (add-actions (assoc ir :ids ids :pred pred))))))))

(defmethod sub->events :skip [base-event _ir]
  [base-event])

(defmethod sub->events :any [base-event ir]
  (-> base-event
      (preds-exprs-reducer [(:pred ir)] (:ids ir))
      (->>
       (mapcat (fn [[event [pred] ids]]
                 (-> event
                     (add-args ids)
                     (add-guards pred)
                     (sequential-subs->events (:subs ir))))))))

(defmethod sub->events :parallel-sub [base-event ir]
  (parallel-subs->events base-event (:subs ir)))

(defmethod sub->events :precondition [base-event ir]
  (-> base-event
      (preds-reducer [(:pred ir)])
      (->>
       (mapcat (fn [[event [pred]]]
                 (-> event
                     (add-guards pred)
                     (sequential-subs->events (:subs ir))))))))

(defmethod sub->events :op-call->extends [base-event {:keys [event-names arg-names arg-vals]}]
  (assert (not (s/selected-any? [:clauses s/ALL (TAG :event-reference)] base-event))
          "An event can only refine one event")
  (->> event-names
       (map-indexed (fn [i event-name]
                      (-> base-event
                          (exprs-reducer (interleave arg-names arg-vals))
                          (->>
                           (mapcat (fn [[event args]]
                                     (-> event
                                         (append-name "-" i)
                                         (apply add-guards (apply butil/band
                                                                  (map butil/b=
                                                                       (take-nth 2 args)
                                                                       (take-nth 2 (rest args)))))
                                         (update :clauses conj (dsl/event-extends event-name)))))))))
       (mapcat identity)))

(defmethod sub->events :if-sub [base-event ir]
  (-> base-event
      (preds-reducer [(:cond ir)])
      (->> (mapcat (fn [[event [cond]]]
                     (let [event-thens (-> event
                                           (append-name "-then")
                                           (add-guards cond)
                                           (sub->events (:then ir)))
                           event-elses (-> event
                                           (append-name "-else")
                                           (add-guards (butil/bnot cond))
                                           (sub->events (or (:else ir) butil/bskip)))]
                       (concat event-thens event-elses)))))))

(defmethod sub->events :cond [base-event {:keys [clauses]}]
  (let [conds (map first (partition 2 clauses))
        subs (map second (partition 2 clauses))
        guards (map (partial apply butil/band)
                    (map-indexed (fn [i cond]
                                   (concat (map butil/bnot (take i conds))
                                           cond
                                           (map butil/bnot (drop (inc i) conds))))
                                 conds))
        pairs (concat (map-indexed (fn [i [guard sub]]
                                     [(str "-cond" i) guard sub])
                                   (map vector guards subs))
                      (when (odd? (count clauses))
                        [["-condelse" (apply butil/band (map butil/bnot conds)) (last clauses)]]))]
    (mapcat (fn [[suffix guard sub]]
              (-> base-event
                  (append-name suffix)
                  (preds-reducer [guard])
                  (->>
                   (mapcat (fn [[event [guard]]]
                             (-> event
                                 (add-guards guard)
                                 (sub->events sub)))))))
            pairs)))

(defmethod sub->events :select [base-event {:keys [clauses]}]
  (if (= (count clauses) 2)
    (let [[pred sub] clauses]
      (-> base-event
          (preds-reducer [pred])
          (->>
           (mapcat (fn [[event [pred]]]
                     (-> event
                         (add-guards pred)
                         (sub->events sub)))))))
    (concat (mapcat identity
                    (->> clauses
                         (partition 2)
                         (map-indexed (fn [i [guard sub]]
                                        (-> base-event
                                            (append-name  "-select" i)
                                            (preds-reducer [guard])
                                            (->>
                                             (mapcat (fn [[event [guard]]]
                                                       (-> event
                                                           (add-guards guard)
                                                           (sub->events sub))))))))))
            (when (odd? (count clauses))
              (-> base-event
                  (append-name  "-selectelse")
                  (preds-reducer [(apply butil/band (map butil/bnot (map first (partition 2 clauses))))])
                  (->>
                   (mapcat (fn [[event [guard]]]
                             (-> event
                                 (add-guards guard)
                                 (sub->events (last clauses)))))))))))

(defmethod sub->events :case [base-event {:keys [expr cases]}]
  (-> base-event
      (exprs-reducer [expr])
      (->> (mapcat (fn [[event [expr]]]
                     (concat
                      (->> cases
                           (partition 2)
                           (map-indexed (fn [i [case-expr sub]]
                                          (-> event
                                              (append-name  "-case" i)
                                              (exprs-reducer [case-expr])
                                              (->>
                                               (mapcat (fn [[event [case-expr]]]
                                                         (-> event
                                                             (add-guards (butil/b= expr case-expr))
                                                             (sub->events sub))))))))
                           (mapcat identity))
                      (when (odd? (count cases))
                        (-> event
                            (append-name  "-caseelse")
                            (preds-reducer [(butil/bnot (butil/bmember? expr (set (map first (partition 2 cases)))))])
                            (->>
                             (mapcat (fn [[event [guard]]]
                                       (-> event
                                           (add-guards guard)
                                           (sub->events (last cases))))))))))))))

(defn- replace-vars-with-vals [ir id-vals]
  (let [replacement (apply hash-map id-vals)]
    (s/transform [ALL-KEYWORDS #(contains? replacement %)] replacement ir))) ;;FIXME: scoping

(defmethod sub->events :let-sub [base-event {:keys [id-vals subs]}]
  (-> base-event
      (sequential-subs->events (map #(replace-vars-with-vals % id-vals) subs))))

(defn op->events [ir]
  (sub->events
   (if (seq (:args ir))
     (eventb-event (:name ir) (apply dsl/event-any (:args ir)))
     (eventb-event (:name ir)))
   (:body ir)))

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

;; B -> EventB

(declare transform-ir transform-all-expressions)

(defn extract-context
  "Extracts an Event-B context from an classical B machine"
  [ir]
  (let [ir (transform-all-expressions ir)
        partitions (map (fn [ir]
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
  (let [ir (transform-all-expressions ir)
        events (apply dsl/eventb-events
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

(defn transform-all-expressions
  "Replaces the B expression which are implemented by *transform-ir*
  with an equivalent Event-B construct"
  [ir]
  (s/transform IR-WALKER transform-ir ir))

(defmulti transform-ir
  #(or (:tag %) (type %))
  :default nil)

(defmethod transform-ir :fin [{:keys [set]}]
  (let [set-keyword (keyword (gensym "lisbset"))]
    (butil/bcomprehension-set
     [set-keyword]
     (butil/band (butil/bsubset? set-keyword set)
                 (eventb-finite set-keyword)))))

(defmethod transform-ir :fin1 [{:keys [set]}]
  (let [set-keyword (keyword (gensym "lisbset"))]
    (butil/bcomprehension-set
     [set-keyword]
     (butil/band (butil/bsubset? set-keyword set)
                 (eventb-finite set-keyword)
                 (butil/bnot= set-keyword #{})))))

(defmethod transform-ir :empty-sequence [_]
  #{})

(defmethod transform-ir :sequence [{:keys [elems]}]
  (set (map-indexed (fn [i v]
                      (->Tuple [(inc i) v]))
                    elems)))

(defmethod transform-ir :size [{:keys [seq]}]
  (butil/bcard seq))

(defmethod transform-ir :tail [{:keys [seq]}]
  (let [sym (keyword (gensym "i"))]
    (butil/blambda [sym]
                   (butil/bmember? sym (butil/binterval 1 (butil/b- (butil/bcard seq) 1)))
                   (butil/bfn-call seq (butil/b+ sym 1)))))

(defmethod transform-ir :append [{:keys [seq elems]}]
  (butil/bunion seq
                (set (map-indexed
                      (fn [i x]
                        (->Tuple [(butil/b+ (butil/bcard seq) (inc i)) x]))
                      elems))))

(defmethod transform-ir :seq [{:keys [set]}]
  (let [sym (keyword (gensym "n"))]
    (butil/bunion-pe [sym]
                     (butil/bmember? sym butil/bnat-set)
                     (butil/btotal-function (butil/binterval 1 sym) set))))

(defmethod transform-ir :let [{:keys [id-vals expr-or-pred]}]
  (replace-vars-with-vals expr-or-pred id-vals))

; default case
(defmethod transform-ir nil [ir]
  ir)
