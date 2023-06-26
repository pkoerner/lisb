(ns lisb.translation.eventb.ir2eventb
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s])
  (:import
   (de.prob.model.eventb
    EventParameter
    EventBMachine
    EventBInvariant
    EventBAction
    EventBAxiom
    EventBGuard
    EventBVariable
    Event
    Event$EventType
    Event$Inheritance
    Context
    EventBConstant
    )
   (de.prob.model.representation
    Variable
    Machine
    Action
    Guard
    BEvent
    Invariant
    Axiom
    Constant
    )
   de.prob.animator.domainobjects.EventB
   de.prob.model.representation.ModelElementList))

(defn rodin-name [kw]
  (str/replace (name kw) #"-" "_"))

(defmulti ir-expr->str (fn [ir] (or (:tag ir) (class ir))))
(defmulti ir-pred->str :tag)

(defn chain-expr [op irs]
  (str/join op (map (fn [ir]
                      (if (:tag ir)
                        (str "(" (ir-expr->str ir) ")")
                        (ir-expr->str ir))) irs)))

(defn chain-pred [op ir]
   (str/join op (map ir-pred->str ir)))

;; Primitives

(defmethod ir-expr->str clojure.lang.Keyword [ir] (rodin-name ir))
(defmethod ir-expr->str java.lang.Long [ir] (str ir))

;; Boolean

(defmethod ir-expr->str java.lang.Boolean [ir] (str/upper-case (str ir)))
(defmethod ir-expr->str :bool-set [_] "BOOL")
(defmethod ir-expr->str :pred->bool [ir]
  (str "bool(" (ir-pred->str (:pred ir)) ")"))

;; Numbers

(defmethod ir-expr->str :add [ir] (chain-expr "+" (:nums ir)))
(defmethod ir-expr->str :sub [ir] (chain-expr "-" (:nums ir)))
(defmethod ir-expr->str :mul [ir] (chain-expr "*" (:nums ir)))
(defmethod ir-expr->str :div [ir] (chain-expr "/" (:nums ir)))
(defmethod ir-expr->str :mod [ir] (chain-expr "mod" (:nums ir)))
;;(defmethod ir-expr->str :pow [ir-expr] (chain-expr " ** " (:nums ir-expr)))

(defmethod ir-expr->str :integer-set [_] "INT")
(defmethod ir-expr->str :int-set [_] "INT")
(defmethod ir-expr->str :natural-set [_] "NAT")
(defmethod ir-expr->str :nat-set [_] "NAT")
(defmethod ir-expr->str :natural1-set [_] "NAT1")
(defmethod ir-expr->str :nat1-set [_] "NAT1")
(defmethod ir-expr->str :interval [ir]
  (str (ir-expr->str (:from ir)) ".." (ir-expr->str (:to ir))))
(defmethod ir-expr->str :max [ir]
  (str "max(" (ir-expr->str (:set ir)) ")"))
(defmethod ir-expr->str :min [ir]
  (str "min(" (ir-expr->str (:set ir)) ")"))
(defmethod ir-expr->str :successor [ir]
  (str (ir-expr->str (:num ir)) "+1"))
(defmethod ir-expr->str :predecessor [ir]
  (str (ir-expr->str (:num ir)) "-1"))

;; Logical predicates

(defmethod ir-pred->str :less [ir] (chain-expr "<" (:nums ir)))
(defmethod ir-pred->str :less-equals [ir] (chain-expr "<=" (:nums ir)))
(defmethod ir-pred->str :greater [ir] (chain-expr ">" (:nums ir)))
(defmethod ir-pred->str :greater-equals [ir] (chain-expr ">=" (:nums ir)))

(defmethod ir-pred->str :and [ir] (chain-pred "&" (:preds ir)))
(defmethod ir-pred->str :or [ir] (chain-pred " or " (:preds ir)))
(defmethod ir-pred->str :implication [ir] (chain-pred "=>" (:preds ir)))
(defmethod ir-pred->str :equivalence [ir] (chain-pred "<=>" (:preds ir)))
(defmethod ir-pred->str :not [ir]
  (str "not(" (ir-pred->str (:pred ir)) ")"))
(defmethod ir-pred->str :for-all [ir]
  (str "!" (str/join "," (map rodin-name (:ids ir))) "." (ir-pred->str (:implication ir))))
(defmethod ir-pred->str :exists [ir]
  (str "#" (str/join "," (map rodin-name (:ids ir)) (ir-pred->str (:pred ir)))))

;; Equality

(defmethod ir-pred->str :equals [ir]
  (str (ir-expr->str (:left ir)) "=" (ir-expr->str (:right ir))))

(defmethod ir-pred->str :not-equals [ir]
  (str (ir-expr->str (:left ir)) "/=" (ir-expr->str (:right ir))))

;; Sets

(defmethod ir-expr->str clojure.lang.PersistentHashSet [ir]
  (str "{" (str/join "," (map ir-expr->str ir)) "}"))

(defmethod ir-expr->str :comprehension-set [ir]
  (str "{"
       (str/join "," (map rodin-name (:ids ir))) "|"
       (ir-pred->str (:pred ir)) "}" ))

(defmethod ir-expr->str :power-set [ir]
  (str "POW(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :power1-set [ir]
  (str "POW1(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :card [ir]
  (str "card(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :unite-sets [ir]
  (str "union(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :unite-sets [ir]
  (str "inter(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :cardinality [ir]
  (str "card(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :cartesian-product [ir]
  (chain-expr "**" (:sets ir)))

(defmethod ir-expr->str :union [ir]
  (chain-expr "\\/" (:sets ir)))

(defmethod ir-expr->str :intersection [ir]
  (chain-expr "/\\" (:sets ir)))

(defmethod ir-pred->str :member [ir]
  (str (ir-expr->str (:elem ir)) ":" (ir-expr->str (:set ir))))

(defn chain-expr-explicit [op elems]
  (str/join "&" (map (fn [a b] (str (ir-expr->str a) op (ir-expr->str b)))
                     (butlast elems) (rest elems))))

(defmethod ir-pred->str :subset [ir]
  (chain-expr-explicit "<:" (:sets ir)))

(defmethod ir-pred->str :strict-subset [ir]
  (chain-expr-explicit "<<:" (:sets ir)))

(defmethod ir-pred->str :partition [ir]
  (str "partition(" (ir-expr->str (:set ir)) ","
       (str/join "," (map ir-expr->str (:partitions ir))) ")"))

;; Relations

(defmethod ir-expr->str :maplet [{:keys [left right]}]
  (chain-expr "|->" [left right]))

;; Functions

(defmethod ir-expr->str :partial-fn [ir]
  (chain-expr "+->" (:sets ir)))

(defmethod ir-expr->str :total-fn [ir]
  (chain-expr "-->" (:sets ir)))

(defmethod ir-expr->str :partial-surjection [ir]
  (chain-expr "+->>" (:sets ir)))

(defmethod ir-expr->str :total-surjection [ir]
  (chain-expr "-->>" (:sets ir)))

(defmethod ir-expr->str :partial-injection [ir]
  (chain-expr ">+>" (:sets ir)))

(defmethod ir-expr->str :total-injection [ir]
  (chain-expr ">->" (:sets ir)))

(defmethod ir-expr->str :partial-bijection [ir]
  (chain-expr ">+>>" (:sets ir)))

(defmethod ir-expr->str :total-bijection [ir]
  (chain-expr ">->>" (:sets ir)))

(defn tuple->maplet [tuple]
  (reduce (fn [acc cur] {:tag :maplet :left acc :right cur})
           tuple))

(defmethod ir-expr->str :lambda [{:keys [ids pred expr]}]
  ;;TODO: In Event-B ids can be arbitrarily nested.
  (str "%" (ir-expr->str (tuple->maplet ids)) "." (ir-pred->str pred) "|" (ir-expr->str expr)))

 ;; TODO: allow multiple args
(defmethod ir-expr->str :fn-call [ir]
  (str (ir-expr->str (:f ir)) "(" (ir-expr->str (first (:args ir))) ")"))

;; Construct ProB Model

(defn id-vals->ids [id-vals]
  (map (comp ir-expr->str first) (partition 2 id-vals)))

(defn id-vals->vals [id-vals]
  (map (comp ir-expr->str second) (partition 2 id-vals)))

;; TODO: If more then 1 id is present no val can be a :fn-call.

(defmulti ir-sub->strs :tag)

(defmethod ir-sub->strs :becomes-element-of [{:keys [ids set]}]
 [(str (str/join "," (map ir-expr->str ids)) " :: " (ir-expr->str set))])

(defmethod ir-sub->strs :becomes-such [{:keys [ids pred]}]
  [(str (str/join "," (map ir-expr->str ids)) " :| " (ir-pred->str pred))])

(defmethod ir-sub->strs :assignment [ir]
  (let [ids (id-vals->ids (:id-vals ir))
        vals (id-vals->vals (:id-vals ir))
        code (str (str/join "," ids) " := " (str/join "," vals))]
    [code]))

(defmethod ir-sub->strs :parallel-sub [ir]
  (mapcat ir-sub->strs (:subs ir)))

(defn find-clause [tag clauses]
  (->> clauses
       (filter #(= tag (:tag %)))
       first))

(defn extract-invariants [clauses]
  (let [invariant (map-indexed
                   (fn [i pred] (EventBInvariant. (str "inv" i) (ir-pred->str pred) false #{}))
                   (:values (find-clause :invariants clauses)))
        theorems  (map-indexed
                   (fn [i pred] (EventBInvariant. (str "thm" i) (ir-pred->str pred) false #{}))
                   (:values (find-clause :assertions clauses)))]
    (ModelElementList. (concat invariant theorems))))

(defn extract-axioms [clauses]
  (let [axioms   (map-indexed
                  (fn [i pred] (EventBAxiom. (str "axm" i) (ir-pred->str pred) false #{}))
                  (:values (find-clause :properties clauses)))
        theorems (map-indexed
                  (fn [i pred] (EventBAxiom. (str "thm" i) (ir-pred->str pred) false #{}))
                  (:values (find-clause :assertions clauses)))]
    (ModelElementList. (concat axioms theorems))))

(defn new-event [name status inheritance]
  (let [inheritance (case inheritance
                      :refines Event$Inheritance/REFINES
                      :extends Event$Inheritance/EXTENDS
                      Event$Inheritance/NONE)
        status (case status
                    :convergent Event$EventType/CONVERGENT
                    :anticipated Event$EventType/ANTICIPATED
                    Event$EventType/ORDINARY)]
    (Event. name status inheritance)))


(defmulti ir->prob :tag)

(defn clause->prob [tag clauses] (ir->prob (find-clause tag clauses)))

(defn extract-events [clauses]
  (let [events (or (clause->prob :events clauses) ())
        init   (find-clause :init clauses)]
    (ModelElementList. (if init
                         (cons (ir->prob init) events)
                         events))))

(defmethod ir->prob :init [{:keys [values]}]
 (->> values
      (mapcat ir-sub->strs)
      (map-indexed (fn [i code] (EventBAction. (str "init" i) code #{})))
      ModelElementList.
      (.withActions (new-event "INITIALISATION" :ordinary :none))))

(defmethod ir->prob :events [{:keys [values]}]
  (ModelElementList. (map ir->prob values)))

(defmethod ir->prob :args [{:keys [values]}]
  (ModelElementList. (map (fn [x] (EventParameter. (clojure.core/name x))) values)))

(defmethod ir->prob :guards [{:keys [values]}]
 (ModelElementList. (map-indexed (fn [i x] (EventBGuard. (str "grd" i) (ir-pred->str x) false #{})) values)))

(defmethod ir->prob :witnesses [{:keys [values]}]
  (ModelElementList. (map ir->prob values)))

(defmethod ir->prob :actions [{:keys [values]}]
  (ModelElementList. (map-indexed
                       (fn [i code] (EventBAction. (str "act" i) code #{}))
                       (mapcat ir-sub->strs values))))

(defmethod ir->prob :event [{:keys [name event-clauses]}]
  (let [parent-event (find-clause :event-reference event-clauses)
        e (-> (new-event (rodin-name name)
                     (:value (find-clause :status event-clauses))
                     (:type parent-event))
              (.withParameters (clause->prob :args event-clauses))
              (.withGuards (clause->prob :guards event-clauses))
              (.withWitnesses (clause->prob :witnesses event-clauses))
              (.withActions (clause->prob :actions event-clauses)))]
    (if (:value parent-event)
      ;;TODO: get actual event
      (.withParentEvent e (new-event (rodin-name (:value parent-event)) :ordinary :none))
      e)))

(defmethod ir->prob :sets [{:keys [values]}]
  (ModelElementList.
   (map (fn [{:keys [id]}]
          (de.prob.model.representation.Set. (EventB. (rodin-name id))))
        values)))

(defmethod ir->prob :constants [{:keys [values]}]
  (ModelElementList. (map (fn [x] (EventBConstant. (rodin-name x) false "")) values)))

(defmethod ir->prob :variables [{:keys [values]}]
  (ModelElementList. (map (fn [x] (EventBVariable. (rodin-name x) "")) values)))

(defmethod ir->prob :refinement [ir]
  (-> (ir->prob (assoc ir :tag :machine))
      ;;TODO: get real machine-clauses
      (.withRefinesMachine (EventBMachine. (rodin-name (:abstract-machine-name ir))))))

(defmethod ir->prob :sees [{:keys [values]}]
  ;;TODO: get real context
  (ModelElementList. (map (fn [x] (Context. (rodin-name x))) values)))

(defmethod ir->prob :machine [{m-name :name clauses :machine-clauses}]
 (-> (EventBMachine. (rodin-name m-name))
      (.withSees (clause->prob :sees clauses))
      (.withInvariants (extract-invariants clauses))
      (.withVariant (clause->prob :variant clauses))
      (.withEvents (extract-events clauses))
      (.withVariables (clause->prob :variables clauses))))

(defmethod ir->prob :extends [{:keys [values]}]
  (ModelElementList. (map (fn [c-ref] (Context. (rodin-name (:name c-ref)))) values)))

(defmethod ir->prob :context [{c-name :name clauses :machine-clauses}]
  (-> (Context. (rodin-name c-name))
      (.withExtends (clause->prob :extends clauses))
      (.withSets (clause->prob :sets clauses))
      (.withConstants (clause->prob :constants clauses))
      (.withAxioms (extract-axioms clauses))))

(defmethod ir->prob nil [_] nil)


