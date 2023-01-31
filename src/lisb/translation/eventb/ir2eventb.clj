(ns lisb.translation.eventb.ir2eventb
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [lisb.translation.util :as util])
  (:import
   (de.prob.model.eventb
    EventBMachine
    EventBInvariant
    EventBAction
    EventBGuard
    EventBVariable
    Event
    Event$EventType
    Context)
   (de.prob.model.representation
    Variable
    Action
    Guard
    BEvent
    Invariant)
   de.prob.model.representation.ModelElementList))

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

(defmethod ir-expr->str clojure.lang.Keyword [ir] (name ir))
(defmethod ir-expr->str java.lang.Long [ir] (str ir))

;; Boolean

(defmethod ir-expr->str java.lang.Boolean [ir] (str/upper-case (str ir)))
(defmethod ir-expr->str :bool-set [_] "BOOL")
(defmethod ir-expr->str :pred->bool [ir]
  (str "bool(" (ir-expr->str (:pred ir)) ")"))

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
  (str "not(" (ir-pred->str (:pred ir) ")")))
(defmethod ir-pred->str :for-all [ir]
  (str "!" (str/join "," (map name (:ids ir))) "." (ir-pred->str (:implementation ir))))
(defmethod ir-pred->str :exists [ir]
  (str "#" (str/join "," (map name (:ids ir)) (ir-pred->str (:pred ir)))))

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
       (str/join "," (map name (:ids ir))) "|"
       (ir-expr->str (:pred ir)) "}" ))

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

;; Relations

;; Functions

 ;; TODO: allow multiple args
(defmethod ir-expr->str :fn-call [ir]
  (str (name (:f ir)) "(" (name (first (:args ir))) ")"))

;; Construct ProB Model

(defn id-vals->ids [id-vals]
  (map (comp ir-expr->str first) (partition 2 id-vals)))

(defn id-vals->vals [id-vals]
  (map (comp ir-expr->str second) (partition 2 id-vals)))

(defn action [name code] (EventBAction. name code #{}))

(defn event
  ([name] (Event. name Event$EventType/ORDINARY false))
  ([name actions] (.set (event name) Action (ModelElementList. actions)))
  ([name guards actions] (.set (event name actions) Guard (ModelElementList. guards))))

(defn guard [name code] (EventBGuard. name code false #{}))

(defn invariant [name code] (EventBInvariant. name code false #{}))

(defn variable [name] (EventBVariable. name nil))

(defn has-tag [tag] (fn [ir] (= tag (:tag ir))))

;; TODO: If more then 1 id is present non of them can be a :fn-call.
(defn extract-actions-code
  [ir]
  (case (:tag ir)
    :assignment
    (let [ids (id-vals->ids (:id-vals ir))
          vals (id-vals->vals (:id-vals ir))
          code (str (str/join "," ids) " := " (str/join "," vals))]
      [code])

    :parallel-sub
    (mapcat extract-actions-code (:subs ir))

    :precondition
    (extract-actions-code (first (:subs ir)))))

(defn extract-actions [ir]
  (map-indexed (fn [i code] (action (str "act" i) code))
               (extract-actions-code ir)))

(defn extract-guards [ir]
  (case (:tag ir)
    :precondition [(guard "grd0" (ir-pred->str (:pred ir)))]
    []))

(defn op->event [op]
  (let [actions (extract-actions (:body op))
        guards (extract-guards (:body op))]
     (event (name (:name op)) guards actions)))

(defn find-first [tag clauses]
  (->> clauses
       (filter #(= tag (:tag %)))
       first
       :values))

(defn extract-events [clauses]
  (map op->event (find-first :operations clauses)))

(defn extract-init [clauses]
  (->> clauses
       (find-first :init)
       (mapcat extract-actions-code)
       (map-indexed (fn [i code] (action (str "init" i) code)))
       (event "INITIALISATION")))

(defn extract-invariants [clauses]
  (map-indexed
   (fn [i pred] (invariant (str "inv" i) (ir-pred->str pred)))
   (find-first :invariants clauses)))

(defn extract-variables [clauses]
  (map (comp variable name) (find-first :variables clauses)))


(defn set-invariants [m invs]
  (.set m Invariant (ModelElementList. invs)))

(defn set-events [m events]
  (-> m
      (.set Event (ModelElementList. events))
      (.set BEvent (ModelElementList. events))))

(defn set-variables [m vars]
  (.set m Variable (ModelElementList. vars)))

(defn ir->eventb-machine [ir]
  (assert (= :machine (:tag ir)))
  (let [m-name (-> ir :name name (str/replace #"-" "_")) ;; rodin does not allow "-" in machine names
        clauses (:machine-clauses ir)]
    (-> (EventBMachine. m-name)
        (set-invariants (extract-invariants clauses))
        (set-events (conj (extract-events clauses) (extract-init clauses)))
        (set-variables (extract-variables clauses)))))

