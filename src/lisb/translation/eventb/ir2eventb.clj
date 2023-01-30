(ns lisb.translation.eventb.ir2eventb
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [lisb.prob.animator :refer [injector]]
            [lisb.translation.util :as util])
  (:import
   de.prob.animator.domainobjects.EventB
   de.prob.model.eventb.translate.ModelToXML
   (de.prob.model.eventb
    EventBMachine
    EventBInvariant
    EventBAction
    EventBGuard
    EventBVariable
    EventBModel
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

(defmulti ir-node->str (fn [ir-node] (or (:tag ir-node) (class ir-node))))

(defn chain->str [op elems]
  (str/join "&"
            (map (fn [a b] (str (ir-node->str a) op (ir-node->str b)))
                 (butlast elems) (rest elems))))

(defn apply->str [op args]
  (str "(" (str/join op (map ir-node->str args)) ")"))

;; To Strings

(defmethod ir-node->str clojure.lang.Keyword [ir-node] (name ir-node))

(defmethod ir-node->str java.lang.Long [ir-node] (str ir-node))

;; Boolean

(defmethod ir-node->str java.lang.Boolean [ir-node] (str/upper-case (str ir-node)))

(defmethod ir-node->str :bool-set [ir-node] "BOOL")

(defmethod ir-node->str :pred->bool [ir-node]
  (str "bool(" (ir-node->str (:pred ir-node)) ")"))

;; Numbers

(defmethod ir-node->str :add [ir-node] (apply->str " + " (:nums ir-node)))
(defmethod ir-node->str :sub [ir-node] (apply->str " - " (:nums ir-node)))
(defmethod ir-node->str :cart-or-mul [ir-node] (apply->str "*" (:nums-or-sets ir-node)))
(defmethod ir-node->str :div [ir-node] (apply->str " / " (:nums ir-node)))
(defmethod ir-node->str :mod [ir-node] (apply->str " mod " (:nums ir-node)))
;;(defmethod ir-node->str :pow [ir-node] (apply->str " ** " (:nums ir-node)))
(defmethod ir-node->str :less [ir-node] (apply->str " < " (:nums ir-node)))
(defmethod ir-node->str :less-equals [ir-node] (apply->str " <= " (:nums ir-node)))
(defmethod ir-node->str :greater [ir-node] (apply->str " > " (:nums ir-node)))
(defmethod ir-node->str :greater-equals [ir-node] (apply->str " >= " (:nums ir-node)))

(defmethod ir-node->str :integer-set [ir-node] "INT")
(defmethod ir-node->str :int-set [ir-node] "INT")
(defmethod ir-node->str :natural-set [ir-node] "NAT")
(defmethod ir-node->str :nat-set [ir-node] "NAT")
(defmethod ir-node->str :natural1-set [ir-node] "NAT1")
(defmethod ir-node->str :nat1-set [ir-node] "NAT1")
(defmethod ir-node->str :interval [ir-node]
  (str (ir-node->str (:from ir-node)) ".." (ir-node->str (:to ir-node))))
(defmethod ir-node->str :max [ir-node]
  (str "max(" (ir-node->str (:set ir-node)) ")"))
(defmethod ir-node->str :min [ir-node]
  (str "min(" (ir-node->str (:set ir-node)) ")"))
(defmethod ir-node->str :successor [ir-node]
  (str (ir-node->str (:num ir-node)) "+1"))
(defmethod ir-node->str :predecessor [ir-node]
  (str (ir-node->str (:num ir-node)) "-1"))

;; Logical predicates

(defmethod ir-node->str :and [ir-node] (apply->str " & " (:preds ir-node)))
(defmethod ir-node->str :or [ir-node] (apply->str " or " (:preds ir-node)))
(defmethod ir-node->str :implication [ir-node] (apply->str " => " (:preds ir-node)))
(defmethod ir-node->str :equivalence [ir-node] (apply->str " <=> " (:preds ir-node)))
(defmethod ir-node->str :not [ir-node]
  (str "not(" (ir-node->str (:pred ir-node) ")")))
(defmethod ir-node->str :for-all [ir-node]
  (str "!" (str/join "," (map name (:ids ir-node))) "." (ir-node->str (:implementation ir-node))))
(defmethod ir-node->str :exists [ir-node]
  (str "#" (str/join "," (map name (:ids ir-node)) (ir-node->str (:pred ir-node)))))

;; Equality

(defmethod ir-node->str :equals [ir-node]
  (str (ir-node->str (:left ir-node)) "=" (ir-node->str (:right ir-node))))

(defmethod ir-node->str :not-equals [ir-node]
  (str (ir-node->str (:left ir-node)) "/=" (ir-node->str (:right ir-node))))

;; Sets

(defmethod ir-node->str clojure.lang.PersistentHashSet [ir-node]
  (str "{" (str/join "," (map ir-node->str ir-node)) "}"))

(defmethod ir-node->str :comprehension-set [ir-node]
  (str "{"
       (str/join "," (map name (:ids ir-node))) "|"
       (ir-node->str (:pred ir-node)) "}" ))

(defmethod ir-node->str :power-set [ir-node]
  (str "POW(" (ir-node->str (:set ir-node)) ")"))

(defmethod ir-node->str :power1-set [ir-node]
  (str "POW1(" (ir-node->str (:set ir-node)) ")"))

(defmethod ir-node->str :card [ir-node]
  (str "card(" (ir-node->str (:set ir-node)) ")"))

(defmethod ir-node->str :unite-sets [ir-node]
  (str "union(" (ir-node->str (:set ir-node)) ")"))

(defmethod ir-node->str :unite-sets [ir-node]
  (str "inter(" (ir-node->str (:set ir-node)) ")"))

(defmethod ir-node->str :cart-or-mul [ir-node]
  (apply->str "*" (:nums-or-sets ir-node)))

(defmethod ir-node->str :union [ir-node]
  (apply->str "\\/" (:sets ir-node)))

(defmethod ir-node->str :intersection [ir-node]
  (apply->str "/\\" (:sets ir-node)))

(defmethod ir-node->str :member [ir-node]
  (str (ir-node->str (:elem ir-node)) ":" (ir-node->str (:set ir-node))))

(defmethod ir-node->str :subset [ir-node]
  (chain->str "<:" (:sets ir-node)))

(defmethod ir-node->str :strict-subset [ir-node]
  (chain->str "<<:" (:sets ir-node)))

;; Relations

;; Functions

 ;; TODO: allow multiple args
(defmethod ir-node->str :fn-call [ir-node]
  (str (name (:f ir-node)) "(" (ir-node->str (first (:args ir-node))) ")"))

;; Substitutions

(defn id-vals->ids [id-vals]
  (map (comp ir-node->str first) (partition 2 id-vals)))

(defn id-vals->vals [id-vals]
  (map (comp ir-node->str second) (partition 2 id-vals)))

;; TODO: If more then 1 id is present non of them can be a :fn-call.
(defmethod ir-node->str :assignment [ir-node]
  (s/assert (s/keys :req-un [::id-vals]) ir-node)
  (let [id-vals (:id-vals ir-node)
        ids (id-vals->ids id-vals)
        vals (id-vals->vals id-vals)]
    (str (str/join "," ids) " := " (str/join "," vals))
    ))

;; Construct ProB Model

(defn action [name code] (EventBAction. name code #{}))

(defn event
  ([name] (Event. name Event$EventType/ORDINARY false))
  ([name actions] (.set (event name) Action (ModelElementList. actions)))
  ([name guards actions] (.set (event name actions) Guard (ModelElementList. guards))))

(defn guard [name code] (EventBGuard. name code false #{}))

(defn invariant [name code] (EventBInvariant. name code false #{}))

(defn variable [name] (EventBVariable. name nil))

(defn has-tag [tag] (fn [ir-node] (= tag (:tag ir-node))))

(defn extract-actions [ir-node]
  (case (:tag ir-node)
    :assignment [(action "act0" (ir-node->str ir-node))]
    :parallel-sub (map-indexed
                   (fn [i n] (action (str "act" i) (ir-node->str n)))
                   (:subs ir-node))
    :precondition (extract-actions (first (:subs ir-node)))
    []))

(defn extract-guards [ir-node]
  (case (:tag ir-node)
    :precondition [(guard "grd0" (ir-node->str (:pred ir-node)))]
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
       (map-indexed (fn [i n] (action (str "init" i) (ir-node->str n))))
       (event "INITIALISATION")))

(defn extract-invariants [clauses]
  (map-indexed
   (fn [i pred] (invariant (str "inv" i) (ir-node->str pred)))
   (find-first :invariants clauses)))

(defn extract-variables [clauses]
  (map (comp variable name) (find-first :variables clauses)))

(def modelCreator (.getProvider injector EventBModel))

(defn set-invariants [m invs]
  (.set m Invariant (ModelElementList. invs)))

(defn set-events [m events]
  (-> m
      (.set Event (ModelElementList. events))
      (.set BEvent (ModelElementList. events))))

(defn set-variables [m vars]
  (.set m Variable (ModelElementList. vars)))

(defn ir->eventb-machine [ir-node]
  (assert (= :machine (:tag ir-node)))
  (let [m-name (-> ir-node :name name (str/replace #"-" "_"))
        clauses (:machine-clauses ir-node)]
    (-> (EventBMachine. m-name)
        (set-invariants (extract-invariants clauses))
        (set-events (conj (extract-events clauses) (extract-init clauses)))
        (set-variables (extract-variables clauses)))))

(defn machine->model [m]
  (-> modelCreator .get (.addMachine m)))

(defn prob->rodin [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(comment
  (ir-node->str (util/b (strict-subset? #{1} #{1,2} #{1,2,3})))
  (ir-node->str (util/b (assign :x (+ :E 1) :y :W)))
  (ir-node->str (util/b (assign (fn-call :f :x) (+ :E 1) :y :W)))
  (ir-node->str (util/b (and (< :x 10) (> :y 0))))

  (extract-actions (util/b (assign :x (+ :E 1) :y :W)))
  (extract-actions (util/b (||
                            (assign :y :W)
                            (assign :x (+ :E 1) :y :W))))

  (extract-actions (util/b  (+ :E 1)))

  (extract-guards (util/b (pre (and (< :x 10) (> :y 0)) (assign :x :y))))

  (op->event (util/b
              (op :inc_dec []
                  (||
                   (assign :x (+ :x 1))
                   (assign :y (- :y 1))))))

  (.getGuards (op->event
               (util/b
                (op :inc_dec []
                    (pre (and (< :x 10) (> :y 0)) (||
                                                   (assign :x (+ :x 1))
                                                   (assign :y (- :y 1))))))))

  (def machine (util/b (machine :hello-world
                       (variables :x :hello)
                       (invariants
                        (in :hello bool-set)
                                 (<= :x 10))
                       (init
                           (assign :x 0)
                           (assign :hello true))
                       (operations
                        (:inc [] (pre (< :x 10) (assign :x (+ :x 1))))
                        (:hello [] (assign :hello true))))))

  (-> machine
      ir->eventb-machine
      machine->model
      (prob->rodin "hello" "./resources/eventb"))

  )
