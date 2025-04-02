(ns lisb.translation.eventb.ir2eventb
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]
            [lisb.translation.irtools :refer [CLAUSE TAG]])
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
    Variant)
   de.prob.animator.domainobjects.EventB
   de.prob.model.representation.ModelElementList))

(def label-postfix (atom ""))

(defn rodin-name [kw]
  (str/replace (name kw) #"-" "_"))

(defmulti ir-expr->str
  "converts expression into string"
  #(or (:tag %) (type %))
  :default nil)
(defmulti ir-pred->str
  "converts predicate into string"
  #(or (:tag %) (type %))
  :default nil)

#_(defmethod ir-expr->str nil [ir] (println "unsupported expr:" ir) (str ir))
#_(defmethod ir-pred->str nil [ir] (println "unsupported pred:" ir) (str ir))

(defn chain-expr [op irs]
  (str/join op (map (fn [ir]
                      (if (:tag ir)
                        (str "(" (ir-expr->str ir) ")")
                        (ir-expr->str ir))) irs)))

(defn chain-pred [op ir]
  (str "(" (str/join op (map ir-pred->str ir)) ")"))

;; Primitives

(defmethod ir-expr->str clojure.lang.Keyword [ir] (rodin-name ir))
(defmethod ir-expr->str java.lang.Long [ir] (str ir))
(defmethod ir-expr->str java.lang.Integer [ir] (str ir))

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
(defmethod ir-expr->str :pow [ir-expr] (chain-expr " ** " (:nums ir-expr)))

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
  (str "(!" (str/join "," (map rodin-name (:ids ir))) "." (ir-pred->str (:implication ir)) ")"))
(defmethod ir-pred->str :exists [ir]
  (str "(#" (str/join "," (map rodin-name (:ids ir))) "." (ir-pred->str (:pred ir)) ")"))

;; Equality

(defmethod ir-pred->str :equals [ir]
  (str (ir-expr->str (:left ir)) "=" (ir-expr->str (:right ir))))

(defmethod ir-pred->str :not-equals [ir]
  (str (ir-expr->str (:left ir)) "/=" (ir-expr->str (:right ir))))

;; Sets

(defmethod ir-expr->str clojure.lang.PersistentHashSet [ir]
  (str "{" (str/join "," (map ir-expr->str ir)) "}"))

(defmethod ir-expr->str :comprehension-set [ir]
  (if (:expr ir)
    (str "{"
         (ir-expr->str (:expr ir))
         "|"
         (ir-pred->str (:pred ir))
         "}")
    (str "{"
         (str/join "," (map rodin-name (:ids ir))) "|"
         (ir-pred->str (:pred ir)) "}")))

(defmethod ir-expr->str :power-set [ir]
  (str "POW(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :power1-set [ir]
  (str "POW1(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :card [ir]
  (str "card(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :unite-sets [ir]
  (str "union(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :intersect-sets [ir]
  (str "inter(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :cardinality [ir]
  (str "card(" (ir-expr->str (:set ir)) ")"))

(defmethod ir-expr->str :cartesian-product [ir]
  (chain-expr "**" (:sets ir)))

(defmethod ir-expr->str :union [ir]
  (chain-expr "\\/" (:sets ir)))

(defmethod ir-expr->str :union-pe [{:keys [ids pred expr]}]
  (str "(UNION " (str/join "," (map ir-expr->str ids)) "." (ir-pred->str pred) "|" (ir-expr->str expr) ")"))

(defmethod ir-expr->str :intersection [ir]
  (chain-expr "/\\" (:sets ir)))

(defmethod ir-expr->str :intersection-pe [{:keys [ids pred expr]}]
  (str "(INTER " (str/join "," (map ir-expr->str ids)) "." (ir-pred->str pred) "|" (ir-expr->str expr) ")"))

(defmethod ir-expr->str :difference [ir]
  (chain-expr "\\" (:sets ir)))

(defmethod ir-pred->str :member [ir]
  (str (ir-expr->str (:elem ir)) ":" (ir-expr->str (:set ir))))

(defn chain-expr-explicit [op elems]
  (str "(" (str/join "&" (map (fn [a b] (str (ir-expr->str a) op (ir-expr->str b)))
                              (butlast elems) (rest elems))) ")"))

(defmethod ir-pred->str :subset [ir]
  (chain-expr-explicit "<:" (:sets ir)))

(defmethod ir-pred->str :strict-subset [ir]
  (chain-expr-explicit "<<:" (:sets ir)))

(defmethod ir-pred->str :partition [ir]
  (str "partition(" (ir-expr->str (:set ir)) ","
       (str/join "," (map ir-expr->str (:partitions ir))) ")"))

(defmethod ir-pred->str :finite [ir]
  (str "finite(" (ir-expr->str (:set ir)) ")"))

;; Relations

(defmethod ir-expr->str lisb.translation.types.Tuple [tuple]
  (str "(" (chain-expr "|->" tuple) ")"))

(defmethod ir-expr->str :maplet [{:keys [elems]}]
  (chain-expr "|->" elems))

(defmethod ir-expr->str :relation [ir]
  (chain-expr "<->" (:sets ir)))

(defmethod ir-expr->str :dom [{:keys [rel]}]
  (str "dom(" (ir-expr->str rel) ")"))

(defmethod ir-expr->str :ran [{:keys [rel]}]
  (str "ran(" (ir-expr->str rel) ")"))

(defmethod ir-expr->str :total-relation [ir]
  (chain-expr "<<->" (:sets ir)))

(defmethod ir-expr->str :surjective-relation [ir]
  (chain-expr "<->>" (:sets ir)))

(defmethod ir-expr->str :total-surjective-relation [ir]
  (chain-expr "<<->>" (:sets ir)))

(defmethod ir-expr->str :composition [ir]
  (chain-expr ";" (:rels ir)))

;; TODO: backwards composition

(defmethod ir-expr->str :id [{:keys [rel]}]
  (str "id(" (ir-expr->str rel) ")"))

(defmethod ir-expr->str :inverse [{:keys [rel]}]
  (str (ir-expr->str rel) "~"))

(defmethod ir-expr->str :image [{:keys [rel set]}]
  (str (ir-expr->str rel) "[" (ir-expr->str set) "]"))

(defmethod ir-expr->str :domain-restriction [{:keys [rel set]}]
  (str (ir-expr->str set) "<|" (ir-expr->str rel)))

(defmethod ir-expr->str :domain-subtraction [{:keys [rel set]}]
  (str (ir-expr->str set) "<<|" (ir-expr->str rel)))

(defmethod ir-expr->str :range-restriction [{:keys [rel set]}]
  (str (ir-expr->str rel) "|>" (ir-expr->str set)))

(defmethod ir-expr->str :range-subtraction [{:keys [rel set]}]
  (str (ir-expr->str rel) "|>>" (ir-expr->str set)))

(defmethod ir-expr->str :override [{:keys [rels]}]
  (chain-expr "<+" rels))

(defmethod ir-expr->str :direct-product [{:keys [rels]}]
  (chain-expr "><" rels))

(defmethod ir-expr->str :parallel-product [{:keys [rels]}]
  (chain-expr "||" rels))

(defmethod ir-expr->str :prj1 [{:keys [set1 set2]}]
  (str "prj1(" (ir-expr->str set1) "," (ir-expr->str set2) ")"))

(defmethod ir-expr->str :prj2 [{:keys [set1 set2]}]
  (str "prj2(" (ir-expr->str set1) "," (ir-expr->str set2) ")"))

(defmethod ir-expr->str :iteration [{:keys [rel num]}]
  (str (ir-expr->str rel) "^" (ir-expr->str num)))

(defmethod ir-expr->str :extended-expr [{:keys [identifier exprs preds] :as xx}]
  (assert (empty? preds)) ;; do not know what happens with preds
  (str (name identifier) "(" (clojure.string/join "," (map ir-expr->str exprs)) ")"))

(defmethod ir-expr->str :extended-pred [{:keys [identifier exprs preds]}]
  (assert false) ;; do not know what happens here
  (str (name identifier) "(" (clojure.string/join "," (map ir-expr->str exprs)) ")"))

;; TODO: missing Closures


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
  (reduce (fn [acc cur] {:tag :maplet :elems [acc cur]})
          tuple))

(defmethod ir-expr->str :lambda [{:keys [ids pred expr]}]
  ;;TODO: In Event-B ids can be arbitrarily nested.
  (str "%" (ir-expr->str (tuple->maplet ids)) "." (ir-pred->str pred) "|" (ir-expr->str expr)))

(defmethod ir-expr->str :fn-call [ir]
  (str (ir-expr->str (:f ir))
       "("
       (if-let [args (seq (:args ir))]
         (ir-expr->str (tuple->maplet args))
         "")
       ")"))

;; Construct ProB Model

(defn id-vals->ids [id-vals]
  (map (comp ir-expr->str first) (partition 2 id-vals)))

(defn id-vals->vals [id-vals]
  (map (comp ir-expr->str second) (partition 2 id-vals)))

;; TODO: If more then 1 id is present no val can be a :fn-call.

(defmulti ir-sub->strs
  "converts the action into string"
  :tag)

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

(defn extract-invariants
  "converts all the invariants and assertions to EventBInvariant objects, where assertions are marked as theorems"
  [clauses]
  (let [invariant (map-indexed
                   (fn [i pred]
                     (EventBInvariant. (if (:label pred) (name (:label pred)) (str "inv" @label-postfix i))
                                       (ir-pred->str (if (= :theorem (:tag pred)) (:pred pred) pred))
                                       (= :theorem (:tag pred))
                                       #{}))
                   (:values (find-clause :invariants clauses)))
        theorems  (map-indexed
                   (fn [i pred] (EventBInvariant. (if (:label pred) (name (:label pred)) (str "thm" @label-postfix i))
                                                  (ir-pred->str pred)
                                                  true
                                                  #{}))
                   (:values (find-clause :assertions clauses)))]
    (ModelElementList. (concat invariant theorems))))



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


(defmulti ir->prob
  "converts IR into nodes of the ProB Model"
  :tag)

(defn clause->prob [tag clauses] (ir->prob (find-clause tag clauses)))

(defn extract-events
  "converts all events and the initialisation to ProB Event objects"
  [clauses]
  (let [events (or (clause->prob :events clauses) ())
        init   (find-clause :init clauses)]
    (ModelElementList. (if init
                         (cons (ir->prob init) events)
                         events))))

(defmethod ir->prob :init [{:keys [values]}]
  (assert (= (count values) 1)) ; more than one means sequential composition!
  (->> values
       (mapcat ir-sub->strs)
       (map-indexed (fn [i code] (EventBAction. (if (:label code) (name (:label code)) (str "init" @label-postfix i))
                                                code
                                                #{})))
       ModelElementList.
       (.withActions (new-event "INITIALISATION" :ordinary :none))))

(defmethod ir->prob :events [{:keys [values]}]
  (ModelElementList. (map ir->prob values)))

(defmethod ir->prob :args [{:keys [values]}]
  (ModelElementList. (map (fn [x] (EventParameter. (clojure.core/name x))) values)))

(defmethod ir->prob :guards [{:keys [values]}]
  (ModelElementList. (map-indexed (fn [i x] (EventBGuard. (if (:label x) (name (:label x)) (str "grd" @label-postfix i))
                                                          (ir-pred->str x)
                                                          false
                                                          #{}))
                                  values)))

(defmethod ir->prob :witnesses [{:keys [values]}]
  (ModelElementList. (map ir->prob values)))

(defmethod ir->prob :actions [{:keys [values]}]
  (ModelElementList. (map-indexed
                      (fn [i code] (EventBAction. (if (:label code) (name (:label code)) (str "act" @label-postfix i))
                                                  code
                                                  #{}))
                      (mapcat ir-sub->strs values))))

(defmethod ir->prob :event [{:keys [name clauses]}]
  (let [parent-event (find-clause :event-reference clauses)
        e (-> (new-event (rodin-name name)
                         (:value (find-clause :status clauses))
                         (:type parent-event))
              (.withParameters (clause->prob :args clauses))
              (.withGuards (clause->prob :guards clauses))
              (.withWitnesses (clause->prob :witnesses clauses))
              (.withActions (clause->prob :actions clauses)))]
    (if (:value parent-event)
      ;;TODO: get actual event
      (.withParentEvent e (new-event (rodin-name (:value parent-event)) :ordinary :none))
      e)))


(defmethod ir->prob :variables [{:keys [values]}]
  (ModelElementList. (map (fn [x] (EventBVariable. (rodin-name x) "")) values)))

(defmethod ir->prob :refinement [ir]
  (-> (ir->prob (assoc ir :tag :machine))
      ;;TODO: get real machine-clauses
      (.withRefinesMachine (EventBMachine. (rodin-name (:abstract-machine-name ir))))))

(defmethod ir->prob :sees [{:keys [values]}]
  ;;TODO: get real context
  (ModelElementList. (map (fn [x] (Context. (rodin-name x))) values)))

(defmethod ir->prob :variant [ir]
  (Variant. (ir-expr->str (:expr ir)) #{}))

(defmethod ir->prob :machine [{m-name :name clauses :machine-clauses}]
  (-> (EventBMachine. (rodin-name m-name))
      (.withSees (clause->prob :sees clauses))
      (.withInvariants (extract-invariants clauses))
      (.withVariant (clause->prob :variant clauses))
      (.withEvents (extract-events clauses))
      (.withVariables (clause->prob :variables clauses))))

(defn extract-sets
  "Gets all sets in the the sets clause"
  [ir]
  (ModelElementList.
   (map (fn [id]
          (de.prob.model.representation.Set. (EventB. (rodin-name id))))
        (s/select [(CLAUSE :sets) :values s/ALL :id] ir))))

(defn extract-constants
  "Gets constants from constants clause and enumerated sets"
  [ir]
  (ModelElementList. (map (fn [c] (EventBConstant. (rodin-name c) false ""))
                          (distinct (s/select (s/multi-path
                                               [(CLAUSE :constants) :values s/ALL]
                                               [(CLAUSE :sets) :values s/ALL (TAG :enumerated-set) :elems s/ALL]) ir)))))
(defn extract-axioms [ir]
  (ModelElementList. (map-indexed
                      (fn [i pred] (EventBAxiom. (if (:label pred) (name (:label pred)) (str "axm" @label-postfix i))
                                                 (ir-pred->str pred)
                                                 false
                                                 #{}))
                      (s/select [(CLAUSE :properties) :values s/ALL] ir))))

(defn extract-theorems [ir]
  (ModelElementList. (map-indexed
                      (fn [i pred] (EventBAxiom. (if (:label pred) (name (:label pred)) (str "thm" @label-postfix i))
                                                 (ir-pred->str pred)
                                                 true
                                                 #{}))
                      (s/select [(CLAUSE :theorems) :values s/ALL] ir))))

(defmethod ir->prob :extends [ir]
  ;;TODO: get real context
  (ModelElementList. (list (Context. (rodin-name (:name (first (:values ir))))))))  ;;there should be only one

(defmethod ir->prob :context [ir]
  (let [clauses (:machine-clauses ir)]
    (-> (Context. (rodin-name (:name ir)))
        (.withExtends (clause->prob :extends clauses))
        (.withSets (extract-sets ir))
        (.withConstants (extract-constants ir))
        (.withAxioms (ModelElementList.
                      (concat (extract-axioms ir)
                              (extract-theorems ir)))))))

(defmethod ir->prob nil [_] nil)

(defn ir->prob-with-label [ir new-label-postfix]
  (reset! label-postfix new-label-postfix)
  (let [result (ir->prob ir)]
    (reset! label-postfix "")
    result))
