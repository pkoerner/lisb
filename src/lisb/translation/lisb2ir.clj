(ns lisb.translation.lisb2ir
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:require [clojure.walk :refer [walk]])
  (:require [clojure.set])
  (:require [clojure.spec.alpha :as s]))


(def machine-clause-tags #{:uses :includes :sees :extends :promotes :constraints :sets :constants :properties
                           :definitions :variables :invariants :assertions :init :operations})
(def sub-tags #{:skip :block :assignment :becomes-element-of :becomes-such :op-call :parallel-sub :sequential-sub :any
                :let-sub :var :precondition :assert :choice :if-sub :cond :select :case})
(def seq-tags #{:empty-sequence :sequence :seq :seq1 :iseq :iseq1 :perm :concat :prepend :append :reverse :front
                :drop-last :conc :take :drop})
(def fn-tags #{:partial-fn :total-fn :partial-surjection :total-surjection :partial-injection :total-injection
               :partial-bijection :total-bijection :lambda
               ; relations fns
               :fnc
               })
(def rel-tags #{:relation :total-relation :surjective-realtion :total-surjective-relation :maplet :id
                :domain-restriction :domain-subtraction :range-restriction :range-subtraction :inverse :image :override
                :direct-product :composition :parallel-product :prj1 :prj2 :closure :closure1 :rel})
(def num-tags #{; numbers
                :min-int :max-int :max :min :add :sub :cartesian-product-or-multiplication :mul :div :pow :mod :product
                :sum :successor :predecessor
                ; set numbers
                :cardinality
                ; seq numbers
                :size
                })                                          ; numbers
(def set-tags #{:comprehension-set :power-set :power1-set :fin :fin1 :cartesian-product
                :cartesian-product-or-multiplication :union :intersection :difference :unite-sets :intersect-sets
                :union-pe :intersection-pe
                ; boolean-sets
                :bool-set
                ; number-sets
                :integer-set :natural-set :int-set :nat-set :nat1-set :interval
                ; string-sets
                :string-set
                ; rel-sets
                :dom :ran
                }) ; #{}
(def boolean-tags #{:pred->bool})                           ; true false
(def expr-tags (clojure.set/union seq-tags fn-tags rel-tags set-tags num-tags boolean-tags
                                  #{:let :if-expr
                                    ; records
                                    :struct :record :record-get
                                    ; seqs
                                    :first :last
                                    ; fns
                                    :fn-call
                                    }))
(def pred-tags #{; logical predicates
                 :and :or :implication :equivalence :not :for-all :exists
                 ; equality
                 :equals :not-equals
                 ; set predicates
                 :member :subset :strict-subset
                 ; number predicates
                 :greater :less :greater-equals :less-equals
                 })

(declare b=)
(declare band)
(declare bnot)
(declare bpredecessor)

(defn to-vec [v]
  (if (vector? v)
    v
    [v]))

(defn bif
  ([node cond then] (assoc node :cond cond :then then))
  ([node cond then else]
   (if else
     (assoc node :cond cond :then then :else else)
     (bif node cond then))))

#_(defn blet [node kvs]
  (let [kv-pairs (partition 2 kvs)
        identifiers (map first kv-pairs)
        assignment (reduce band (map (partial apply b=) kv-pairs))]
    (assoc node :identifiers identifiers :assignment assignment)))


;;; parse units

(defn process-machine-name [m machine-name]
  (if (seqable? machine-name)
    (let [[name & args] machine-name]
      (assoc m :name name :args args))
    (assoc m :name machine-name :args [])))
(s/fdef process-machine-name
        :args (s/cat :m ::m :machine-name ::machine-name)
        :ret (s/keys :req-un (::tag) :req (::machine-clauses))
        )             ; TODO: concretize spec

(defn bmachine [machine-name & machine-clauses]
  (process-machine-name
    {:tag :machine
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef bmachine
        :args (s/cat :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un (::tag) :req (::machine-clauses))
                    #(= :machine (:tag %))))             ; TODO: concretize spec

(defn bmodel [machine-name & machine-clauses]
  (process-machine-name
    {:tag :model
     :machine-clauses   machine-clauses}
    machine-name))
(s/fdef bmodel
        :args (s/cat :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un (::tag) :req (::machine-clauses))
                    #(= :model (:tag %))))             ; TODO: concretize spec

(defn bsystem [machine-name & machine-clauses]
  (process-machine-name
    {:tag :system
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef bsystem
        :args (s/cat :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un (::tag) :req (::machine-clauses))
                    #(= :system (:tag %))))             ; TODO: concretize spec

(defn brefinement [machine-name abstract-machine-name & machine-clauses]
  (process-machine-name
    {:tag :refinement
     :abstract-machine-name abstract-machine-name
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef brefinement
        :args (s/cat :machine-names ::machine-names :abstract-machine-name ::abstract-machine-name :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un (::tag) :req (::abstract-machine-name ::machine-clauses))
                    #(= :refinement (:tag %))))             ; TODO: concretize spec

(defn bimplementation [machine-name abstract-machine-name & machine-clauses]
  (process-machine-name
    {:tag :implementation
     :abstract-machine-name abstract-machine-name
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef bimplementation
        :args (s/cat :machine-names ::machine-names :abstract-machine-name ::abstract-machine-name :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un (::tag) :req (::abstract-machine-name ::machine-clauses))
                    #(= :implementation (:tag %))))             ; TODO: concretize spec


;;; machine clauses
;; machine inclusions

(defn process-machine-reference [machine-reference]
  (process-machine-name {:tag :machine-reference} machine-reference))
(s/fdef process-machine-reference
        :args (s/cat :machine-references ::machine-references)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :machine-reference (:tag %))))              ; TODO: concretize spec

(defn buses [& machine-names]
  {:tag :uses
   :values machine-names})
(s/fdef buses
        :args (s/cat :machine-names ::machine-names)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :uses (:tag %))))

(defn bincludes [& machine-references]
  {:tag :includes
   :values (map process-machine-reference machine-references)})
(s/fdef bincludes
        :args (s/cat :machine-references ::machine-references)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :includes (:tag %))))

(defn bsees [& machine-names]
  {:tag :sees
   :values machine-names})
(s/fdef bsees
        :args (s/cat :machine-names ::machine-names)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :sees (:tag %))))

(defn bextends [& machine-references]
  {:tag    :extends
   :values (map process-machine-reference machine-references)})
(s/fdef bextends
        :args (s/cat :machine-references ::machine-references)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :extends (:tag %))))

(defn bpromotes [& ops]
  {:tag :promotes
   :values ops})
(s/fdef bpromotes
        :args (s/cat :ops ::ops)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :promotes (:tag %))))

;; machine sections

(defn bconstraints [& preds]
  {:tag :contraints
   :values preds})
(s/fdef bconstraints
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :contraints (:tag %))))

(defn bsets [& set-defs]
  {:tag :sets
   :values set-defs})
(s/fdef bsets
        :args (s/cat :set-defs ::set-defs)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :sets (:tag %))))
(defn bdeferred-set [id]
  {:tag :deferred-set
   :id id})
(s/fdef bdeferred-set
        :args (s/cat :id ::id)
        :ret (s/and (s/keys :req-un (::tag) :req (::id))
                    #(= :deferred-set (:tag %))))
(defn benumerated-set [id & elems]
  {:tag :enumerated-set
   :id id
   :elems elems})
(s/fdef benumerated-set
        :args (s/cat :id ::id :elems ::elems)
        :ret (s/and (s/keys :req-un (::tag) :req (::id ::elems))
                    #(= :enumerated-set (:tag %))))

(defn bconstants [& ids]
  {:tag :constants
   :values ids})
(s/fdef bconstants
        :args (s/cat :ids ::ids)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :constants (:tag %))))

(defn bproperties [& preds]
  {:tag :properties
   :values preds})
(s/fdef bproperties
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :properties (:tag %))))

(defn bdefinitions [& defs]
  {:tag :definitions
   :values defs})
(s/fdef bdefinitions
        :args (s/cat :defs ::defs)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :definitions (:tag %))))

(defn bvariables [& ids]
  {:tag :variables
   :values ids})
(s/fdef bvariables
        :args (s/cat :ids ::ids)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :variables (:tag %))))

(defn binvariants [& preds]
  {:tag :invariants
   :values preds})
(s/fdef binvariants
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :invariants (:tag %))))

(defn bassertions [& preds]
  {:tag :assertions
   :values preds})
(s/fdef bassertions
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :assertions (:tag %))))

(defn binit [& subs]
  {:tag :init
   :values subs})
(s/fdef binit
        :args (s/cat :subs ::subs)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :init (:tag %))))

(defn boperations [& op-defs]
  {:tag :operations
   :values op-defs})
(s/fdef boperations
        :args (s/cat :op-defs ::op-defs)
        :ret (s/and (s/keys :req-un (::tag) :req (::values))
                    #(= :operations (:tag %))))
(defn bop
  ([returns name args body]
   {:tag         :op
    :returns returns
    :name        name
    :args        args
    :body        body})
  ([name args body]
   (bop [] name args body)))
(s/fdef bop
        :args (s/cat :returns (s/? ::returns) :name ::name :args ::args :body ::body)
        :ret (s/and (s/keys :req-un (::tag) :req (::name ::args ::body) :opt (::returns))
                    #(= :op (:tag %))))


;;; substitutions

(def bskip {:tag :skip})
(s/fdef bskip
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :skip (:tag %))))

(defn bblock [sub]
  {:tag :block
   :sub sub})
(s/fdef bblock
        :args (s/cat :sub ::sub)
        :ret (s/and (s/keys :req-un (::tag) :req (::sub))
                    #(= :block (:tag %))))

(defn bassign [& id-vals]
  {:tag :assignment
   :id-vals id-vals})
(s/fdef bassign
        :args (s/cat :id-vals ::id-vals)
        :ret (s/and (s/keys :req-un (::tag) :req (::id-vals))
                    #(= :assignment (:tag %))))

(defn bbecomes-element-of [ids set]
  {:tag :becomes-element-of
   :ids ids
   :set set})
(s/fdef bbecomes-element-of
        :args (s/cat :ids ::ids :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::set))
                    #(= :becomes-element-of (:tag %))))

(defn bbecomes-such [ids pred]
  {:tag :becomes-such
   :ids ids
   :pred pred})
(s/fdef bbecomes-such
        :args (s/cat :ids ::ids :pred ::pred)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred))
                    #(= :becomes-such (:tag %))))

(defn bop-call-with-returns [returns op & args]
  {:tag     :op-call
   :returns returns
   :op      op
   :args    args})
(s/fdef bop-call-with-returns
        :args (s/cat :returns ::returns :op ::op :args ::args)
        :ret (s/and (s/keys :req-un (::tag) :req (::returns ::op ::args))
                    #(= :op-call (:tag %))))

(defn bop-call [op & args]
  (apply (partial bop-call-with-returns [] op) args))
(s/fdef bop-call-with-returns
        :args (s/cat :op ::op :args ::args)
        :ret (s/and (s/keys :req-un (::tag) :req (::returns ::op ::args))
                    #(= :op-call (:tag %))))               ; TODO: concretize spec

(defn bparallel-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :parallel-sub
     :subs subs}))
(s/fdef bparallel-sub
        :args (s/cat :subs ::subs)
        :ret (s/or :parallel-sub (s/and (s/keys :req-un (::tag) :req (::subs))
                                          #(= :parallel-sub (:tag %)))
                   :sub ::sub))

(defn bsequential-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :sequential-sub
     :subs subs}))
(s/fdef bsequential-sub
        :args (s/cat :subs ::subs)
        :ret (s/or :sequential-sub (s/and (s/keys :req-un (::tag) :req (::subs))
                                          #(= :sequential-sub (:tag %)))
                   :sub ::sub))

(defn bany [ids pred & subs]
  {:tag :any
   :ids ids
   :pred pred
   :subs subs})
(s/fdef bany
        :args (s/cat :ids ::ids :pred ::pred :subs ::subs)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred ::subs))
                    #(= :any (:tag %))))

(defn blet-sub [id-vals & subs]
  {:tag :let-sub
   :id-vals id-vals
   :subs subs})
(s/fdef blet-sub
        :args (s/cat :id-vals ::id-vals :subs ::subs)
        :ret (s/and (s/keys :req-un (::tag) :req (::id-vals ::subs))
                    #(= :let-sub (:tag %))))

(defn bvar [ids & subs]
  {:tag :var
   :ids ids
   :subs subs})
(s/fdef bvar
        :args (s/cat :ids ::ids :subs ::subs)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::subs))
                    #(= :var (:tag %))))

(defn bprecondition [pred & subs]
  {:tag :precondition
   :pred pred
   :subs subs})
(s/fdef bprecondition
        :args (s/cat :pred ::pred :subs ::subs)
        :ret (s/and (s/keys :req-un (::tag) :req (::pred ::subs))
                    #(= :precondition (:tag %))))

(defn bassert [pred & subs]
  {:tag :assert
   :pred pred
   :subs subs})
(s/fdef bassert
        :args (s/cat :pred ::pred :subs ::subs)
        :ret (s/and (s/keys :req-un (::tag) :req (::pred ::subs))
                    #(= :assert (:tag %))))

(defn bchoice [& subs]
  {:tag :choice
   :subs subs})
(s/fdef bchoice
        :args (s/cat :subs ::subs)
        :ret (s/and (s/keys :req-un (::tag) :req (::subs))
                    #(= :choice (:tag %))))

(defn bif-sub
  ([cond then] (bif {:tag :if-sub} cond then))
  ([cond then else] (bif {:tag :if-sub} cond then else)))
(s/fdef bif-sub
        :args (s/cat :cond ::cond :then ::then :else(s/? ::else))
        :ret (s/and (s/keys :req-un (::tag) :req (::cond ::then) :opt (::else))
                    #(= :if-sub (:tag %))))
(defn bcond [& clauses]
    {:tag :cond
     :clauses clauses})
(s/fdef bcond
        :args (s/cat :clauses ::clauses)
        :ret (s/and (s/keys :req-un (::tag) :req (::clauses))
                    #(= :cond (:tag %))))

(defn bselect [& clauses]
  {:tag :select
   :clauses clauses})
(s/fdef bselect
        :args (s/cat :clauses ::clauses)
        :ret (s/and (s/keys :req-un (::tag) :req (::clauses))
                    #(= :select (:tag %))))

(defn bcase [expr & cases]
  {:tag :case
   :expr expr
   :cases cases})
(s/fdef bcase
        :args (s/cat :expr ::expr :cases ::cases)
        :ret (s/and (s/keys :req-un (::tag) :req (::expr ::cases))
                    #(= :case (:tag %))))


;;; if

(defn bif-expr
  ([cond then else] (bif {:tag :if-expr} cond then else))) ; else is always present
(s/fdef bif-expr
        :args (s/cat :cond ::cond :then ::then :else ::else)
        :ret (s/and (s/keys :req-un (::tag) :req (::cond ::then ::else))
                    #(= :if-expr (:tag %))))


;;; let

(defn blet [id-vals expr-or-pred]
  {:tag :let
   :id-vals id-vals
   :expr-or-pred expr-or-pred})
(s/fdef blet
        :args (s/cat :id-vals ::id-vals :expr-or-pred ::expr-or-pred)
        :ret (s/and (s/keys :req-un (::tag) :req (::id-vals ::expr-or-pred))
                    #(= :let (:tag %))))

;;; trees


;;; reals - (alpha - besser nicht verwenden)


;;; strings

(def bstring-set
  {:tag :string-set})
(s/fdef bstring-set
        :args (s/cat )
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :string-set (:tag %))))


;;; records

(defn bstruct [& id-types]
  {:tag :struct
   :id-types id-types})
(s/fdef bstruct
        :args (s/cat :id-types ::id-types)
        :ret (s/and (s/keys :req-un (::tag) :req (::id-types))
                    #(= :struct (:tag %))))

(defn brecord [& id-vals]
  {:tag :record
   :id-vals id-vals})
(s/fdef brecord
        :args (s/cat :id-vals ::id-vals)
        :ret (s/and (s/keys :req-un (::tag) :req (::id-vals))
                    #(= :record (:tag %))))

(defn brecord-get [rec id]
  {:tag :record-get
   :rec rec
   :id id})
(s/fdef brecord-get
        :args (s/cat :rec ::rec :id ::id)
        :ret (s/and (s/keys :req-un (::tag) :req (::rec ::id))
                    #(= :record-get (:tag %))))


;;; sequences

(defn bsequence
  ([] {:tag :empty-sequence})
  ([& elems]
   {:tag    :sequence
    :elems elems}))
(s/fdef bsequence
        :args (s/or :empty-sequence (s/cat) :sequence (s/cat :elems ::elems))
        :ret (s/and (s/keys :req-un (::tag) :opt (::elems))
                    #(contains? #{:empty-sequence :sequence} (:tag %))))

(defn bseq [set]
  {:tag :seq
   :set set})
(s/fdef bseq
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :seq (:tag %))))

(defn bseq1 [set]
  {:tag :seq1
   :set set})
(s/fdef bseq1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :seq1 (:tag %))))

(defn biseq [set]
  {:tag :iseq
   :set set})
(s/fdef biseq
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :iseq (:tag %))))

(defn biseq1 [set]
  {:tag :iseq1
   :set set})
(s/fdef biseq1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :iseq1 (:tag %))))

(defn bperm [set]
  {:tag :perm
   :set set})
(s/fdef bperm
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :perm (:tag %))))

(defn bsize [seq]
  {:tag :size
   :seq seq})
(s/fdef bsize
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq))
                    #(= :size (:tag %))))

(defn bconcat [& seqs]
  {:tag :concat
   :seqs seqs})
(s/fdef bconcat
        :args (s/cat :seqs ::seqs)
        :ret (s/and (s/keys :req-un (::tag) :req (::seqs))
                    #(= :concat (:tag %))))

(defn bprepend [elem seq]
  {:tag :prepend
   :elem elem
   :seq seq})
(s/fdef bprepend
        :args (s/cat :elem ::elem :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq ::elem))
                    #(= :prepend (:tag %))))

(defn bappend [seq & elems]
  {:tag :append
   :seq seq
   :elems elems})
(s/fdef bappend
        :args (s/cat :seq ::seq :elems ::elems)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq ::elems))
                    #(= :append (:tag %))))

(defn breverse [seq]
  {:tag :reverse
   :seq seq})
(s/fdef breverse
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq))
                    #(= :reverse (:tag %))))

(defn bfirst [seq]
  {:tag :first
   :seq seq})
(s/fdef bfirst
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq))
                    #(= :first (:tag %))))

(defn blast [seq]
  {:tag :last
   :seq seq})
(s/fdef blast
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq))
                    #(= :last (:tag %))))

(defn bfront [seq]
  {:tag :front
   :seq seq})
(s/fdef bfront
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq))
                    #(= :front (:tag %))))

(defn btail [seq]
  {:tag :tail
   :seq seq})
(s/fdef btail
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq))
                    #(= :tail (:tag %))))

(defn bconc [seq-of-seqs]
  {:tag :conc
   :seq-of-seqs seq-of-seqs})
(s/fdef bconc
        :args (s/cat :seq-of-seqs ::seq-of-seqs)
        :ret (s/and (s/keys :req-un (::tag) :req (::seq-of-seqs))
                    #(= :conc (:tag %))))

(defn btake [num seq]
  {:tag :take
   :seq seq
   :num num})
(s/fdef btake
        :args (s/cat :num ::num :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::num ::seq))
                    #(= :take (:tag %))))

(defn bdrop [num seq]
  {:tag :drop
   :seq seq
   :num num})
(s/fdef bdrop
        :args (s/cat :num ::num :seq ::seq)
        :ret (s/and (s/keys :req-un (::tag) :req (::num ::seq))
                    #(= :drop (:tag %))))


;;; functions

(defn bpartial-function [& sets]
  {:tag :partial-fn
   :sets sets})
(s/fdef bpartial-function
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :partial-fn (:tag %))))

(defn btotal-function [& sets]
  {:tag :total-fn
   :sets sets})
(s/fdef btotal-function
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :total-fn (:tag %))))

(defn bpartial-surjection [& sets]
  {:tag :partial-surjection
   :sets sets})
(s/fdef bpartial-surjection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :partial-surjection (:tag %))))

(defn btotal-surjection [& sets]
  {:tag :total-surjection
   :sets sets})
(s/fdef btotal-surjection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :total-surjection (:tag %))))

(defn bpartial-injection [& sets]
  {:tag :partial-injection
   :sets sets})
(s/fdef bpartial-injection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :partial-injection (:tag %))))

(defn btotal-injection [& sets]
  {:tag :total-injection
   :sets sets})
(s/fdef btotal-injection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :total-injection (:tag %))))

(defn bpartial-bijection [& sets]
  {:tag :partial-bijection
   :sets sets})
(s/fdef bpartial-bijection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :partial-bijection (:tag %))))

(defn btotal-bijection [& sets]
  {:tag :total-bijection
   :sets sets})
(s/fdef btotal-bijection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :total-bijection (:tag %))))

(defn blambda [ids pred expr]
  {:tag :lambda
   :ids ids
   :pred pred
   :expr expr})
(s/fdef blambda
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred ::expr))
                    #(= :lambda (:tag %))))
#_(defn bfn [id-types expr]
  {:tag :fn
   :id-types id-types
   :expr expr})

(defn bfn-call [f & args]
  {:tag :fn-call
   :f f
   :args args})
(s/fdef bfn-call
        :args (s/cat :f ::f :args ::args)
        :ret (s/and (s/keys :req-un (::tag) :req (::f ::args))
                    #(= :fn-call (:tag %))))


;;; relations

(defn brelation [& sets]
  {:tag :relation
   :sets sets})
(s/fdef brelation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :relation (:tag %))))

(defn btotal-relation [& sets]
  {:tag :total-relation
   :sets sets})
(s/fdef btotal-relation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :total-relation (:tag %))))

(defn bsurjective-relation [& sets]
  {:tag :surjective-realtion
   :sets sets})
(s/fdef bsurjective-relation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :surjective-realtion (:tag %))))

(defn btotal-surjective-relation [& sets]
  {:tag :total-surjective-relation
   :sets sets})
(s/fdef btotal-surjective-relation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :total-surjective-relation (:tag %))))

(defn bmaplet [left right]
  {:tag :maplet
   :left left
   :right right})
(s/fdef bmaplet
        :args (s/cat :left ::left :right ::right)
        :ret (s/and (s/keys :req-un (::tag) :req (::left ::right))
                    #(= :maplet (:tag %))))

(defn bdom [rel]
  {:tag :dom
   :rel rel})
(s/fdef bdom
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel))
                    #(= :dom (:tag %))))

(defn bran [rel]
  {:tag :ran
   :rel rel})
(s/fdef bran
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel))
                    #(= :ran (:tag %))))

(defn bid [set]
  {:tag :id
   :set set})
(s/fdef bid
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :id (:tag %))))

(defn bdomain-restriction [set rel]
  {:tag :domain-restriction
   :set set
   :rel rel})
(s/fdef bdomain-restriction
        :args (s/cat :set ::set :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel ::set))
                    #(= :domain-restriction (:tag %))))

(defn bdomain-subtraction [set rel]
  {:tag :domain-subtraction
   :set set
   :rel rel})
(s/fdef bdomain-subtraction
        :args (s/cat :set ::set :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel ::set))
                    #(= :domain-subtraction (:tag %))))

(defn brange-restriction [rel set]
  {:tag :range-restriction
   :rel rel
   :set set})
(s/fdef brange-restriction
        :args (s/cat :rel ::rel :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel ::set))
                    #(= :range-restriction (:tag %))))

(defn brange-subtraction [rel set]
  {:tag :range-subtraction
   :rel rel
   :set set})
(s/fdef brange-subtraction
        :args (s/cat :rel ::rel :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel ::set))
                    #(= :range-subtraction (:tag %))))

(defn binverse [rel]
  {:tag :inverse
   :rel rel})
(s/fdef binverse
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel))
                    #(= :inverse (:tag %))))

(defn bimage [rel set]
  {:tag :image
   :rel rel
   :set set})
(s/fdef bimage
        :args (s/cat :rel ::rel :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel ::set))
                    #(= :image (:tag %))))

(defn boverride [& rels]
  {:tag :override
   :rels rels})
(s/fdef boverride
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un (::tag) :req (::rels))
                    #(= :override (:tag %))))

(defn bdirect-product [& rels]
  {:tag :direct-product
   :rels rels})
(s/fdef bdirect-product
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un (::tag) :req (::rels))
                    #(= :direct-product (:tag %))))

(defn bcomposition [& rels]
  {:tag :composition
   :rels rels})
(s/fdef bcomposition
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un (::tag) :req (::rels))
                    #(= :composition (:tag %))))

(defn bparallel-product [& rels]
  {:tag :parallel-product
   :rels rels})
(s/fdef bparallel-product
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un (::tag) :req (::rels))
                    #(= :parallel-product (:tag %))))

(defn bprj1 [set1 set2]
  {:tag :prj1
   :set1 set1
   :set2 set2})
(s/fdef bprj1
        :args (s/cat :set1 ::set1 :set2 ::set2)
        :ret (s/and (s/keys :req-un (::tag) :req (::set1 ::set2))
                    #(= :prj1 (:tag %))))

(defn bprj2 [set1 set2]
  {:tag :prj2
   :set1 set1
   :set2 set2})
(s/fdef bprj2
        :args (s/cat :set1 ::set1 :set2 ::set2)
        :ret (s/and (s/keys :req-un (::tag) :req (::set1 ::set2))
                    #(= :prj2 (:tag %))))

(defn bclosure1 [rel]
  {:tag :closure1
   :rel rel})
(s/fdef bclosure1
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel))
                    #(= :closure1 (:tag %))))

(defn bclosure [rel]
  {:tag :closure
   :rel rel})
(s/fdef bclosure
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel))
                    #(= :closure (:tag %))))

(defn biterate [rel num]
  {:tag :iterate
   :rel rel
   :num num})
(s/fdef biterate
        :args (s/cat :rel ::rel :num ::num)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel ::num))
                    #(= :iterate (:tag %))))

(defn bfnc [rel]
  {:tag :fnc
   :rel rel})
(s/fdef bfnc
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel))
                    #(= :fnc (:tag %))))

(defn brel [rel]
  {:tag :rel
   :rel rel})
(s/fdef brel
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un (::tag) :req (::rel))
                    #(= :rel (:tag %))))


;;; numbers

(def binteger-set {:tag :integer-set})
(s/fdef binteger-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :integer-set (:tag %))))

(def bnatural-set {:tag :natural-set})
(s/fdef bnatural-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :natural-set (:tag %))))

(def bnatural1-set {:tag :natural1-set})
(s/fdef bnatural1-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :natural1-set (:tag %))))

(def bint-set {:tag :int-set})
(s/fdef bint-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :int-set (:tag %))))

(def bnat-set {:tag :nat-set})
(s/fdef bnat-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :nat-set (:tag %))))

(def bnat1-set {:tag :nat1-set})
(s/fdef bnat1-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :nat1-set (:tag %))))

(defn binterval [from to]
  {:tag :interval
   :from from
   :to to})
(s/fdef binterval
        :args (s/cat :from ::from :to ::to)
        :ret (s/and (s/keys :req-un (::tag) :req (::from ::to))
                    #(= :interval (:tag %))))

(defn brange [from to]
  (binterval from (bpredecessor to)))
(s/fdef binterval
        :args (s/cat :from ::from :to ::to)
        :ret (s/and (s/keys :req-un (::tag) :req (::from ::to))
                    #(= :interval (:tag %))))  ; TODO: concretize spec

(def bmin-int
  {:tag :min-int})
(s/fdef bmin-int
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :min-int (:tag %))))

(def bmax-int
  {:tag :max-int})
(s/fdef bmax-int
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :max-int (:tag %))))

(defn b> [& nums]
  {:tag :greater
   :nums nums})
(s/fdef b>
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :greater (:tag %))))

(defn b< [& nums]
  {:tag :less
   :nums nums})
(s/fdef b<
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :less (:tag %))))

(defn b>= [& nums]
  {:tag :greater-equals
   :nums nums})
(s/fdef b>=
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :greater-equals (:tag %))))

(defn b<= [& nums]
  {:tag :less-equals
   :nums nums})
(s/fdef b<=
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :less-equals (:tag %))))

(defn bmax
  ([set]
   {:tag :max, :set set})
  ; adds enumeration to max
  ([num & nums]
   (let [nums (into #{num} nums)]
     (bmax nums))))
(s/fdef bmax
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :max (:tag %))))

(defn bmin
  ([set]
   {:tag :min, :set set})
  ; adds enumeration to min
  ([num & nums]
   (let [nums (into #{num} nums)]
     (bmin nums))))
(s/fdef bmin
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :min (:tag %))))

(defn b+ [& nums]
  {:tag :add
   :nums nums})
(s/fdef b+
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :add (:tag %))))

(defn b- [& nums]
  (if (= 1 (count nums))
    {:tag :unary-minus, :num (first nums)}
    {:tag :sub, :nums nums}))
(s/fdef b-
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :sub (:tag %))))

(defn bcart-or-mult [& nums-or-sets]
  {:tag :cartesian-product-or-multiplication
   :nums-or-sets nums-or-sets})
(s/fdef bcart-or-mult
        :args (s/cat :nums-or-sets ::nums-or-sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums-or-sets))
                    #(= :cartesian-product-or-multiplication (:tag %))))

(defn b* [& nums]
  {:tag :mul
   :nums nums})
(s/fdef b*
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :mul (:tag %))))

(defn bdiv [& nums]
  {:tag :div
   :nums nums})
(s/fdef bdiv
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :div (:tag %))))

(defn b** [& nums]
  {:tag :pow
   :nums nums})
(s/fdef b**
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :pow (:tag %))))

(defn bmod [& nums]
  {:tag :mod
   :nums nums})
(s/fdef bmod
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un (::tag) :req (::nums))
                    #(= :mod (:tag %))))

(defn bpi [ids pred expr]
  {:tag :pi
   :ids ids
   :pred pred
   :expr expr})
(s/fdef bpi
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred ::expr))
                    #(= :pi (:tag %))))

(defn bsigma [ids pred expr]
  {:tag :sigma
   :ids ids
   :pred pred
   :expr expr})
(s/fdef bsigma
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred ::expr))
                    #(= :sigma (:tag %))))

(defn bsuccessor [num]
  {:tag :successor
   :num num})
(s/fdef bsuccessor
        :args (s/cat :num ::num)
        :ret (s/and (s/keys :req-un (::tag) :req (::num))
                    #(= :successor (:tag %))))

(defn bpredecessor [num]
  {:tag :predecessor
   :num num})
(s/fdef bpredecessor
        :args (s/cat :num ::num)
        :ret (s/and (s/keys :req-un (::tag) :req (::num))
                    #(= :predecessor (:tag %))))


;;; sets

(defn bcomprehension-set [ids pred]
  {:tag  :comprehension-set
   :ids  (to-vec ids)
   :pred pred})
(s/fdef bcomprehension-set
        :args (s/cat :ids ::ids :pred ::pred)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred))
                    #(= :comprehension-set (:tag %))))

(defn bpow [set]
  {:tag :power-set
   :set set})
(s/fdef bpow
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :power-set (:tag %))))

(defn bpow1 [set]
  {:tag :power1-set
   :set set})
(s/fdef bpow1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :power1-set (:tag %))))

(defn bfin [set]
  {:tag :fin
   :set set})
(s/fdef bfin
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :fin (:tag %))))

(defn bfin1 [set]
  {:tag :fin1
   :set set})
(s/fdef bfin1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :fin1 (:tag %))))

(defn bcard [set]
  {:tag :cardinality
   :set set})
(s/fdef bcard
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::set))
                    #(= :cardinality (:tag %))))

(defn bcartesian-product [& sets]
  {:tag :cartesian-product
   :sets sets})
(s/fdef bcartesian-product
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :cartesian-product (:tag %))))

(defn bunion [& sets]
  {:tag :union
   :sets sets})
(s/fdef bunion
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :union (:tag %))))

(defn bintersection [& sets]
  {:tag :intersection
   :sets sets})
(s/fdef bintersection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :intersection (:tag %))))

(defn bset- [& sets]
  {:tag :difference
   :sets sets})
(s/fdef bset-
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :difference (:tag %))))

(defn bmember? [elem set]
  {:tag :member
   :elem elem
   :set set})
(s/fdef bmember?
        :args (s/cat :elem ::elem :set ::set)
        :ret (s/and (s/keys :req-un (::tag) :req (::elem ::set))
                    #(= :member (:tag %))))

(defn bcontains? [set & elems]
  (apply band (reduce
                (fn [res elem]
                  (conj res (bmember? elem set)))
                []
                elems)))
(s/fdef bcontains?
        :args (s/cat :set ::set  :elem ::elem )
        :ret (s/and (s/keys :req-un (::tag) :req (::preds))
                    #(= :and (:tag %))))  ; TODO: concretize spec

(defn bsubset? [& sets]
  {:tag :subset
   :sets sets})
(s/fdef bsubset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :subset (:tag %))))

(defn bstrict-subset? [& sets]
  {:tag :strict-subset
   :sets sets})
(s/fdef bstrict-subset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :strict-subset (:tag %))))

; adds superset to lisb
(defn bsuperset? [& sets]
  (apply bsubset? (reverse sets)))
(s/fdef bsuperset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :subset (:tag %))))  ; TODO: concretize spec

; adds superset-strict to lisb
(defn bstrict-superset? [& sets]
  (apply bstrict-subset? (reverse sets)))
(s/fdef bstrict-superset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::sets))
                    #(= :strict-subset (:tag %))))  ; TODO: concretize spec

(defn bunite-sets [set-of-sets]
  {:tag :unite-sets
   :set-of-sets set-of-sets})
(s/fdef bunite-sets
        :args (s/cat :set-of-sets ::set-of-sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::set-of-sets))
                    #(= :unite-sets (:tag %))))

(defn bintersect-sets [set-of-sets]
  {:tag :intersect-sets
   :set-of-sets set-of-sets})
(s/fdef bintersect-sets
        :args (s/cat :set-of-sets ::set-of-sets)
        :ret (s/and (s/keys :req-un (::tag) :req (::set-of-sets))
                    #(= :intersect-sets (:tag %))))

(defn bunion-pe [ids pred expr]
  {:tag :union-pe
   :ids ids
   :pred pred
   :expr expr})
(s/fdef bunion-pe
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred ::expr))
                    #(= :union-pe (:tag %))))

(defn bintersection-pe [ids pred expr]
  {:tag :intersection-pe
   :ids ids
   :pred pred
   :expr expr})
(s/fdef bintersection-pe
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred ::expr))
                    #(= :intersection-pe (:tag %))))


;;; booleans

(def bbool-set {:tag :bool-set})
(s/fdef bbool-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un (::tag))
                    #(= :bool-set (:tag %))))

(defn bpred->bool [pred]
  {:tag :pred->bool
   :pred pred})
(s/fdef bpred->bool
        :args (s/cat :pred ::pred)
        :ret (s/and (s/keys :req-un (::tag) :req (::pred))
                    #(= :pred->bool (:tag %))))


;;; equality predicates

(defn b= [left right]
  {:tag :equals
   :left left
   :right right})
(s/fdef b=
        :args (s/cat :left ::left :right ::right)
        :ret (s/and (s/keys :req-un (::tag) :req (::left ::right))
                    #(= :equals (:tag %))))

(defn bnot= [left right]
  {:tag :not-equals
   :left left
   :right right})
(s/fdef bnot=
        :args (s/cat :left ::left :right ::right)
        :ret (s/and (s/keys :req-un (::tag) :req (::left ::right))
                    #(= :not-equals (:tag %))))
; syntactic sugar
(defn bdistinct? [& elems]
  (apply band (map (fn [[elem1 elem2]] (bnot= elem1 elem2)) (combinations elems 2))))
(s/fdef bdistinct?
        :args (s/cat :elems ::elems)
        :ret (s/and (s/keys :req-un (::tag) :req (::preds))
                    #(= :and (:tag %))))                    ; TODO: concretize spec


;;; logical predicates

(defn band [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :and
     :preds preds}))
(s/fdef band
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::preds))
                    #(= :and (:tag %))))

(defn bor [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :or
     :preds preds}))
(s/fdef bor
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::preds))
                    #(= :or (:tag %))))

(defn bimplication [& preds]
  {:tag :implication
   :preds preds})
(s/fdef bimplication
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::preds))
                    #(= :implication (:tag %))))

(defn bequivalence [& preds]
  {:tag :equivalence
   :preds preds})
(s/fdef bequivalence
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un (::tag) :req (::preds))
                    #(= :equivalence (:tag %))))

(defn bnot [pred]
  {:tag :not
   :pred pred})
(s/fdef bnot
        :args (s/cat :pred ::pred)
        :ret (s/and (s/keys :req-un (::tag) :req (::pred))
                    #(= :not (:tag %))))

(defn bfor-all
  ([ids implication]
   {:tag :for-all
    :ids ids
    :implication implication})
  ([ids premise conclusion]
   (bfor-all ids (bimplication premise conclusion))))
(s/fdef bfor-all
        :args (s/or :arity-2 (s/cat :ids ::ids :implication ::implication)
                    :arity-3 (s/cat :ids ::ids :premise ::pred :conclusion ::pred))
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::implication))
                    #(= :for-all (:tag %))))

(defn bexists [ids pred]
  {:tag :exists
   :ids ids
   :pred pred})
(s/fdef bexists
        :args (s/cat :ids ::ids :pred ::pred)
        :ret (s/and (s/keys :req-un (::tag) :req (::ids ::pred))
                    #(= :exists (:tag %))))

;;; misc


(defn bset-enum [& elements]
  {:tag :set-enum
   :elements elements})

(defn bmap-set [p s]
  (bran (blambda [:x] (bmember? :x s) (p :x))))


(declare pre-process-lisb)

(defn process-comprehension-set [lisb]
  (if (= 3 (count lisb))
    (let [lisb (disj lisb '|)
          [ids pred]
          (let [f (first lisb)
                s (second lisb)]
            (cond
              (or (vector? f) (keyword? f)) [f s]
              (or (vector? s) (keyword? s)) [s f]
              :else (throw (Exception. (str "Unsupported way of set comprehension: " lisb)))))]
      (conj (list ids pred) 'comprehension-set))
    (throw (Exception. (str "Unsupported way of set comprehension: " lisb)))))

(defn process-set-definitions [sets-clause]
  (loop [todo-set-defs (rest sets-clause)
         finished-set-defs []]
    (if (empty? todo-set-defs)
      (cons 'sets finished-set-defs)
      (let [current (first todo-set-defs)]
        (cond
          (seq? current) (recur (rest todo-set-defs) (conj finished-set-defs current))
          (keyword? current) (if (set? (second todo-set-defs))
                               (recur (drop 2 todo-set-defs) (conj finished-set-defs (concat (list 'enumerated-set current) (second todo-set-defs))))
                               (recur (rest todo-set-defs) (conj finished-set-defs (cons 'deferred-set [current]))))
          :else (throw (Exception. (str "Unsupported way of set definitions: " current))))))))

(defn process-op-definitions [operations-clause]
  (list* 'operations (reduce
                      (fn [acc op]
                        (cond (= '<-- (first op)) (let [returns (second op)
                                                        operation (last op)
                                                        name (first operation)
                                                        args (second operation)
                                                        body (pre-process-lisb (last operation))]
                                                    (conj acc (list 'op returns name args body)))
                              (keyword? (first op)) (conj acc (cons 'op op))
                              :else (throw (Exception. (str "Unsupported way of operation definitions: " op)))))
                      []
                      (rest operations-clause))))


(defn process-assign-returns [lisb]
  (if (= 3 (count lisb))
    (let [returns (second lisb)
          op-call (last lisb)]
      (list* 'op-call-with-returns returns (rest op-call)))
    (throw (Exception. (str "Unsupported way of assign-returns: " lisb)))))

(defn pre-process-lisb [lisb]
  (cond
    (and (set? lisb) (contains? lisb '|)) (process-comprehension-set lisb)
    (and (seq? lisb) (= 'sets (first lisb))) (process-set-definitions lisb)
    (and (seq? lisb) (= 'operations (first lisb))) (process-op-definitions lisb)
    (and (seq? lisb) (= '<-- (first lisb))) (process-assign-returns lisb)
    (seqable? lisb) (walk pre-process-lisb identity lisb)
    :else lisb))


(defmacro b [lisb]
  (let [pre-processed-lisb (pre-process-lisb lisb)]
    `(let [
           ; parse units
           ~'machine bmachine
           ~'model bmodel
           ~'system bsystem
           ~'refinement brefinement
           ~'implementation bimplementation

           ; machine clauses
           ; machine inclusions
           ~'uses buses
           ~'includes bincludes
           ~'sees bsees
           ~'extends bextends
           ~'promotes bpromotes
           ; machine sections
           ~'constraints bconstraints
           ~'sets bsets
           ~'deferred-set bdeferred-set
           ~'enumerated-set benumerated-set
           ~'constants bconstants
           ~'properties bproperties
           ~'definitions bdefinitions
           ~'variables bvariables
           ~'invariants binvariants
           ~'assertions bassertions
           ~'init binit
           ~'operations boperations
           ~'op bop

           ; substitutions
           ~'skip bskip
           ~'block bblock
           ~'assign bassign
           ~'set! bassign                                   ; sugar
           ~'becomes-element-of bbecomes-element-of
           ~'becomes-member bbecomes-element-of             ; sugar
           ~'becomes-such bbecomes-such
           ~'op-call bop-call
           ~'op-call-with-returns bop-call-with-returns
           ~'parallel-sub bparallel-sub
           ~'|| bparallel-sub                               ; sugar
           ~'sequential-sub bsequential-sub
           ~'any bany
           ~'let-sub blet-sub
           ~'var-sub bvar
           ~'pre bprecondition
           ~'assert bassert
           ~'choice bchoice
           ~'if-sub bif-sub
           ~'cond bcond
           ~'select bselect
           ~'case bcase

           ; if
           ~'if-expr bif-expr

           ; let
           ~'let blet

           ; trees

           ; reals - (alpha - besser nicht verwenden)

           ; strings
           ~'string-set bstring-set

           ; records
           ~'struct bstruct
           ~'record brecord
           ~'record-get brecord-get

           ; sequences
           ~'sequence bsequence
           ~'seq bseq
           ~'seq1 bseq1
           ~'iseq biseq
           ~'iseq1 biseq1
           ~'perm bperm
           ~'size bsize
           ~'concat bconcat
           ~'prepend bprepend
           ~'-> bprepend                                    ; sugar
           ~'append bappend
           ~'<- bappend                                     ; sugar
           ~'reverse breverse
           ~'first bfirst
           ~'last blast
           ~'front bfront
           ~'drop-last bfront                               ; clojure
           ~'tail btail
           ~'rest btail                                     ; clojure
           ~'conc bconc
           ~'take btake
           ~'drop bdrop

           ; functions
           ~'partial-function bpartial-function
           ~'+-> bpartial-function                          ; sugar
           ~'total-function btotal-function
           ~'--> btotal-function
           ~'partial-surjection bpartial-surjection
           ~'+->> bpartial-surjection
           ~'total-surjection btotal-surjection
           ~'-->> btotal-surjection
           ~'partial-injection bpartial-injection
           ~'>+> bpartial-injection
           ~'total-injection btotal-injection
           ~'>-> btotal-injection
           ~'partial-bijection bpartial-bijection
           ~'>+>> bpartial-bijection
           ~'total-bijection btotal-bijection
           ~'>->> btotal-bijection
           ~'lambda blambda
           ;~'fn bfn  ; sugar
           ~'fn-call bfn-call

           ; relations
           ~'relation brelation
           ~'<-> brelation                                  ; sugar
           ~'total-relation btotal-relation
           ~'<<-> btotal-relation                           ; sugar
           ~'surjective-relation bsurjective-relation
           ~'<->> bsurjective-relation                      ; sugar
           ~'total-surjective-relation btotal-surjective-relation
           ~'<<->> btotal-surjective-relation               ; sugar
           ~'maplet bmaplet
           ~'|-> bmaplet                                    ; sugar
           ~'dom bdom
           ~'ran bran
           ~'id bid
           ~'domain-restriction bdomain-restriction
           ~'<| bdomain-restriction                         ; sugar
           ~'domain-subtraction bdomain-subtraction
           ~'<<| bdomain-subtraction                        ; sugar
           ~'range-restriction brange-restriction
           ~'|> brange-restriction                          ; sugar
           ~'range-subtraction brange-subtraction
           ~'|>> brange-subtraction                         ; sugar
           ~'inverse binverse
           ~'image bimage
           ~'override boverride
           ~'<+ boverride                                   ; sugar
           ~'direct-product bdirect-product
           ~'>< bdirect-product                             ; sugar
           ~'composition bcomposition
           ~'parallel-product bparallel-product
           ~'prj1 bprj1
           ~'prj2 bprj2
           ~'closure1 bclosure1
           ~'closure bclosure
           ~'iterate biterate
           ~'fnc bfnc
           ~'rel brel

           ; numbers
           ~'integer-set binteger-set
           ~'natural-set bnatural-set
           ~'natural1-set bnatural1-set
           ~'int-set bint-set
           ~'nat-set bnat-set
           ~'nat1-set bnat1-set
           ~'interval binterval
           ~'range brange
           ~'min-int bmin-int
           ~'max-int bmax-int
           ~'> b>
           ~'< b<
           ~'>= b>=
           ~'<= b<=
           ~'max bmax
           ~'min bmin
           ~'+ b+
           ~'- b-
           ~'cart-or-mult bcart-or-mult
           ~'* b*                                           ; added separat multiplication
           ~'div bdiv
           ~'/ bdiv
           ~'** b**
           ~'mod bmod
           ~'pi bpi
           ~'π bpi                                          ; sugar
           ~'sigma bsigma
           ~'Σ bsigma                                       ; sugar
           ~'successor bsuccessor
           ~'inc bsuccessor                                 ; sugar
           ~'predecessor bpredecessor
           ~'dec bpredecessor                               ; sugar

           ;;; sets
           ~'comprehension-set bcomprehension-set
           ~'pow bpow
           ~'pow1 bpow1
           ~'fin bfin
           ~'fin1 bfin1
           ~'card bcard
           ~'cartesian-product bcartesian-product           ; added separat cartesian-product
           ; TODO: rebind ids in def-pred
           ;~'x bcartesian-product                             ; sugar
           ~'union bunion
           ~'intersection bintersection
           ~'set- bset-
           ~'member? bmember?
           ~'in bmember?                                    ; sugar
           ~'contains? bcontains?                           ; sugar
           ~'subset? bsubset?
           ~'strict-subset? bstrict-subset?
           ~'superset? bsuperset?                           ; sugar
           ~'strict-superset? bstrict-superset?             ; sugar
           ~'unite-sets bunite-sets
           ~'intersect-sets bintersect-sets
           ~'union-pe bunion-pe
           ~'intersection-pe bintersection-pe

           ;;; booleans
           ~'bool-set bbool-set
           ~'pred->bool bpred->bool

           ;;; equality predicates
           ~'= b=
           ~'not= bnot=
           ~'distinct? bdistinct?                           ; added funcionality

           ;;; logical predicates
           ~'and band
           ~'or bor
           ~'equivalence bequivalence
           ~'<=> bequivalence                               ; sugar
           ~'implication bimplication
           ~'=> bimplication                                ; sugar
           ~'not bnot
           ~'for-all bfor-all
           ~'exists bexists]
       ~pre-processed-lisb
       )))

(defn lisb->ir [lisb]
  (eval `(b ~lisb)))


(def bempty-machine (bmachine :Empty))


(defn wrap [ctx node]
  (cond
    (keyword? node) `(~node ~ctx)
    (map? node) (into {} (map (fn f [[k v]]  [k (wrap ctx v)]) node))
    (set? node) (set (map (partial wrap ctx) node))
    (list? node) (apply list (map (partial wrap ctx) node))
    (vector? node) (vec  (map (partial wrap ctx) node))
    :otherwise node))


(defn almost-flatten [x]
  (remove coll? (rest (tree-seq coll? seq x))))


(defmacro pred [name & args]
  (let [body (last args)
        params (drop-last args)
        ctx (gensym 'lisb_ctx_)
        wrapped-body (wrap ctx body)
        keywords (set (filter keyword? (almost-flatten body)))]
    `(fn ~name ~@params
       (let [~ctx (into {} (mapv (fn [x#] [x# (keyword (gensym "lisb_"))]) ~keywords))]
         (do (b ~wrapped-body))))))


(defmacro defpred [name & args]
  `(def ~name (pred ~name ~@args)))
