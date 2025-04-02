(ns lisb.translation.lisb2ir
  (:require [lisb.translation.types :refer [->Tuple]])
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:require [clojure.walk :refer [walk postwalk]])
  (:require [clojure.set])
  (:require [clojure.spec.alpha :as s]))


;; TODO: check where iterate needs to be listed as well

(def machine-clause-tags #{:uses :includes :sees :extends :promotes :constraints :sets :constants :properties
                           :definitions :freetypes :variables :invariants :assertions :init :operations})
(def substitution-tags #{:skip :block :assignment :becomes-element-of :becomes-such :op-call :parallel-sub :sequential-sub :any
                :let-sub :var :precondition :assert :choice :if-sub :cond :select :case})
(def seq-tags #{:empty-sequence :sequence :seq :seq1 :iseq :iseq1 :perm :concat :prepend :append :reverse :front
                :drop-last :conc :take :drop :tail})
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
                :convert-to-real :floor :ceil
                ; set numbers
                :cardinality
                ; seq numbers
                :size
                :sigma :pi
                })                                          ; numbers
(def set-tags #{:comprehension-set :power-set :power1-set :fin :fin1 :cartesian-product
                :cartesian-product-or-multiplication :union :intersection :difference :unite-sets :intersect-sets
                :union-pe :intersection-pe
                :iterate
                ; boolean-sets
                :bool-set
                ; number-sets
                :real-set :integer-set :natural-set :natural1-set :int-set :nat-set :nat1-set :interval
                ; string-sets
                :string-set
                ; rel-sets
                :dom :ran
                ; structs
                :struct }) ; #{}
(def boolean-tags #{:pred->bool})                           ; true false
(def expr-tags (clojure.set/union seq-tags fn-tags rel-tags set-tags num-tags boolean-tags
                                  #{:let :if
                                    ; records
                                    :record :record-get
                                    ; seqs
                                    :first :last
                                    ; tuples
                                    :eventb-prj1 :eventb-prj2
                                    ; fns
                                    :fn-call
                                    }))
(def pred-tags #{:let :if
                 ; logical predicates
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

;; TODO: duplicated code, also in ir2ast
(defn- to-vec [v]
  (if (vector? v)
    v
    [v]))


;;; parse units

(defn process-machine-name [m machine-name]
  (if (seqable? machine-name)
    (let [[name & args] machine-name]
      (assoc m :name name :args args))
    (assoc m :name machine-name :args [])))
(s/fdef process-machine-name
        :args (s/cat :m ::m :machine-name ::machine-name)
        :ret (s/keys :req-un [::tag] :req [::machine-clauses])
        )             ; TODO: concretize spec

(defn bmachine [machine-name & machine-clauses]
  (process-machine-name
    {:tag :machine
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef bmachine
        :args (s/cat :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un [::tag] :req [::machine-clauses])
                    #(= :machine (:tag %))))             ; TODO: concretize spec

(defn bmodel [machine-name & machine-clauses]
  (process-machine-name
    {:tag :model
     :machine-clauses   machine-clauses}
    machine-name))
(s/fdef bmodel
        :args (s/cat :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un [::tag] :req [::machine-clauses])
                    #(= :model (:tag %))))             ; TODO: concretize spec

(defn bsystem [machine-name & machine-clauses]
  (process-machine-name
    {:tag :system
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef bsystem
        :args (s/cat :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un [::tag] :req [::machine-clauses])
                    #(= :system (:tag %))))             ; TODO: concretize spec

(defn brefinement [machine-name abstract-machine-name & machine-clauses]
  (process-machine-name
    {:tag :refinement
     :abstract-machine-name abstract-machine-name
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef brefinement
        :args (s/cat :machine-names ::machine-names :abstract-machine-name ::abstract-machine-name :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un [::tag] :req [::abstract-machine-name ::machine-clauses])
                    #(= :refinement (:tag %))))             ; TODO: concretize spec

(defn bimplementation [machine-name abstract-machine-name & machine-clauses]
  (process-machine-name
    {:tag :implementation
     :abstract-machine-name abstract-machine-name
     :machine-clauses machine-clauses}
    machine-name))
(s/fdef bimplementation
        :args (s/cat :machine-names ::machine-names :abstract-machine-name ::abstract-machine-name :machine-clauses ::machine-clauses)
        :ret (s/and (s/keys :req-un [::tag] :req [::abstract-machine-name ::machine-clauses])
                    #(= :implementation (:tag %))))             ; TODO: concretize spec


;;; machine clauses
;; machine inclusions

(defn process-machine-reference [machine-reference]
  (process-machine-name {:tag :machine-reference} machine-reference))
(s/fdef process-machine-reference
        :args (s/cat :machine-references ::machine-references)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :machine-reference (:tag %))))              ; TODO: concretize spec

(defn buses [& machine-names]
  {:tag :uses
   :values machine-names})
(s/fdef buses
        :args (s/cat :machine-names ::machine-names)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :uses (:tag %))))

(defn bincludes [& machine-references]
  {:tag :includes
   :values (map process-machine-reference machine-references)})
(s/fdef bincludes
        :args (s/cat :machine-references ::machine-references)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :includes (:tag %))))

(defn bsees [& machine-names]
  {:tag :sees
   :values machine-names})
(s/fdef bsees
        :args (s/cat :machine-names ::machine-names)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :sees (:tag %))))

(defn bextends [& machine-references]
  {:tag    :extends
   :values (map process-machine-reference machine-references)})
(s/fdef bextends
        :args (s/cat :machine-references ::machine-references)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :extends (:tag %))))

(defn bpromotes [& ops]
  {:tag :promotes
   :values ops})
(s/fdef bpromotes
        :args (s/cat :ops ::ops)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :promotes (:tag %))))

;; machine sections

(defn bconstraints [& preds]
  {:tag :contraints
   :values preds})
(s/fdef bconstraints
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :contraints (:tag %))))

(defn bsets [& set-defs]
  {:tag :sets
   :values set-defs})
(s/fdef bsets
        :args (s/cat :set-defs ::set-defs)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :sets (:tag %))))
(defn bdeferred-set [id]
  {:tag :deferred-set
   :id id})
(s/fdef bdeferred-set
        :args (s/cat :id ::id)
        :ret (s/and (s/keys :req-un [::tag] :req [::id])
                    #(= :deferred-set (:tag %))))
(defn benumerated-set [id & elems]
  {:tag :enumerated-set
   :id id
   :elems elems})
(s/fdef benumerated-set
        :args (s/cat :id ::id :elems ::elems)
        :ret (s/and (s/keys :req-un [::tag] :req [::id ::elems])
                    #(= :enumerated-set (:tag %))))

(defn bconstants [& ids]
  {:tag :constants
   :values ids})
(s/fdef bconstants
        :args (s/cat :ids ::ids)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :constants (:tag %))))

(defn babstract-constants [& ids]
  {:tag :abstract-constants
   :values ids})
(s/fdef babstract-constants
        :args (s/cat :ids ::ids)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :constants (:tag %))))

(defn bproperties [& preds]
  {:tag :properties
   :values preds})
(s/fdef bproperties
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :properties (:tag %))))

(defn bdefinitions [& defs]
  {:tag :definitions
   :values defs})
(s/fdef bdefinitions
        :args (s/cat :defs ::defs)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :definitions (:tag %))))
(defn bexpression-definition [name args expr]
  {:tag :expression-definition
   :name name
   :args args
   :expr expr})
(s/fdef bexpression-definition
        :args (s/cat :name ::name :args ::args :expr ::expr)
        :ret (s/and (s/keys :req-un [::tag] :req [::name ::args ::expr])
                    #(= :expression-definition (:tag %))))
(defn bpredicate-definition [name args pred]
  {:tag :predicate-definition
   :name name
   :args args
   :pred pred})
(s/fdef bpredicate-definition
        :args (s/cat :name ::name :args ::args :pred ::pred)
        :ret (s/and (s/keys :req-un [::tag] :req [::name ::args ::pred])
                    #(= :predicate-definition (:tag %))))
(defn bsubstitution-definition [name args sub]
  {:tag :substitution-definition
   :name name
   :args args
   :sub sub})
(s/fdef bsubstitution-definition
        :args (s/cat :name ::name :args ::args :sub ::sub)
        :ret (s/and (s/keys :req-un [::tag] :req [::name ::args ::sub])
                    #(= :substitution-definition (:tag %))))
(defn bfile-definition [file]
  {:tag :file-definition
   :file file})
(s/fdef bfile-definition
        :args (s/cat :file ::file)
        :ret (s/and (s/keys :req-un [::tag] :req [::file])
                    #(= :file-definition (:tag %))))

(defn bfreetypes [& ft-defs]
  {:tag :freetypes
   :values ft-defs})
(s/fdef bfreetypes
  :args (s/cat :ft-defs ::ft-defs)
  :ret (s/and (s/keys :req-un [::tag] :req [::values])
              #(= :freetypes (:tag %))))
(defn bfreetype [id args & constructors]
  {:tag :freetype
   :id id
   :args args
   :constructors constructors})
(s/fdef bfreetype
  :args (s/cat :id ::id :args ::args :constructors ::constructors)
  :ret (s/and (s/keys :req-un [::tag] :req [::id ::args ::values])
              #(= :freetype (:tag %))))
(defn bconstructor
  ([id] {:tag :ft-element
         :id id})
  ([id argument] {:tag :ft-constructor
                  :id id
                  :expr argument}))
(s/fdef bconstructor
  :args (s/cat :id ::id :argument (s/nilable ::expr))
  :ret (s/or :ft-element (s/and (s/keys :req-un [::tag] :req [::id]) #(= :ft-element (:tag %)))
             :ft-constructor (s/and (s/keys :req-un [::tag] :req [::id ::expr]) #(= :ft-constructor (:tag %)))))

(defn bvariables [& ids]
  {:tag :variables
   :values ids})
(s/fdef bvariables
        :args (s/cat :ids ::ids)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :variables (:tag %))))

(defn binvariants [& preds]
  {:tag :invariants
   :values preds})
(s/fdef binvariants
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :invariants (:tag %))))

(defn bassertions [& preds]
  {:tag :assertions
   :values preds})
(s/fdef bassertions
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :assertions (:tag %))))

(defn binit [& subs]
  {:tag :init
   :values subs})
(s/fdef binit
        :args (s/cat :subs ::subs)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
                    #(= :init (:tag %))))

(defn boperations [& op-defs]
  {:tag :operations
   :values op-defs})
(s/fdef boperations
        :args (s/cat :op-defs ::op-defs)
        :ret (s/and (s/keys :req-un [::tag] :req [::values])
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
        :ret (s/and (s/keys :req-un [::tag] :req [::name ::args ::body] :opt [::returns])
                    #(= :op (:tag %))))


;;; substitutions

(def bskip {:tag :skip})
(s/fdef bskip
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :skip (:tag %))))

(defn bblock [sub]
  {:tag :block
   :sub sub})
(s/fdef bblock
        :args (s/cat :sub ::sub)
        :ret (s/and (s/keys :req-un [::tag] :req [::sub])
                    #(= :block (:tag %))))

(defn bassign [& id-vals]
  {:tag :assignment
   :id-vals id-vals})
(s/fdef bassign
        :args (s/cat :id-vals ::id-vals)
        :ret (s/and (s/keys :req-un [::tag] :req [::id-vals])
                    #(= :assignment (:tag %))))

(defn bbecomes-element-of [ids set]
  {:tag :becomes-element-of
   :ids (to-vec ids)
   :set set})
(s/fdef bbecomes-element-of
        :args (s/cat :ids ::ids :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::set])
                    #(= :becomes-element-of (:tag %))))

(defn bbecomes-such [ids pred]
  {:tag :becomes-such
   :ids (to-vec ids)
   :pred pred})
(s/fdef bbecomes-such
        :args (s/cat :ids ::ids :pred ::pred)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred])
                    #(= :becomes-such (:tag %))))

(defn bop-call-with-returns [returns op & args]
  {:tag     :op-call
   :returns returns
   :op      op
   :args    args})
(s/fdef bop-call-with-returns
        :args (s/cat :returns ::returns :op ::op :args ::args)
        :ret (s/and (s/keys :req-un [::tag] :req [::returns ::op ::args])
                    #(= :op-call (:tag %))))

(defn bop-call [op & args]
  (apply (partial bop-call-with-returns [] op) args))
(s/fdef bop-call-with-returns
        :args (s/cat :op ::op :args ::args)
        :ret (s/and (s/keys :req-un [::tag] :req [::returns ::op ::args])
                    #(= :op-call (:tag %))))               ; TODO: concretize spec

(defn bparallel-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :parallel-sub
     :subs subs}))
(s/fdef bparallel-sub
        :args (s/cat :subs ::subs)
        :ret (s/or :parallel-sub (s/and (s/keys :req-un [::tag] :req [::subs])
                                          #(= :parallel-sub (:tag %)))
                   :sub ::sub))

(defn bsequential-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :sequential-sub
     :subs subs}))
(s/fdef bsequential-sub
        :args (s/cat :subs ::subs)
        :ret (s/or :sequential-sub (s/and (s/keys :req-un [::tag] :req [::subs])
                                          #(= :sequential-sub (:tag %)))
                   :sub ::sub))

(defn bany [ids pred & subs]
  {:tag :any
   :ids (to-vec ids)
   :pred pred
   :subs subs})
(s/fdef bany
        :args (s/cat :ids ::ids :pred ::pred :subs ::subs)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred ::subs])
                    #(= :any (:tag %))))

(defn blet-sub [id-vals & subs]
  {:tag :let-sub
   :id-vals id-vals
   :subs subs})
(s/fdef blet-sub
        :args (s/cat :id-vals ::id-vals :subs ::subs)
        :ret (s/and (s/keys :req-un [::tag] :req [::id-vals ::subs])
                    #(= :let-sub (:tag %))))

(defn bvar [ids & subs]
  {:tag :var
   :ids (to-vec ids)
   :subs subs})
(s/fdef bvar
        :args (s/cat :ids ::ids :subs ::subs)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::subs])
                    #(= :var (:tag %))))

(defn bprecondition [pred & subs]
  {:tag :precondition
   :pred pred
   :subs subs})
(s/fdef bprecondition
        :args (s/cat :pred ::pred :subs ::subs)
        :ret (s/and (s/keys :req-un [::tag] :req [::pred ::subs])
                    #(= :precondition (:tag %))))

(defn bassert [pred & subs]
  {:tag :assert
   :pred pred
   :subs subs})
(s/fdef bassert
        :args (s/cat :pred ::pred :subs ::subs)
        :ret (s/and (s/keys :req-un [::tag] :req [::pred ::subs])
                    #(= :assert (:tag %))))

(defn bchoice [& subs]
  {:tag :choice
   :subs subs})
(s/fdef bchoice
        :args (s/cat :subs ::subs)
        :ret (s/and (s/keys :req-un [::tag] :req [::subs])
                    #(= :choice (:tag %))))

(defn bif-sub
  ([cond then] {:tag :if-sub
                :cond cond
                :then then})
  ([cond then else] (if else
                      {:tag :if-sub
                       :cond cond
                       :then then
                       :else else}
                      (bif-sub cond then))))
(s/fdef bif-sub
        :args (s/cat :cond ::cond :then ::then :else(s/? ::else))
        :ret (s/and (s/keys :req-un [::tag] :req [::cond ::then] :opt [::else])
                    #(= :if-sub (:tag %))))
(defn bcond [& clauses]
    {:tag :cond
     :clauses clauses})
(s/fdef bcond
        :args (s/cat :clauses ::clauses)
        :ret (s/and (s/keys :req-un [::tag] :req [::clauses])
                    #(= :cond (:tag %))))

(defn bselect [& clauses]
  {:tag :select
   :clauses clauses})
(s/fdef bselect
        :args (s/cat :clauses ::clauses)
        :ret (s/and (s/keys :req-un [::tag] :req [::clauses])
                    #(= :select (:tag %))))

(defn bcase [expr & cases]
  {:tag :case
   :expr expr
   :cases cases})
(s/fdef bcase
        :args (s/cat :expr ::expr :cases ::cases)
        :ret (s/and (s/keys :req-un [::tag] :req [::expr ::cases])
                    #(= :case (:tag %))))


;;; if

#_(defn bif
    ([node cond then] (assoc node :cond cond :then then))
    ([node cond then else]
     (if else
       (assoc node :cond cond :then then :else else)
       (bif node cond then))))

(defn bif
  ([cond then else] {:tag :if :cond cond :then then :else else})) ; else is always present
(s/fdef bif
        :args (s/cat :cond ::cond :then ::then :else ::else)
        :ret (s/and (s/keys :req-un [::tag] :req [::cond ::then ::else])
                    #(= :if (:tag %))))


;;; let

#_(defn blet [node kvs]
    (let [kv-pairs (partition 2 kvs)
          identifiers (map first kv-pairs)
          assignment (reduce band (map (partial apply b=) kv-pairs))]
      (assoc node :identifiers identifiers :assignment assignment)))

(defn blet [id-vals expr-or-pred]
  {:tag :let
   :id-vals id-vals
   :expr-or-pred expr-or-pred})
(s/fdef blet
        :args (s/cat :id-vals ::id-vals :expr-or-pred ::expr-or-pred)
        :ret (s/and (s/keys :req-un [::tag] :req [::id-vals ::expr-or-pred])
                    #(= :let (:tag %))))

;;; trees


;;; reals - (alpha - besser nicht verwenden)

(def breal-set {:tag :real-set})

(defn bto-real [x]
  {:tag :convert-to-real
   :expr x})

(defn bfloor [x]
  {:tag :floor
   :expr x})

(defn bceil [x]
  {:tag :ceil
   :expr x})


;;; strings

(def bstring-set
  {:tag :string-set})
(s/fdef bstring-set
        :args (s/cat )
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :string-set (:tag %))))


;;; records

(defn bstruct [& id-types]
  {:tag :struct
   :id-types id-types})
(s/fdef bstruct
        :args (s/cat :id-types ::id-types)
        :ret (s/and (s/keys :req-un [::tag] :req [::id-types])
                    #(= :struct (:tag %))))

(defn brecord [& id-vals]
  {:tag :record
   :id-vals id-vals})
(s/fdef brecord
        :args (s/cat :id-vals ::id-vals)
        :ret (s/and (s/keys :req-un [::tag] :req [::id-vals])
                    #(= :record (:tag %))))

(defn brecord-get [rec id]
  {:tag :record-get
   :rec rec
   :id id})
(s/fdef brecord-get
        :args (s/cat :rec ::rec :id ::id)
        :ret (s/and (s/keys :req-un [::tag] :req [::rec ::id])
                    #(= :record-get (:tag %))))


;;; sequences

(defn bsequence
  ([] {:tag :empty-sequence})
  ([& elems]
   {:tag    :sequence
    :elems elems}))
(s/fdef bsequence
        :args (s/or :empty-sequence (s/cat) :sequence (s/cat :elems ::elems))
        :ret (s/and (s/keys :req-un [::tag] :opt [::elems])
                    #(contains? #{:empty-sequence :sequence} (:tag %))))

(defn bseq [set]
  {:tag :seq
   :set set})
(s/fdef bseq
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :seq (:tag %))))

(defn bseq1 [set]
  {:tag :seq1
   :set set})
(s/fdef bseq1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :seq1 (:tag %))))

(defn biseq [set]
  {:tag :iseq
   :set set})
(s/fdef biseq
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :iseq (:tag %))))

(defn biseq1 [set]
  {:tag :iseq1
   :set set})
(s/fdef biseq1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :iseq1 (:tag %))))

(defn bperm [set]
  {:tag :perm
   :set set})
(s/fdef bperm
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :perm (:tag %))))

(defn bsize [seq]
  {:tag :size
   :seq seq})
(s/fdef bsize
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq])
                    #(= :size (:tag %))))

(defn bconcat [& seqs]
  {:tag :concat
   :seqs seqs})
(s/fdef bconcat
        :args (s/cat :seqs ::seqs)
        :ret (s/and (s/keys :req-un [::tag] :req [::seqs])
                    #(= :concat (:tag %))))

(defn bprepend [elem seq]
  {:tag :prepend
   :elem elem
   :seq seq})
(s/fdef bprepend
        :args (s/cat :elem ::elem :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq ::elem])
                    #(= :prepend (:tag %))))

(defn bappend [seq & elems]
  {:tag :append
   :seq seq
   :elems elems})
(s/fdef bappend
        :args (s/cat :seq ::seq :elems ::elems)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq ::elems])
                    #(= :append (:tag %))))

(defn breverse [seq]
  {:tag :reverse
   :seq seq})
(s/fdef breverse
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq])
                    #(= :reverse (:tag %))))

(defn bfirst [seq]
  {:tag :first
   :seq seq})
(s/fdef bfirst
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq])
                    #(= :first (:tag %))))

(defn blast [seq]
  {:tag :last
   :seq seq})
(s/fdef blast
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq])
                    #(= :last (:tag %))))

(defn bfront [seq]
  {:tag :front
   :seq seq})
(s/fdef bfront
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq])
                    #(= :front (:tag %))))

(defn btail [seq]
  {:tag :tail
   :seq seq})
(s/fdef btail
        :args (s/cat :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq])
                    #(= :tail (:tag %))))

(defn bconc [seq-of-seqs]
  {:tag :conc
   :seq-of-seqs seq-of-seqs})
(s/fdef bconc
        :args (s/cat :seq-of-seqs ::seq-of-seqs)
        :ret (s/and (s/keys :req-un [::tag] :req [::seq-of-seqs])
                    #(= :conc (:tag %))))

(defn btake [num seq]
  {:tag :take
   :seq seq
   :num num})
(s/fdef btake
        :args (s/cat :num ::num :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::num ::seq])
                    #(= :take (:tag %))))

(defn bdrop [num seq]
  {:tag :drop
   :seq seq
   :num num})
(s/fdef bdrop
        :args (s/cat :num ::num :seq ::seq)
        :ret (s/and (s/keys :req-un [::tag] :req [::num ::seq])
                    #(= :drop (:tag %))))


;;; functions

(defn bpartial-function [& sets]
  {:tag :partial-fn
   :sets sets})
(s/fdef bpartial-function
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :partial-fn (:tag %))))

(defn btotal-function [& sets]
  {:tag :total-fn
   :sets sets})
(s/fdef btotal-function
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :total-fn (:tag %))))

(defn bpartial-surjection [& sets]
  {:tag :partial-surjection
   :sets sets})
(s/fdef bpartial-surjection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :partial-surjection (:tag %))))

(defn btotal-surjection [& sets]
  {:tag :total-surjection
   :sets sets})
(s/fdef btotal-surjection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :total-surjection (:tag %))))

(defn bpartial-injection [& sets]
  {:tag :partial-injection
   :sets sets})
(s/fdef bpartial-injection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :partial-injection (:tag %))))

(defn btotal-injection [& sets]
  {:tag :total-injection
   :sets sets})
(s/fdef btotal-injection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :total-injection (:tag %))))

(defn bpartial-bijection [& sets]
  {:tag :partial-bijection
   :sets sets})
(s/fdef bpartial-bijection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :partial-bijection (:tag %))))

(defn btotal-bijection [& sets]
  {:tag :total-bijection
   :sets sets})
(s/fdef btotal-bijection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :total-bijection (:tag %))))

(defn blambda [ids pred expr]
  {:tag :lambda
   :ids (to-vec ids)
   :pred pred
   :expr expr})
(s/fdef blambda
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred ::expr])
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
        :ret (s/and (s/keys :req-un [::tag] :req [::f ::args])
                    #(= :fn-call (:tag %))))


;;; relations

(defn brelation [& sets]
  {:tag :relation
   :sets sets})
(s/fdef brelation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :relation (:tag %))))

(defn btotal-relation [& sets]
  {:tag :total-relation
   :sets sets})
(s/fdef btotal-relation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :total-relation (:tag %))))

(defn bsurjective-relation [& sets]
  {:tag :surjective-realtion
   :sets sets})
(s/fdef bsurjective-relation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :surjective-realtion (:tag %))))

(defn btotal-surjective-relation [& sets]
  {:tag :total-surjective-relation
   :sets sets})
(s/fdef btotal-surjective-relation
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :total-surjective-relation (:tag %))))

(defn bmaplet [& elems]
  {:tag :maplet
   :elems elems})
(s/fdef bmaplet
        :args (s/cat :elems ::elems)
        :ret (s/and (s/keys :req-un [::tag] :req [::elems])
                    #(= :maplet (:tag %))))

(defn bdom [rel]
  {:tag :dom
   :rel rel})
(s/fdef bdom
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel])
                    #(= :dom (:tag %))))

(defn bran [rel]
  {:tag :ran
   :rel rel})
(s/fdef bran
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel])
                    #(= :ran (:tag %))))

(defn bid [set]
  {:tag :id
   :set set})
(s/fdef bid
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :id (:tag %))))

(defn bdomain-restriction [set rel]
  {:tag :domain-restriction
   :set set
   :rel rel})
(s/fdef bdomain-restriction
        :args (s/cat :set ::set :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel ::set])
                    #(= :domain-restriction (:tag %))))

(defn bdomain-subtraction [set rel]
  {:tag :domain-subtraction
   :set set
   :rel rel})
(s/fdef bdomain-subtraction
        :args (s/cat :set ::set :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel ::set])
                    #(= :domain-subtraction (:tag %))))

(defn brange-restriction [rel set]
  {:tag :range-restriction
   :rel rel
   :set set})
(s/fdef brange-restriction
        :args (s/cat :rel ::rel :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel ::set])
                    #(= :range-restriction (:tag %))))

(defn brange-subtraction [rel set]
  {:tag :range-subtraction
   :rel rel
   :set set})
(s/fdef brange-subtraction
        :args (s/cat :rel ::rel :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel ::set])
                    #(= :range-subtraction (:tag %))))

(defn binverse [rel]
  {:tag :inverse
   :rel rel})
(s/fdef binverse
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel])
                    #(= :inverse (:tag %))))

(defn bimage [rel set]
  {:tag :image
   :rel rel
   :set set})
(s/fdef bimage
        :args (s/cat :rel ::rel :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel ::set])
                    #(= :image (:tag %))))

(defn boverride [& rels]
  {:tag :override
   :rels rels})
(s/fdef boverride
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un [::tag] :req [::rels])
                    #(= :override (:tag %))))

(defn bdirect-product [& rels]
  {:tag :direct-product
   :rels rels})
(s/fdef bdirect-product
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un [::tag] :req [::rels])
                    #(= :direct-product (:tag %))))

(defn bcomposition [& rels]
  {:tag :composition
   :rels rels})
(s/fdef bcomposition
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un [::tag] :req [::rels])
                    #(= :composition (:tag %))))

(defn bparallel-product [& rels]
  {:tag :parallel-product
   :rels rels})
(s/fdef bparallel-product
        :args (s/cat :rels ::rels)
        :ret (s/and (s/keys :req-un [::tag] :req [::rels])
                    #(= :parallel-product (:tag %))))

(defn bprj1 [set1 set2]
  {:tag :prj1
   :set1 set1
   :set2 set2})
(s/fdef bprj1
        :args (s/cat :set1 ::set1 :set2 ::set2)
        :ret (s/and (s/keys :req-un [::tag] :req [::set1 ::set2])
                    #(= :prj1 (:tag %))))

(defn bprj2 [set1 set2]
  {:tag :prj2
   :set1 set1
   :set2 set2})
(s/fdef bprj2
        :args (s/cat :set1 ::set1 :set2 ::set2)
        :ret (s/and (s/keys :req-un [::tag] :req [::set1 ::set2])
                    #(= :prj2 (:tag %))))

(defn beventb-prj1 [expr]
  {:tag :eventb-prj1
   :expr expr})
(s/fdef beventb-prj1
  :args (s/cat :expr ::expr)
  :ret (s/and (s/keys :req-un [::tag] :req [::expr])
              #(= :eventb-prj1 (:tag %))))

(defn beventb-prj2 [expr]
  {:tag :eventb-prj2
   :expr expr})
(s/fdef beventb-prj2
  :args (s/cat :expr ::expr)
  :ret (s/and (s/keys :req-un [::tag] :req [::expr])
              #(= :eventb-prj2 (:tag %))))

(defn bclosure1 [rel]
  {:tag :closure1
   :rel rel})
(s/fdef bclosure1
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel])
                    #(= :closure1 (:tag %))))

(defn bclosure [rel]
  {:tag :closure
   :rel rel})
(s/fdef bclosure
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel])
                    #(= :closure (:tag %))))

(defn biterate [rel num]
  {:tag :iterate
   :rel rel
   :num num})
(s/fdef biterate
        :args (s/cat :rel ::rel :num ::num)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel ::num])
                    #(= :iterate (:tag %))))

(defn bfnc [rel]
  {:tag :fnc
   :rel rel})
(s/fdef bfnc
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel])
                    #(= :fnc (:tag %))))

(defn brel [rel]
  {:tag :rel
   :rel rel})
(s/fdef brel
        :args (s/cat :rel ::rel)
        :ret (s/and (s/keys :req-un [::tag] :req [::rel])
                    #(= :rel (:tag %))))


;;; numbers

(def binteger-set {:tag :integer-set})
(s/fdef binteger-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :integer-set (:tag %))))

(def bnatural-set {:tag :natural-set})
(s/fdef bnatural-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :natural-set (:tag %))))

(def bnatural1-set {:tag :natural1-set})
(s/fdef bnatural1-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :natural1-set (:tag %))))

(def bint-set {:tag :int-set})
(s/fdef bint-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :int-set (:tag %))))

(def bnat-set {:tag :nat-set})
(s/fdef bnat-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :nat-set (:tag %))))

(def bnat1-set {:tag :nat1-set})
(s/fdef bnat1-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :nat1-set (:tag %))))

(defn binterval [from to]
  {:tag :interval
   :from from
   :to to})
(s/fdef binterval
        :args (s/cat :from ::from :to ::to)
        :ret (s/and (s/keys :req-un [::tag] :req [::from ::to])
                    #(= :interval (:tag %))))

(defn brange [from to]
  (binterval from (bpredecessor to)))
(s/fdef binterval
        :args (s/cat :from ::from :to ::to)
        :ret (s/and (s/keys :req-un [::tag] :req [::from ::to])
                    #(= :interval (:tag %))))  ; TODO: concretize spec

(def bmin-int
  {:tag :min-int})
(s/fdef bmin-int
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :min-int (:tag %))))

(def bmax-int
  {:tag :max-int})
(s/fdef bmax-int
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :max-int (:tag %))))

(defn b> [& nums]
  {:tag :greater
   :nums nums})
(s/fdef b>
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :greater (:tag %))))

(defn b< [& nums]
  {:tag :less
   :nums nums})
(s/fdef b<
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :less (:tag %))))

(defn b>= [& nums]
  {:tag :greater-equals
   :nums nums})
(s/fdef b>=
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :greater-equals (:tag %))))

(defn b<= [& nums]
  {:tag :less-equals
   :nums nums})
(s/fdef b<=
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
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
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
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
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :min (:tag %))))

(defn b+ [& nums]
  {:tag :add
   :nums nums})
(s/fdef b+
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :add (:tag %))))

(defn b- [& nums]
  (if (= 1 (count nums))
    {:tag :unary-minus, :num (first nums)}
    {:tag :sub, :nums nums}))
(s/fdef b-
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :sub (:tag %))))

(defn bcart-or-mult [& nums-or-sets]
  {:tag :cartesian-product-or-multiplication
   :nums-or-sets nums-or-sets})
(s/fdef bcart-or-mult
        :args (s/cat :nums-or-sets ::nums-or-sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums-or-sets])
                    #(= :cartesian-product-or-multiplication (:tag %))))

(defn b* [& nums]
  {:tag :mul
   :nums nums})
(s/fdef b*
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :mul (:tag %))))

(defn bdiv [& nums]
  {:tag :div
   :nums nums})
(s/fdef bdiv
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :div (:tag %))))

(defn b** [& nums]
  {:tag :pow
   :nums nums})
(s/fdef b**
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :pow (:tag %))))

(defn bmod [& nums]
  {:tag :mod
   :nums nums})
(s/fdef bmod
        :args (s/cat :nums ::nums)
        :ret (s/and (s/keys :req-un [::tag] :req [::nums])
                    #(= :mod (:tag %))))

(defn bpi [ids pred expr]
  {:tag :pi
   :ids (to-vec ids)
   :pred pred
   :expr expr})
(s/fdef bpi
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred ::expr])
                    #(= :pi (:tag %))))

(defn bsigma [ids pred expr]
  {:tag :sigma
   :ids (to-vec ids)
   :pred pred
   :expr expr})
(s/fdef bsigma
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred ::expr])
                    #(= :sigma (:tag %))))

(defn bsuccessor [num]
  {:tag :successor
   :num num})
(s/fdef bsuccessor
        :args (s/cat :num ::num)
        :ret (s/and (s/keys :req-un [::tag] :req [::num])
                    #(= :successor (:tag %))))

(defn bpredecessor [num]
  {:tag :predecessor
   :num num})
(s/fdef bpredecessor
        :args (s/cat :num ::num)
        :ret (s/and (s/keys :req-un [::tag] :req [::num])
                    #(= :predecessor (:tag %))))


;;; sets

(defn bcomprehension-set
  ([ids pred] {:tag  :comprehension-set
               :ids  (to-vec ids)
               :pred pred
               :expr nil})
  ([ids pred expr] {:tag  :comprehension-set
                    :ids  (to-vec ids)
                    :pred pred
                    :expr expr}))
#_(s/fdef bcomprehension-set
        :args (s/cat :ids ::ids :pred ::pred)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred])
                    #(= :comprehension-set (:tag %))))

(defn bpow [set]
  {:tag :power-set
   :set set})
(s/fdef bpow
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :power-set (:tag %))))

(defn bpow1 [set]
  {:tag :power1-set
   :set set})
(s/fdef bpow1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :power1-set (:tag %))))

(defn bfin [set]
  {:tag :fin
   :set set})
(s/fdef bfin
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :fin (:tag %))))

(defn bfin1 [set]
  {:tag :fin1
   :set set})
(s/fdef bfin1
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :fin1 (:tag %))))

(defn bcard [set]
  {:tag :cardinality
   :set set})
(s/fdef bcard
        :args (s/cat :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::set])
                    #(= :cardinality (:tag %))))

(defn bcartesian-product [& sets]
  {:tag :cartesian-product
   :sets sets})
(s/fdef bcartesian-product
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :cartesian-product (:tag %))))

(defn bunion [& sets]
  {:tag :union
   :sets sets})
(s/fdef bunion
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :union (:tag %))))

(defn bintersection [& sets]
  {:tag :intersection
   :sets sets})
(s/fdef bintersection
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :intersection (:tag %))))

(defn bset- [& sets]
  {:tag :difference
   :sets sets})
(s/fdef bset-
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :difference (:tag %))))

(defn bmember? [elem set]
  {:tag :member
   :elem elem
   :set set})
(s/fdef bmember?
        :args (s/cat :elem ::elem :set ::set)
        :ret (s/and (s/keys :req-un [::tag] :req [::elem ::set])
                    #(= :member (:tag %))))

(defn bcontains? [set & elems]
  (apply band (reduce
                (fn [res elem]
                  (conj res (bmember? elem set)))
                []
                elems)))
(s/fdef bcontains?
        :args (s/cat :set ::set  :elem ::elem )
        :ret (s/and (s/keys :req-un [::tag] :req [::preds])
                    #(= :and (:tag %))))  ; TODO: concretize spec

(defn bsubset? [& sets]
  {:tag :subset
   :sets sets})
(s/fdef bsubset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :subset (:tag %))))

(defn bstrict-subset? [& sets]
  {:tag :strict-subset
   :sets sets})
(s/fdef bstrict-subset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :strict-subset (:tag %))))

; adds superset to lisb
(defn bsuperset? [& sets]
  (apply bsubset? (reverse sets)))
(s/fdef bsuperset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :subset (:tag %))))  ; TODO: concretize spec

; adds superset-strict to lisb
(defn bstrict-superset? [& sets]
  (apply bstrict-subset? (reverse sets)))
(s/fdef bstrict-superset?
        :args (s/cat :sets ::sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::sets])
                    #(= :strict-subset (:tag %))))  ; TODO: concretize spec

(defn bunite-sets [set-of-sets]
  {:tag :unite-sets
   :set-of-sets set-of-sets})
(s/fdef bunite-sets
        :args (s/cat :set-of-sets ::set-of-sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::set-of-sets])
                    #(= :unite-sets (:tag %))))

(defn bintersect-sets [set-of-sets]
  {:tag :intersect-sets
   :set-of-sets set-of-sets})
(s/fdef bintersect-sets
        :args (s/cat :set-of-sets ::set-of-sets)
        :ret (s/and (s/keys :req-un [::tag] :req [::set-of-sets])
                    #(= :intersect-sets (:tag %))))

(defn bunion-pe [ids pred expr]
  {:tag :union-pe
   :ids (to-vec ids)
   :pred pred
   :expr expr})
(s/fdef bunion-pe
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred ::expr])
                    #(= :union-pe (:tag %))))

(defn bintersection-pe [ids pred expr]
  {:tag :intersection-pe
   :ids (to-vec ids)
   :pred pred
   :expr expr})
(s/fdef bintersection-pe
        :args (s/cat :ids ::ids :pred ::pred :expr ::expr)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred ::expr])
                    #(= :intersection-pe (:tag %))))


;;; booleans

(def bbool-set {:tag :bool-set})
(s/fdef bbool-set
        :args (s/cat)
        :ret (s/and (s/keys :req-un [::tag])
                    #(= :bool-set (:tag %))))

(defn bpred->bool [pred]
  {:tag :pred->bool
   :pred pred})
(s/fdef bpred->bool
        :args (s/cat :pred ::pred)
        :ret (s/and (s/keys :req-un [::tag] :req [::pred])
                    #(= :pred->bool (:tag %))))


;;; equality predicates

(defn b=
  ([left right]
   {:tag :equals
    :left left
    :right right})
  ([left right & more]
   (apply band (map (partial apply b=) (partition 2 1 (cons left (cons right more)))))))
(s/fdef b=
        :args (s/cat :left ::left :right ::right)
        :ret (s/and (s/keys :req-un [::tag] :req [::left ::right])
                    #(= :equals (:tag %))))

(defn bnot= [left right]
  {:tag :not-equals
   :left left
   :right right})
(s/fdef bnot=
        :args (s/cat :left ::left :right ::right)
        :ret (s/and (s/keys :req-un [::tag] :req [::left ::right])
                    #(= :not-equals (:tag %))))
; syntactic sugar
(defn bdistinct? [& elems]
  (apply band (map (fn [[elem1 elem2]] (bnot= elem1 elem2)) (combinations elems 2))))
(s/fdef bdistinct?
        :args (s/cat :elems ::elems)
        :ret (s/and (s/keys :req-un [::tag] :req [::preds])
                    #(= :and (:tag %))))                    ; TODO: concretize spec


;;; logical predicates

(defn band [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :and
     :preds preds}))
(s/fdef band
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::preds])
                    #(= :and (:tag %))))

(defn bor [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :or
     :preds preds}))
(s/fdef bor
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::preds])
                    #(= :or (:tag %))))

(defn bimplication [& preds]
  {:tag :implication
   :preds preds})
(s/fdef bimplication
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::preds])
                    #(= :implication (:tag %))))

(defn bequivalence [& preds]
  {:tag :equivalence
   :preds preds})
(s/fdef bequivalence
        :args (s/cat :preds ::preds)
        :ret (s/and (s/keys :req-un [::tag] :req [::preds])
                    #(= :equivalence (:tag %))))

(defn bnot [pred]
  {:tag :not
   :pred pred})
(s/fdef bnot
        :args (s/cat :pred ::pred)
        :ret (s/and (s/keys :req-un [::tag] :req [::pred])
                    #(= :not (:tag %))))

(defn bfor-all
  ([ids implication]
   {:tag :for-all
    :ids (to-vec ids)
    :implication implication})
  ([ids premise conclusion]
   (bfor-all ids (bimplication premise conclusion))))
(s/fdef bfor-all
        :args (s/or :arity-2 (s/cat :ids ::ids :implication ::implication)
                    :arity-3 (s/cat :ids ::ids :premise ::pred :conclusion ::pred))
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::implication])
                    #(= :for-all (:tag %))))

(defn bexists [ids pred]
  {:tag :exists
   :ids (to-vec ids)
   :pred pred})
(s/fdef bexists
        :args (s/cat :ids ::ids :pred ::pred)
        :ret (s/and (s/keys :req-un [::tag] :req [::ids ::pred])
                    #(= :exists (:tag %))))

;;; misc


(defn bset-enum [& elements]
  {:tag :set-enum
   :elements elements})

(defn bmap-set [p s]
  (bran (blambda [:x] (bmember? :x s) (p :x))))


(declare pre-process-lisb lisb->ir)


(defn process-comprehension-set [lisb]
  (if (= 3 (count lisb))
    (let [lisb (remove #(and (symbol? %) (= (name %) "|")) lisb)
          _ (assert (= (count lisb) 2))
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
                        (cond (and (symbol (first op)) (= "<--" (name (first op)))) (let [returns (second op)
                                                                                          operation (last op)
                                                                                          name (first operation)
                                                                                          args (second operation)
                                                                                          body (pre-process-lisb (last operation))]
                                                                                      (conj acc (list 'op returns name args body)))
                              (keyword? (first op)) (conj acc (cons 'op (pre-process-lisb op)))
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
    (and (vector? lisb) (<= 3 (count lisb)) (apply = "->" (map #(when (symbol? %) (name %)) (take-nth 2 (rest lisb)))))
         ;; catch all tuples in the form of [1 -> 2 -> 3 -> ...]
         (let [elems (take-nth 2 lisb)]
           ;(list* lisb.translation.lisb2ir/bmaplet (map pre-process-lisb elems))
           (->Tuple (map pre-process-lisb elems))
           )
    (and (set? lisb) (some #(and (symbol? %) (= "|" (name %))) lisb)) (process-comprehension-set lisb)
    (and (seq? lisb) (symbol? (first lisb)) (= "sets" (name (first lisb)))) (process-set-definitions lisb)
    (and (seq? lisb) (symbol? (first lisb)) (= "operations" (name (first lisb)))) (process-op-definitions lisb)
    (and (seq? lisb) (symbol? (first lisb)) (= "<--" (name (first lisb)))) (process-assign-returns lisb)
    (and (seq? lisb) (symbol? (first lisb)) (= "if" (name (first lisb)))) (pre-process-lisb (list* lisb.translation.lisb2ir/bif (rest lisb)))
    (set? lisb) `(hash-set ~@(map pre-process-lisb lisb)) ; make sure we do not generate code that crashes when there are duplicate elements
    (seqable? lisb) (walk pre-process-lisb identity lisb)
    :else lisb))


(def dsl-sugar {; parse units
                "machine" 'lisb.translation.lisb2ir/bmachine
                "model" 'lisb.translation.lisb2ir/bmodel
                "system" 'lisb.translation.lisb2ir/bsystem
                "refinement" 'lisb.translation.lisb2ir/brefinement
                "implementation" 'lisb.translation.lisb2ir/bimplementation

                ; machine clauses
                ; machine inclusions
                "uses" 'lisb.translation.lisb2ir/buses
                "includes" 'lisb.translation.lisb2ir/bincludes
                "sees" 'lisb.translation.lisb2ir/bsees
                "extends" 'lisb.translation.lisb2ir/bextends
                "promotes" 'lisb.translation.lisb2ir/bpromotes

                ; machine sections
                "constraints" 'lisb.translation.lisb2ir/bconstraints
                "sets" 'lisb.translation.lisb2ir/bsets
                "deferred-set" 'lisb.translation.lisb2ir/bdeferred-set
                "enumerated-set" 'lisb.translation.lisb2ir/benumerated-set
                "constants" 'lisb.translation.lisb2ir/bconstants
                "abstract-constants" 'lisb.translation.lisb2ir/babstract-constants
                "properties" 'lisb.translation.lisb2ir/bproperties
                "definitions" 'lisb.translation.lisb2ir/bdefinitions
                "expression-definition" 'lisb.translation.lisb2ir/bexpression-definition
                "predicate-definition" 'lisb.translation.lisb2ir/bpredicate-definition
                "substitution-definition" 'lisb.translation.lisb2ir/bsubstitution-definition
                "file-definition" 'lisb.translation.lisb2ir/bfile-definition
                "freetypes" 'lisb.translation.lisb2ir/bfreetypes
                "freetype" 'lisb.translation.lisb2ir/bfreetype
                "constructor" 'lisb.translation.lisb2ir/bconstructor
                "variables" 'lisb.translation.lisb2ir/bvariables
                "invariants" 'lisb.translation.lisb2ir/binvariants
                "assertions" 'lisb.translation.lisb2ir/bassertions
                "init" 'lisb.translation.lisb2ir/binit
                "operations" 'lisb.translation.lisb2ir/boperations
                "op" 'lisb.translation.lisb2ir/bop

                ; substitutions
                "skip" 'lisb.translation.lisb2ir/bskip
                "block" 'lisb.translation.lisb2ir/bblock
                "assign" 'lisb.translation.lisb2ir/bassign
                ; 'set! 'lisb.translation.lisb2ir/bassign                                   ; sugar
                "becomes-element-of" 'lisb.translation.lisb2ir/bbecomes-element-of
                "becomes-member" 'lisb.translation.lisb2ir/bbecomes-element-of             ; sugar
                "becomes-such" 'lisb.translation.lisb2ir/bbecomes-such
                "op-call" 'lisb.translation.lisb2ir/bop-call
                "op-call-with-returns" 'lisb.translation.lisb2ir/bop-call-with-returns
                "parallel-sub" 'lisb.translation.lisb2ir/bparallel-sub
                "||" 'lisb.translation.lisb2ir/bparallel-sub                               ; sugar
                "sequential-sub" 'lisb.translation.lisb2ir/bsequential-sub
                "any" 'lisb.translation.lisb2ir/bany
                "let-sub" 'lisb.translation.lisb2ir/blet-sub
                "var-sub" 'lisb.translation.lisb2ir/bvar
                "pre" 'lisb.translation.lisb2ir/bprecondition
                "assert" 'lisb.translation.lisb2ir/bassert
                "choice" 'lisb.translation.lisb2ir/bchoice
                "if-sub" 'lisb.translation.lisb2ir/bif-sub
                "cond" 'lisb.translation.lisb2ir/bcond
                "select" 'lisb.translation.lisb2ir/bselect
                "case" 'lisb.translation.lisb2ir/bcase

                ; if
                "if" 'lisb.translation.lisb2ir/bif ; this does not work 'lisb.translation.lisb2ir/because "if" is a special form, handled in pre-process-lisb
                "if-expr" 'lisb.translation.lisb2ir/bif
                "if-pred" 'lisb.translation.lisb2ir/bif

                ; let
                "let" 'lisb.translation.lisb2ir/blet ; this DOES work 'lisb.translation.lisb2ir/because "let*" is the special form, "let" is just a macro
                "let-expr" 'lisb.translation.lisb2ir/blet
                "let-pred" 'lisb.translation.lisb2ir/blet

                ; trees
                ;; missing

                ; reals
                "real" 'lisb.translation.lisb2ir/bto-real
                "real-set" 'lisb.translation.lisb2ir/breal-set
                "floor" 'lisb.translation.lisb2ir/bfloor
                "ceil" 'lisb.translation.lisb2ir/bceil

                ; strings
                "string-set" 'lisb.translation.lisb2ir/bstring-set

                ; structs
                "struct" 'lisb.translation.lisb2ir/bstruct
                "record" 'lisb.translation.lisb2ir/brecord
                "record-get" 'lisb.translation.lisb2ir/brecord-get

                ; sequences
                "sequence" 'lisb.translation.lisb2ir/bsequence
                "seq" 'lisb.translation.lisb2ir/bseq
                "seq1" 'lisb.translation.lisb2ir/bseq1
                "iseq" 'lisb.translation.lisb2ir/biseq
                "iseq1" 'lisb.translation.lisb2ir/biseq1
                "perm" 'lisb.translation.lisb2ir/bperm
                "size" 'lisb.translation.lisb2ir/bsize
                "concat" 'lisb.translation.lisb2ir/bconcat
                "prepend" 'lisb.translation.lisb2ir/bprepend
                "->" 'lisb.translation.lisb2ir/bprepend                                    ; sugar
                "append" 'lisb.translation.lisb2ir/bappend
                "<-" 'lisb.translation.lisb2ir/bappend                                     ; sugar
                "reverse" 'lisb.translation.lisb2ir/breverse
                "first" 'lisb.translation.lisb2ir/bfirst
                "last" 'lisb.translation.lisb2ir/blast
                "front" 'lisb.translation.lisb2ir/bfront
                "drop-last" 'lisb.translation.lisb2ir/bfront                               ; clojure
                "tail" 'lisb.translation.lisb2ir/btail
                "rest" 'lisb.translation.lisb2ir/btail                                     ; clojure
                "conc" 'lisb.translation.lisb2ir/bconc
                "take" 'lisb.translation.lisb2ir/btake
                "drop" 'lisb.translation.lisb2ir/bdrop

                 ; functions
                "partial-function" 'lisb.translation.lisb2ir/bpartial-function
                "+->" 'lisb.translation.lisb2ir/bpartial-function                          ; sugar
                "total-function" 'lisb.translation.lisb2ir/btotal-function
                "-->" 'lisb.translation.lisb2ir/btotal-function
                "partial-surjection" 'lisb.translation.lisb2ir/bpartial-surjection
                "+->>" 'lisb.translation.lisb2ir/bpartial-surjection
                "total-surjection" 'lisb.translation.lisb2ir/btotal-surjection
                "-->>" 'lisb.translation.lisb2ir/btotal-surjection
                "partial-injection" 'lisb.translation.lisb2ir/bpartial-injection
                ">+>" 'lisb.translation.lisb2ir/bpartial-injection
                "total-injection" 'lisb.translation.lisb2ir/btotal-injection
                ">->" 'lisb.translation.lisb2ir/btotal-injection
                "partial-bijection" 'lisb.translation.lisb2ir/bpartial-bijection
                ">+>>" 'lisb.translation.lisb2ir/bpartial-bijection
                "total-bijection" 'lisb.translation.lisb2ir/btotal-bijection
                ">->>" 'lisb.translation.lisb2ir/btotal-bijection
                "lambda" 'lisb.translation.lisb2ir/blambda
                ; "fn" 'lisb.translation.lisb2ir/bfn  ; sugar
                "fn-call" 'lisb.translation.lisb2ir/bfn-call


                ; relations
                "relation" 'lisb.translation.lisb2ir/brelation
                "<->" 'lisb.translation.lisb2ir/brelation                                  ; sugar
                "total-relation" 'lisb.translation.lisb2ir/btotal-relation
                "<<->" 'lisb.translation.lisb2ir/btotal-relation                           ; sugar
                "surjective-relation" 'lisb.translation.lisb2ir/bsurjective-relation
                "<->>" 'lisb.translation.lisb2ir/bsurjective-relation                      ; sugar
                "total-surjective-relation" 'lisb.translation.lisb2ir/btotal-surjective-relation
                "<<->>" 'lisb.translation.lisb2ir/btotal-surjective-relation               ; sugar
                "maplet" 'lisb.translation.lisb2ir/bmaplet
                "|->" 'lisb.translation.lisb2ir/bmaplet                                    ; sugar
                "dom" 'lisb.translation.lisb2ir/bdom
                "ran" 'lisb.translation.lisb2ir/bran
                "id" 'lisb.translation.lisb2ir/bid
                "domain-restriction" 'lisb.translation.lisb2ir/bdomain-restriction
                "<|" 'lisb.translation.lisb2ir/bdomain-restriction                         ; sugar
                "domain-subtraction" 'lisb.translation.lisb2ir/bdomain-subtraction
                "<<|" 'lisb.translation.lisb2ir/bdomain-subtraction                        ; sugar
                "range-restriction" 'lisb.translation.lisb2ir/brange-restriction
                "|>" 'lisb.translation.lisb2ir/brange-restriction                          ; sugar
                "range-subtraction" 'lisb.translation.lisb2ir/brange-subtraction
                "|>>" 'lisb.translation.lisb2ir/brange-subtraction                         ; sugar
                "inverse" 'lisb.translation.lisb2ir/binverse
                "image" 'lisb.translation.lisb2ir/bimage
                "override" 'lisb.translation.lisb2ir/boverride
                "<+" 'lisb.translation.lisb2ir/boverride                                   ; sugar
                "direct-product" 'lisb.translation.lisb2ir/bdirect-product
                "><" 'lisb.translation.lisb2ir/bdirect-product                             ; sugar
                "composition" 'lisb.translation.lisb2ir/bcomposition
                "parallel-product" 'lisb.translation.lisb2ir/bparallel-product
                "prj1" 'lisb.translation.lisb2ir/bprj1
                "prj2" 'lisb.translation.lisb2ir/bprj2
                "eventb-prj1" 'lisb.translation.lisb2ir/beventb-prj1
                "eventb-prj2" 'lisb.translation.lisb2ir/beventb-prj2
                "closure1" 'lisb.translation.lisb2ir/bclosure1
                "closure" 'lisb.translation.lisb2ir/bclosure
                "iterate" 'lisb.translation.lisb2ir/biterate
                "fnc" 'lisb.translation.lisb2ir/bfnc
                "rel" 'lisb.translation.lisb2ir/brel


                ; numbers
                "integer-set" 'lisb.translation.lisb2ir/binteger-set
                "natural-set" 'lisb.translation.lisb2ir/bnatural-set
                "natural1-set" 'lisb.translation.lisb2ir/bnatural1-set
                "int-set" 'lisb.translation.lisb2ir/bint-set
                "nat-set" 'lisb.translation.lisb2ir/bnat-set
                "nat1-set" 'lisb.translation.lisb2ir/bnat1-set
                "interval" 'lisb.translation.lisb2ir/binterval
                "range" 'lisb.translation.lisb2ir/brange
                "min-int" 'lisb.translation.lisb2ir/bmin-int
                "max-int" 'lisb.translation.lisb2ir/bmax-int
                ">" 'lisb.translation.lisb2ir/b>
                "<" 'lisb.translation.lisb2ir/b<
                ">=" 'lisb.translation.lisb2ir/b>=
                "<=" 'lisb.translation.lisb2ir/b<=
                "max" 'lisb.translation.lisb2ir/bmax
                "min" 'lisb.translation.lisb2ir/bmin
                "+" 'lisb.translation.lisb2ir/b+
                "-" 'lisb.translation.lisb2ir/b-
                "cart-or-mult" 'lisb.translation.lisb2ir/bcart-or-mult
                "*" 'lisb.translation.lisb2ir/b*                                           ; added separat multiplication
                "div" 'lisb.translation.lisb2ir/bdiv
                "/" 'lisb.translation.lisb2ir/bdiv
                "**" 'lisb.translation.lisb2ir/b**
                "mod" 'lisb.translation.lisb2ir/bmod
                "pi" 'lisb.translation.lisb2ir/bpi
                "π" 'lisb.translation.lisb2ir/bpi                                          ; sugar
                "sigma" 'lisb.translation.lisb2ir/bsigma
                "Σ" 'lisb.translation.lisb2ir/bsigma                                       ; sugar
                "successor" 'lisb.translation.lisb2ir/bsuccessor
                "inc" 'lisb.translation.lisb2ir/bsuccessor                                 ; sugar
                "predecessor" 'lisb.translation.lisb2ir/bpredecessor
                "dec" 'lisb.translation.lisb2ir/bpredecessor                               ; sugar

                ; sets
                "comprehension-set" 'lisb.translation.lisb2ir/bcomprehension-set
                "pow" 'lisb.translation.lisb2ir/bpow
                "pow1" 'lisb.translation.lisb2ir/bpow1
                "fin" 'lisb.translation.lisb2ir/bfin
                "fin1" 'lisb.translation.lisb2ir/bfin1
                "card" 'lisb.translation.lisb2ir/bcard
                "cartesian-product" 'lisb.translation.lisb2ir/bcartesian-product           ; added separat cartesian-product
                "union" 'lisb.translation.lisb2ir/bunion
                "intersection" 'lisb.translation.lisb2ir/bintersection
                "set-" 'lisb.translation.lisb2ir/bset-
                "member?" 'lisb.translation.lisb2ir/bmember?
                "in" 'lisb.translation.lisb2ir/bmember?                                    ; sugar
                "contains?" 'lisb.translation.lisb2ir/bcontains?                           ; sugar
                "subset?" 'lisb.translation.lisb2ir/bsubset?
                "strict-subset?" 'lisb.translation.lisb2ir/bstrict-subset?
                "superset?" 'lisb.translation.lisb2ir/bsuperset?                           ; sugar
                "strict-superset?" 'lisb.translation.lisb2ir/bstrict-superset?             ; sugar
                "unite-sets" 'lisb.translation.lisb2ir/bunite-sets
                "intersect-sets" 'lisb.translation.lisb2ir/bintersect-sets
                "union-pe" 'lisb.translation.lisb2ir/bunion-pe
                "intersection-pe" 'lisb.translation.lisb2ir/bintersection-pe

                ; bools
                "bool-set" 'lisb.translation.lisb2ir/bbool-set
                "pred->bool" 'lisb.translation.lisb2ir/bpred->bool

                ; equality
                "=" 'lisb.translation.lisb2ir/b=
                "not=" 'lisb.translation.lisb2ir/bnot=
                "distinct?" 'lisb.translation.lisb2ir/bdistinct?                           ; added funcionality

                ; predicates
                "and" 'lisb.translation.lisb2ir/band
                "or" 'lisb.translation.lisb2ir/bor
                "equivalence" 'lisb.translation.lisb2ir/bequivalence
                "<=>" 'lisb.translation.lisb2ir/bequivalence                               ; sugar
                "implication" 'lisb.translation.lisb2ir/bimplication
                "=>" 'lisb.translation.lisb2ir/bimplication                                ; sugar
                "not" 'lisb.translation.lisb2ir/bnot
                "for-all" 'lisb.translation.lisb2ir/bfor-all
                "exists" 'lisb.translation.lisb2ir/bexists  

                ;'| :lisb-internal/any-value
                ;'<-- 'lisb.translation.lisb2ir/bop-call-with-returns
                })

(defmacro b 
  [lisb]
  (let [pre-processed-lisb (pre-process-lisb lisb)]
    `(let  
        [~@(apply concat (for [[k v] dsl-sugar] [(symbol k) v]))]
       ~pre-processed-lisb)))

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
    :else node))


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


(defmacro defpred 
  [name & args]
  `(def ~name (pred ~name ~@args)))


(defn bexpand [code]
  (postwalk
   (fn [form]
     (cond (symbol? form) (get dsl-sugar (name form) form)
           (instance? lisb.translation.types.Tuple form) (->Tuple (map bexpand form))
           :other form))
     code))

(defn beval 
  "Similar to eval, but also evaluates all elements in a tuple to the IR.
  Maps will not be evaluated."
  [code]
  (eval (clojure.walk/postwalk 
    (fn [x] (cond (instance? lisb.translation.types.Tuple x)
                    (->Tuple (map beval x))
                  (map? x)
                    `'~x
                  :otherwise x))
    code)))

(defn bb [code]
  (beval (bexpand (pre-process-lisb code))))

(comment (bb `(= ~(= 1 2) 3))
         (bexpand (pre-process-lisb `[1 -> (+ 1 1)])) 
         (bb `[1 -> [(+ 1 1) -> (+ 1 1)]]) 
         (bb `#{[:x] | :x + 1 = 0})
         (bb `#{[:x] | (= 0 (+ :x 1))})
         (b #{[:x] | (= 0 (+ :x 1))})
         (pre-process-lisb `#{[:x] | :x})
         (pre-process-lisb2 `[2 -> [3 -> 4]])
         (b (+ 1 2))
         )

