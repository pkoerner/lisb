(ns lisb.translation.lisb2ir
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:require [clojure.walk :refer [walk]]))


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

(defn bmachine [machine-name & machine-clauses]
  (process-machine-name
    {:tag :machine
     :machine-clauses machine-clauses}
    machine-name))

(defn bmodel [machine-name & machine-clauses]
  (process-machine-name
    {:tag :model
     :machine-clauses   machine-clauses}
    machine-name))

(defn bsystem [machine-name & machine-clauses]
  (process-machine-name
    {:tag :system
     :machine-clauses machine-clauses}
    machine-name))

(defn brefinement [machine-name abstract-machine-name & machine-clauses]
  (process-machine-name
    {:tag :refinement
     :abstract-machine-name abstract-machine-name
     :machine-clauses machine-clauses}
    machine-name))

(defn bimplementation [machine-name abstract-machine-name & machine-clauses]
  (process-machine-name
    {:tag :implementation
     :abstract-machine-name abstract-machine-name
     :machine-clauses machine-clauses}
    machine-name))


;;; machine clauses
;; machine inclusions

(defn process-machine-reference [machine-reference]
  (process-machine-name {:tag :machine-reference} machine-reference))

(defn buses [& machine-names]
  {:tag :uses
   :values machine-names})

(defn bincludes [& machine-references]
  {:tag :includes
   :values (map process-machine-reference machine-references)})

(defn bsees [& machine-names]
  {:tag :sees
   :values machine-names})

(defn bextends [& machine-references]
  {:tag    :extends
   :values (map process-machine-reference machine-references)})

(defn bpromotes [& ops]
  {:tag :promotes
   :values ops})

;; machine sections

(defn bconstraints [& preds]
  {:tag :contraints
   :values preds})

(defn bsets [& set-defs]
  {:tag :sets
   :values set-defs})
(defn bdeferred-set [id]
  {:tag :deferred-set
   :id id})
(defn benumerated-set [id & elems]
  {:tag :enumerated-set
   :id id
   :elems elems})

(defn bconstants [& ids]
  {:tag :constants
   :values ids})

(defn bproperties [& preds]
  {:tag :properties
   :values preds})

(defn bdefinitions [& defs]
  {:tag :definitions
   :values defs})

(defn bvariables [& ids]
  {:tag :variables
   :values ids})

(defn binvariants [& preds]
  {:tag :invariants
   :values preds})

(defn bassertions [& preds]
  {:tag :assertions
   :values preds})

(defn binit [& subs]
  {:tag :init
   :values subs})

(defn boperations [& op-defs]
  {:tag :operations
   :values op-defs})
(defn bop
  ([returns name args body]
   {:tag         :op
    :returns returns
    :name        name
    :args        args
    :body        body})
  ([name args body]
   (bop [] name args body)))


;;; substitutions

(def bskip {:tag :skip})

(defn bblock [sub]
  {:tag :block
   :sub sub})

(defn bassign [& id-vals]
  {:tag :assignment
   :id-vals id-vals})

(defn bbecomes-element-of [ids set]
  {:tag :becomes-element-of
   :ids ids
   :set set})

(defn bbecomes-such [ids pred]
  {:tag :becomes-such
   :ids ids
   :pred pred})

(defn bop-call
  [op & args]
  {:tag     :op-call
   :op      op
   :args    args})

(defn bop-call-with-returns
  [returns op & args]
  {:tag     :op-call
   :returns returns
   :op      op
   :args    args})

(defn bparallel-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :parallel-sub
     :subs subs}))

(defn bsequential-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :sequential-sub
     :subs subs}))

(defn bany [ids pred & subs]
  {:tag :any
   :ids ids
   :pred pred
   :subs subs})

(defn blet-sub [id-vals & subs]
  {:tag :let-sub
   :id-vals id-vals
   :subs subs})

(defn bvar [ids & subs]
  {:tag :var
   :ids ids
   :subs subs})

(defn bprecondition [pred & subs]
  {:tag :precondition
   :pred pred
   :subs subs})

(defn bassert [pred & subs]
  {:tag :assert
   :pred pred
   :subs subs})

(defn bchoice [& subs]
  {:tag :choice
   :subs subs})

(defn bif-sub
  ([cond then] (bif {:tag :if-sub} cond then))
  ([cond then else] (bif {:tag :if-sub} cond then else)))
(defn bcond [& clauses]
    {:tag :cond
     :clauses clauses})

(defn bselect [& clauses]
  {:tag :select
   :clauses clauses})

(defn bcase [expr & cases]
  {:tag :case
   :expr expr
   :cases cases})


;;; if

(defn bif-expr
  ([cond then else] (bif {:tag :if-expr} cond then else))) ; else is always present


;;; let

(defn blet [id-vals expr-or-pred]
  {:tag :let
   :id-vals id-vals
   :expr-or-pred expr-or-pred})

;;; trees


;;; reals - (alpha - besser nicht verwenden)


;;; strings

(def bstring-set
  {:tag :string-set})


;;; records

(defn bstruct [& id-types]
  {:tag :struct
   :id-types id-types})

(defn brecord [& id-vals]
  {:tag :record
   :id-vals id-vals})

(defn brecord-get [rec id]
  {:tag :record-get
   :rec rec
   :id id})


;;; sequences

(defn bsequence
  ([] {:tag :empty-sequence})
  ([& elems]
   {:tag    :sequence
    :elems elems}))

(defn bseq [set]
  {:tag :seq
   :set set})

(defn bseq1 [set]
  {:tag :seq1
   :set set})

(defn biseq [set]
  {:tag :iseq
   :set set})

(defn biseq1 [set]
  {:tag :iseq1
   :set set})

(defn bperm [set]
  {:tag :perm
   :set set})

(defn bsize [seq]
  {:tag :size
   :seq seq})

(defn bconcat [& seqs]
  {:tag :concat
   :seqs seqs})

(defn bprepend [elem seq]
  {:tag :prepend
   :elem elem
   :seq seq})

(defn bappend [seq & elems]
  {:tag :append
   :seq seq
   :elems elems})

(defn breverse [seq]
  {:tag :reverse
   :seq seq})

(defn bfirst [seq]
  {:tag :first
   :seq seq})

(defn blast [seq]
  {:tag :last
   :seq seq})

(defn bfront [seq]
  {:tag :front
   :seq seq})

(defn btail [seq]
  {:tag :tail
   :seq seq})

(defn bconc [seq-of-seqs]
  {:tag :conc
   :seq-of-seqs seq-of-seqs})

(defn btake [num seq]
  {:tag :take
   :seq seq
   :num num})

(defn bdrop [num seq]
  {:tag :drop
   :seq seq
   :num num})


;;; functions

(defn bpartial-function [& sets]
  {:tag :partial-fn
   :sets sets})

(defn btotal-function [& sets]
  {:tag :total-fn
   :sets sets})

(defn bpartial-surjection [& sets]
  {:tag :partial-surjection
   :sets sets})

(defn btotal-surjection [& sets]
  {:tag :total-surjection
   :sets sets})

(defn bpartial-injection [& sets]
  {:tag :partial-injection
   :sets sets})

(defn btotal-injection [& sets]
  {:tag :total-injection
   :sets sets})

(defn bpartial-bijection [& sets]
  {:tag :partial-bijection
   :sets sets})

(defn btotal-bijection [& sets]
  {:tag :total-bijection
   :sets sets})

(defn blambda [ids pred expr]
  {:tag :lambda
   :ids ids
   :pred pred
   :expr expr})
#_(defn bfn [id-types expr]
  {:tag :fn
   :id-types id-types
   :expr expr})

(defn bfn-call [f & args]
  {:tag :fn-call
   :f f
   :args args})


;;; relations

(defn brelation [& sets]
  {:tag :relation
   :sets sets})

(defn btotal-relation [& sets]
  {:tag :total-relation
   :sets sets})

(defn bsurjective-relation [& sets]
  {:tag :surjective-realtion
   :sets sets})

(defn btotal-surjective-relation [& sets]
  {:tag :total-surjective-relation
   :sets sets})

(defn bmaplet [left right]
  {:tag :maplet
   :left left
   :right right})

(defn bdom [rel]
  {:tag :dom
   :rel rel})

(defn bran [rel]
  {:tag :ran
   :rel rel})

(defn bid [set]
  {:tag :id
   :set set})

(defn bdomain-restriction [set rel]
  {:tag :domain-restriction
   :set set
   :rel rel})

(defn bdomain-subtraction [set rel]
  {:tag :domain-subtraction
   :set set
   :rel rel})

(defn brange-restriction [rel set]
  {:tag :range-restriction
   :rel rel
   :set set})

(defn brange-subtraction [rel set]
  {:tag :range-subtraction
   :rel rel
   :set set})

(defn binverse [rel]
  {:tag :inverse
   :rel rel})

(defn bimage [rel set]
  {:tag :image
   :rel rel
   :set set})

(defn boverride [& rels]
  {:tag :override
   :rels rels})

(defn bdirect-product [& rels]
  {:tag :direct-product
   :rels rels})

(defn bcomposition [& rels]
  {:tag :composition
   :rels rels})

(defn bparallel-product [& rels]
  {:tag :parallel-product
   :rels rels})

(defn bprj1 [set1 set2]
  {:tag :prj1
   :set1 set1
   :set2 set2})

(defn bprj2 [set1 set2]
  {:tag :prj2
   :set1 set1
   :set2 set2})

(defn bclosure1 [rel]
  {:tag :closure1
   :rel rel})

(defn bclosure [rel]
  {:tag :closure
   :rel rel})

(defn biterate [rel num]
  {:tag :iterate
   :rel rel
   :num num})

(defn bfnc [rel]
  {:tag :fnc
   :rel rel})

(defn brel [rel]
  {:tag :rel
   :rel rel})


;;; numbers

(def binteger-set {:tag :integer-set})

(def bnatural-set {:tag :natural-set})

(def bnatural1-set {:tag :natural1-set})

(def bint-set {:tag :int-set})

(def bnat-set {:tag :nat-set})

(def bnat1-set {:tag :nat1-set})

(defn binterval [from to]
  {:tag :interval
   :from from
   :to to})

(defn brange [from to]
  (binterval from (bpredecessor to)))

(def bmin-int
  {:tag :min-int})

(def bmax-int
  {:tag :max-int})

(defn b> [& nums]
  {:tag :greater
   :nums nums})

(defn b< [& nums]
  {:tag :less
   :nums nums})

(defn b>= [& nums]
  {:tag :greater-equals
   :nums nums})

(defn b<= [& nums]
  {:tag :less-equals
   :nums nums})

(defn bmax
  ([set]
   {:tag :max, :set set})
  ; adds enumeration to max
  ([num & nums]
   (let [nums (into #{num} nums)]
     (bmax nums))))

(defn bmin
  ([set]
   {:tag :min, :set set})
  ; adds enumeration to min
  ([num & nums]
   (let [nums (into #{num} nums)]
     (bmin nums))))

(defn b+ [& nums]
  {:tag :add
   :nums nums})

(defn b- [& nums]
  (if (= 1 (count nums))
    {:tag :unary-minus, :num (first nums)}
    {:tag :sub, :nums nums}))

(defn bcart-or-mult [& nums-or-sets]
  {:tag :cartesian-product-or-multiplication
   :nums-or-sets nums-or-sets})

(defn b* [& nums]
  {:tag :mul
   :nums nums})

(defn bdiv [& nums]
  {:tag :div
   :nums nums})

(defn b** [& nums]
  {:tag :pow
   :nums nums})

(defn bmod [& nums]
  {:tag :mod
   :nums nums})

(defn bpi [ids pred expr]
  {:tag :pi
   :ids ids
   :pred pred
   :expr expr})

(defn bsigma [ids pred expr]
  {:tag :sigma
   :ids ids
   :pred pred
   :expr expr})

(defn bsuccessor [num]
  {:tag :successor
   :num num})

(defn bpredecessor [num]
  {:tag :predecessor
   :num num})


;;; sets

(defn bcomprehension-set [ids pred]
  {:tag  :comprehension-set
   :ids  (to-vec ids)
   :pred pred})

(defn bpow [set]
  {:tag :power-set
   :set set})

(defn bpow1 [set]
  {:tag :power1-set
   :set set})

(defn bfin [set]
  {:tag :fin
   :set set})

(defn bfin1 [set]
  {:tag :fin1
   :set set})

(defn bcard [set]
  {:tag :cardinality
   :set set})

(defn bcartesian-product [& sets]
  {:tag :cartesian-product
   :sets sets})

(defn bunion [& sets]
  {:tag :union
   :sets sets})

(defn bintersection [& sets]
  {:tag :intersection
   :sets sets})

(defn bset- [& sets]
  {:tag :difference
   :sets sets})

(defn bmember? [elem set]
  {:tag :member
   :elem elem
   :set set})

(defn bcontains? [set & elems]
  (apply band (reduce
                (fn [res elem]
                  (conj res (bmember? elem set)))
                []
                elems)))

(defn bsubset? [& sets]
  {:tag :subset
   :sets sets})

(defn bstrict-subset? [& sets]
  {:tag :strict-subset
   :sets sets})

; adds superset to lisb
(defn bsuperset? [& sets]
  (apply bsubset? (reverse sets)))

; adds superset-strict to lisb
(defn bstrict-superset? [& sets]
  (apply bstrict-subset? (reverse sets)))

(defn bunite-sets [set-of-sets]
  {:tag :unite-sets
   :set-of-sets set-of-sets})

(defn bintersect-sets [set-of-sets]
  {:tag :intersect-sets
   :set-of-sets set-of-sets})

(defn bunion-pe [ids pred expr]
  {:tag :union-pe
   :ids ids
   :pred pred
   :expr expr})

(defn bintersection-pe [ids pred expr]
  {:tag :intersection-pe
   :ids ids
   :pred pred
   :expr expr})


;;; booleans

(def bbool-set {:tag :bool-set})

(defn bpred->bool [pred]
  {:tag :pred->bool
   :pred pred})


;;; equality predicates

(defn b= [left right]
  {:tag :equals
   :left left
   :right right})

(defn bnot= [left right]
  {:tag :not-equals
   :left left
   :right right})
; syntactic sugar
(defn bdistinct? [& elems]
  (apply band (map (fn [[elem1 elem2]] (bnot= elem1 elem2)) (combinations elems 2))))


;;; logical predicates

; TODO: Überprüfung mit precondition map oder mit spec
(defn band [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :and
     :preds preds}))

(defn bor [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :or
     :preds preds}))

(defn bimplication [& preds]
  {:tag :implication
   :preds preds})

(defn bequivalence [& preds]
  {:tag :equivalence
   :preds preds})

(defn bnot [pred]
  {:tag :not
   :pred pred})

(defn bfor-all
  ([ids implication]
   {:tag :for-all
    :ids ids
    :implication implication})
  ([ids premise conclusion]
   (bfor-all ids (bimplication premise conclusion))))

(defn bexists [ids pred]
  {:tag :exists
   :ids ids
   :pred pred})

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
