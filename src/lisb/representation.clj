(ns lisb.representation)


(declare b=)
(declare band)
(declare bnot)
(declare bdec)

(defn bif
  ([node condition then] (assoc node :condition condition :then then))
  ([node condition then else]
   (if else
     (assoc node :condition condition :then then :else else)
     (bif node condition then))))

(defn blet [node kvs]
  (let [kv-pairs (partition 2 kvs)
        identifiers (map first kv-pairs)
        assignment (reduce band (map (partial apply b=) kv-pairs))]
    (assoc node :identifiers identifiers :assignment assignment)))


;;; parse units

(defn bmachine [variant header & clauses]
  {:tag :machine
   :variant variant
   :header header
   :clauses clauses})
(defn bmachine-variant []
  {:tag :machine-variant})
(defn bmachine-header [name parameters]
  {:tag :machine-header
   :name name
   :parameters parameters})


;;; machine clauses

(defn bconstraints [predicate]
  {:tag :contraints
   :predicate predicate})

(defn bsets [& set-definitions]
  {:tag :sets
   :set-definitions set-definitions})
(defn bdeferred-set [identifier]
  {:tag :deferred-set
   :identifier identifier})
(defn benumerated-set [identifier & elements]
  {:tag :enumerated-set
   :identifier identifier
   :elements elements})

(defn bconstants [& identifiers]
  {:tag :constants
   :identifiers identifiers})

(defn bproperties [predicate]
  {:tag :properties
   :predicate predicate})

(defn bdefinitions [& definitions]
  {:tag :definitions
   :definitions definitions})

(defn bvariables [& identifiers]
  {:tag :variables
   :identifiers identifiers})

(defn binvariant [predicate]
  {:tag :invariants
   :predicate predicate})

(defn bassertions [& predicates]
  {:tag :assertions
   :predicates predicates})

(defn binit [substitution]
  {:tag :init
   :substitution substitution})

(defn boperations [& operations]
  {:tag :operations
   :operations operations})
(defn boperation [return-values name parameters body]
  {:tag :operation
   :return return-values
   :name name
   :parameters parameters
   :body body})


;;; substitutions

(def bskip {:tag :skip})

(defn bblock [substitution]
  {:tag :block
   :substitution substitution})

(defn bassign [& idvals]
  (let [idvals (apply map list (partition 2 idvals))
        identifiers (first idvals)
        values (second idvals)]
    {:tag         :assign
     :identifiers identifiers
     :values        values}))

(defn bbecomes-element-of [identifiers set]
  {:tag :becomes-element-of
   :identifiers identifiers
   :set set})

(defn bbecomes-such [identifiers predicate]
  {:tag :becomes-such
   :identifiers identifiers
   :predicate predicate})

(defn boperation-call [identifiers operation parameters]
  {:tag :operation-call
   :identifiers identifiers
   :operation operation
   :parameters parameters})

(defn bparallel-substitution [& substitutions]
  {:tag :parallel-substitution
   :substitutions substitutions})

(defn bsequence-substitution [& substitutions]
  {:tag :sequence-substitution
   :substitutions substitutions})

(defn bany [identifiers where then]
  {:tag :any
   :identifiers identifiers
   :where where
   :then then})

(defn blet-sub
  ([kvs substitution] (blet {:tag :let-sub :substitution substitution} kvs))
  ([identifiers predicate substitution]
                {:tag          :let-sub
                 :identifiers  identifiers
                 :predicate    predicate
                 :substitution substitution}))

(defn bvar [identifiers substitution]
  {:tag :var
   :identifiers identifiers
   :substitution substitution})

(defn bprecondition [predicate substitution]
  {:tag :precondition
   :predicate predicate
   :substitution substitution})

(defn bassert [predicate substitution]
  {:tag :assert
   :predicate predicate
   :substitution substitution})

(defn bchoice [& substitutions]
  {:tag :choice
   :substitutions substitutions})

(defn bif-sub
  ([predicate then] (bif {:tag :if-sub} predicate then))
  ([predicate then else] (bif {:tag :if-sub} predicate then else)))
(defn bcond [& clauses]
    {:tag :cond
     :clauses clauses})

(defn bselect [& clauses]
  {:tag :select
   :clauses clauses})


;;; if

(defn bif-expr
  ([condition then else] (bif {:tag :if-expr} condition then else))) ; else is always present


;;; let

(defn blet-expr
  ([kvs expression] (blet {:tag :let-expr, :expression expression} kvs))
  ([identifiers assignment expression] {:tag :let-expr, :identifiers identifiers :assignment assignment :expression expression}))

(defn blet-pred
  ([kvs predicate] (blet {:tag :let-pred, :predicate predicate} kvs))
  ([identifiers assignment predicate] {:tag :let-pred, :identifiers identifiers :assignment assignment :predicate predicate}))


;;; trees


;;; reals - (alpha - besser nicht verwenden)


;;; strings

(def bstring-set
  {:tag :string-set})


;;; records

(defn bstruct [& id-types]
  {:tag :struct
   :id-types id-types})

(defn brecord [& id-values]
  {:tag :record
   :id-values id-values})

(defn brec-get [record identifier]
  {:tag :rec-get
   :record record
   :identifier identifier})


;;; sequences

(defn bsequence
  ([] {:tag :empty-sequence})
  ([& args]
   {:tag    :sequence
    :elements args}))

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

(defn bsize [set]
  {:tag :size
   :set set})

(defn bconcat [& seqs]
  {:tag :concat
   :seqs seqs})

(defn bcons [seq & elements]
  {:tag :insert-front
   :seq seq
   :elements elements})

; TODO: Documentation: hinten anfügen
(defn bconj [seq & elements]
  {:tag :insert-tail
   :seq seq
   :elements elements})

(defn breverse [seq]
  {:tag :reverse
   :seq seq})

(defn bfirst [seq]
  {:tag :first
   :seq seq})

(defn blast [seq]
  {:tag :last
   :seq seq})

(defn bdrop-last [seq]
  {:tag :front
   :seq seq})

(defn brest [seq]
  {:tag :tail
   :seq seq})

(defn bconc [seq-of-seqs]
  {:tag :conc
   :seq-of-seqs seq-of-seqs})

(defn btake [n seq]
  {:tag :restrict-front
   :seq seq
   :number n})

(defn bdrop [n seq]
  {:tag :restrict-tail
   :seq seq
   :number n})


;;; functions

(defn b+-> [& sets]
  {:tag :partial-fn
   :sets sets})

(defn b--> [& sets]
  {:tag :total-fn
   :sets sets})

(defn b+->> [& sets]
  {:tag :partial-surjection
   :sets sets})

(defn b-->> [& sets]
  {:tag :total-surjection
   :sets sets})

(defn b>+> [& sets]
  {:tag :partial-injection
   :sets sets})

(defn b>-> [& sets]
  {:tag :total-injection
   :sets sets})

(defn b>+>> [& sets]
  {:tag :partial-bijection
   :sets sets})

(defn b>->> [& sets]
  {:tag :total-bijection
   :sets sets})

(defn blambda [identifiers predicate expression]
  {:tag :lambda
   :identifiers identifiers
   :predicate predicate
   :expression expression})

(defn bcall [f & args]
  {:tag :call
   :f f
   :args args})


;;; relations

(defn b<-> [& sets]
  {:tag :relation
   :sets sets})

(defn btotal-relation [& sets]
  {:tag :total-relation
   :sets sets})

(defn bsurjective-relation [& sets]
  {:tag :surjective-relation
   :sets sets})

(defn btotal-surjective-relation [& sets]
  {:tag :total-surjective-relation
   :sets sets})

(defn bcouple [& elements]
  {:tag :couple
   :elements elements})

(defn bdom [relation]
  {:tag :domain
   :relation relation})

(defn bran [relation]
  {:tag :range
   :relation relation})

(defn bid [set]
  {:tag :identity-relation
   :set set})

(defn b<| [set relation]
  {:tag :domain-restriction
   :set set
   :relation relation})

(defn b<<| [set relation]
  {:tag :domain-subtraction
   :set set
   :relation relation})

(defn b|> [relation set]
  {:tag :range-restriction
   :relation relation
   :set set})

(defn b|>> [relation set]
  {:tag :range-subtraction
   :relation relation
   :set set})

(defn binverse [relation]
  {:tag :inverse-relation
   :relation relation})

(defn bimage [relation set]
  {:tag :relational-image
   :relation relation
   :set set})

(defn b<+ [& relations]
  {:tag :relational-override
   :relations relations})

(defn b>< [& relations]
  {:tag :direct-product
   :relations relations})

(defn bcomp [& relations]
  {:tag :relational-composition
   :relations relations})

(defn b|| [& relations]
  {:tag :parallel-product
   :relations relations})

(defn bprj1 [set1 set2]
  {:tag :prj1
   :set1 set1
   :set2 set2})

(defn bprj2 [set1 set2]
  {:tag :prj2
   :set1 set1
   :set2 set2})

(defn bclosure1 [relation]
  {:tag :closure1
   :relation relation})

(defn bclosure [relation]
  {:tag :closure
   :relation relation})

(defn biterate [relation number]
  {:tag :iterate
   :relation relation
   :number number})

(defn bfnc [relation]
  {:tag :functionise
   :relation relation})

(defn brel [relation]
  {:tag :relationise
   :relation relation})


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
  (let [partial-binterval (partial binterval from)]
    (cond
      (number? to) (partial-binterval (dec to))
      (= :succ (:tag to)) (partial-binterval (:number to))
      :else (partial-binterval (bdec to)))))

(def bmin-int
  {:tag :min-int})

(def bmax-int
  {:tag :max-int})

(defn bmax
  ([set]
   {:tag :max, :set set})
  ; adds enumeration to max
  ([el & r]
   (let [args (conj (set r) el)]
     (bmax args))))

(defn bmin
  ([set]
   {:tag :min, :set set})
  ; adds enumeration to min
  ([el & r]
   (let [args (conj (set r) el)]
     (bmin args))))

(defn b+ [& numbers]
  {:tag :plus
   :numbers numbers})

(defn b- [& numbers]
  (if (= 1 (count numbers))
    {:tag :unary-minus, :number (first numbers)}
    {:tag :minus, :numbers numbers}))

(defn b* [& numbers]
  {:tag :mult-or-cart
   :numbers numbers})

(defn bdiv [& numbers]
  {:tag :div
   :numbers numbers})

(defn b** [& numbers]
  {:tag :pow
   :numbers numbers})

(defn bmod [& numbers]
  {:tag :mod
   :numbers numbers})

(defn bpi [identifiers predicate expression]
  {:tag :pi
   :identifiers identifiers
   :predicate predicate
   :expression expression})

(defn bsigma [identifiers predicate expression]
  {:tag :sigma
   :identifiers identifiers
   :predicate predicate
   :expression expression})

(defn binc [n]
  {:tag :inc
   :number n})

(defn bdec [n]
  {:tag :dec
   :number n})

; number predicates

(defn b< [& numbers]
  {:tag :less
   :numbers numbers})

(defn b> [& numbers]
  {:tag :greater
   :numbers numbers})

(defn b<= [& numbers]
  {:tag :less-eq
   :numbers numbers})

(defn b>= [& numbers]
  {:tag :greater-eq
   :numbers numbers})


;;; sets

(defn bcomp-set [identifiers predicate]
  {:tag :comp-set
   :identifiers identifiers
   :predicate predicate})

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

(defn bcount [set]
  {:tag :card
   :set set})

(defn bunion [& sets]
  {:tag :union
   :sets sets})

(defn bintersection [& sets]
  {:tag :intersection
   :sets sets})

(defn bdifference [& sets]
  {:tag :difference
   :sets sets})

(defn bmember? [element set]
  {:tag :member
   :element element
   :set set})

; TODO: (bcontains-all? set & elements)
(defn bcontains? [set element]
 (bmember? element set))

(defn bsubset? [subset set]
  {:tag :subset
   :subset subset
   :set set})

(defn bsubset-strict? [subset-strict set]
  {:tag :subset-strict
   :subset-strict subset-strict
   :set set})

; adds superset to lisb
(defn bsuperset? [superset set]
  (bsubset? set superset))

; adds superset-strict to lisb
(defn bsuperset-strict? [superset-strict set]
  (bsubset-strict? set superset-strict))

(defn bunite-sets [set-of-sets]
  {:tag :general-union
   :set-of-sets set-of-sets})

(defn bintersect-sets [set-of-sets]
  {:tag :general-intersection
   :set-of-sets set-of-sets})

(defn bunion-pe [identifiers predicate expression]
  {:tag :union-pe
   :identifiers identifiers
   :predicate predicate
   :expression expression})

(defn bintersection-pe [identifiers predicate expression]
  {:tag :intersection-pe
   :identifiers identifiers
   :predicate predicate
   :expression expression})


;;; booleans

(def bbool-set {:tag :bool-set})

(defn bpred->bool [predicate]
  {:tag :pred->bool
   :predicate predicate})


;;; equality predicates

(defn b= [left right]
  {:tag :equal
   :left left
   :right right})

(defn bnot= [left right]
  {:tag :not-equal
   :left left
   :right right})
; syntactic sugar
(defn bdistinct? [& elements]
  {:tag :distinct
   :elements elements})


;;; logical predicates

(defn band [& predicates]
  {:tag :and
   :predicates predicates})

(defn bor [& predicates]
  {:tag :or
   :predicates predicates})

(defn b=> [& predicates]
  {:tag :implication
   :predicates predicates})

(defn b<=> [& predicates]
  {:tag :equivalence
   :predicates predicates})

(defn bnot [predicate]
  {:tag :not
   :predicate predicate})

(defn bfor-all
  ([identifiers implication]
   {:tag         :for-all
    :identifiers identifiers
    :implication implication})
  ;syntactic sugar
  ([identifiers implication-left implication-right]
   (bfor-all identifiers (b=> implication-left implication-right))))


(defn bexists [identifiers predicate]
  {:tag :exists
   :identifiers identifiers
   :predicate predicate})


;;; misc

#_(defn bexpr [s]
  (node :bexpr s))

#_(defn brec-get [r e]
  (node :record-get r e))

#_(defn btuple [l r]
  (node :tuple l r))

(defn bmap-set [p s]
  (bran (blambda [:x] (bmember? :x s) (p :x))))


; TODO: - negations for subset/superset, strict/non-strict










;; TODO: bset, bpow, bpow1, bfin, bfin1,
;;       sets of bools, naturals, ints, nats
(defmacro b [repr]
  `(let [
         ; parse units
         ~'machine bmachine
         ~'machine-variant bmachine-variant
         ~'machine-header bmachine-header

         ; machine clauses
         ~'constraints bconstraints
         ~'sets bsets
         ~'deferred-set bdeferred-set
         ~'enumerated-set benumerated-set
         ~'constants bconstants
         ~'properties bproperties
         ~'definitions bdefinitions
         ~'variables bvariables
         ~'invariant binvariant
         ~'assertions bassertions
         ~'init binit
         ~'operations boperations
         ~'operation boperation

         ; substitutions
         ~'skip bskip
         ~'block bblock
         ~'assign bassign
         ~'becomes-element-of bbecomes-element-of
         ~'becomes-such bbecomes-such
         ~'operation-call boperation-call
         ~'parallel-substitution bparallel-substitution
         ~'sequence-substitution bsequence-substitution
         ~'any bany
         ~'let-sub blet-sub
         ~'var-sub bvar
         ~'pre bprecondition
         ~'assert bassert
         ~'choice bchoice
         ~'if-sub bif-sub
         ~'cond bcond
         ~'select bselect
         ;~'case bcase

         ; if
         ~'if-expr bif-expr

         ; let
         ~'let-expr blet-expr
         ~'let-pred blet-pred

         ; trees

         ; reals - (alpha - besser nicht verwenden)

         ; strings
         ~'string-set bstring-set

         ; records
         ~'struct bstruct
         ~'record brecord
         ~'rec-get brec-get

         ; sequences
         ~'sequence bsequence
         ~'seq bseq
         ~'seq1 bseq1
         ~'iseq biseq
         ~'iseq1 biseq1
         ~'perm bperm
         ~'count-seq bsize
         ~'concat bconcat
         ~'cons bcons
         ~'conj bconj
         ~'prepend bcons
         ~'append bconj
         ~'reverse breverse
         ~'first bfirst
         ~'last blast
         ~'drop-last bdrop-last
         ~'rest brest
         ~'conc bconc
         ~'take btake
         ~'drop bdrop

         ; functions
         ~'+-> b+->
         ~'--> b-->
         ~'+->> b+->>
         ~'-->> b-->>
         ~'>+> b>+>
         ~'>-> b>->
         ~'>+>> b>+>>
         ~'>->> b>->>
         ~'lambda blambda
         ~'call bcall ;TODO: special case keyword :function-name

         ; relations
         ~'<-> b<->
         ~'total-relation btotal-relation
         ~'surjective-relation bsurjective-relation
         ~'total-surjective-relation btotal-surjective-relation
         ~'couple bcouple
         ~'dom bdom
         ~'ran bran
         ~'identity bid
         ~'<| b<|
         ~'<<| b<<|
         ~'|> b|>
         ~'|>> b|>>
         ~'inverse binverse
         ~'image bimage
         ~'<+ b<+
         ~'>< b><
         ~'comp bcomp
         ~'|| b||
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
         ~'< b<
         ~'> b>
         ~'<= b<=
         ~'>= b>=
         ~'max bmax
         ~'min bmin
         ~'+ b+
         ~'- b-
         ~'* b*
         ~'/ bdiv
         ~'** b**
         ~'mod bmod
         ~'sigma bsigma
         ~'pi bpi
         ~'inc binc
         ~'dec bdec

         ;;; sets
         ~'comp-set bcomp-set
         ~'pow bpow
         ~'pow1 bpow1
         ~'fin bfin
         ~'fin1 bfin1
         ~'count bcount
         ~'union bunion
         ~'intersection bintersection
         ~'difference bdifference
         ~'unite-sets bunite-sets
         ~'intersect-sets bintersect-sets
         ~'union-pe bunion-pe
         ~'intersection-pe bintersection-pe
         ~'contains? bcontains?
         ~'member? bmember?
         ~'subset? bsubset?
         ~'subset-strict? bsubset-strict?
         ~'superset? bsuperset?
         ~'superset-strict? bsuperset-strict?

         ;;; booleans
         ~'bool-set bbool-set
         ~'pred->bool bpred->bool

         ;;; equality predicates
         ~'= b=
         ~'not= bnot=
         ; added funcionality
         ~'distinct? bdistinct?

         ;;; logical predicates
         ~'and band
         ~'or bor
         ~'<=> b<=>
         ~'=> b=>
         ~'not bnot
         ~'for-all bfor-all
         ~'exists bexists]
     ~repr
    ))

(def bempty-machine (bmachine
                     (bmachine-variant)
                     (bmachine-header :Empty [])))

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
