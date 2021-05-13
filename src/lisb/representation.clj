(ns lisb.representation)


(defn node [tag & children]
  {:tag tag
   :children (if children children [])})

(defn left-right [tag left right]
  {:tag tag, :left left, :right right})

(defn on-sequence [tag seq]
  {:tag tag, :seq seq})

(defn left-associative [tag [first second & chained-nodes]]
  (reduce
    (partial left-right tag)
    (left-right tag first second)
    chained-nodes))

(declare b=)
(declare band)
(declare bnot)
#_(declare blambda)
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

(defn- bstructy
  ([k m]
   (node k
         (apply node :list (map name (keys m)))
         (apply node :list (vals m))))
  ([k k1 v1 & keyvals]
   (let [m (apply hash-map k1 v1 keyvals)]
     (bstructy k m))))


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
(defn benumerated-set [identifier elements]
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

(defn binvariants [predicate]
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

#_([predicate then & else-ifs]
   (if (= (mod (count else-ifs) 2) 0)
     {:tag :if, :predicate predicate, :then then, :else-ifs else-ifs}
     {:tag :if, :predicate predicate, :then then, :else-ifs (drop-last else-ifs), :else (last else-ifs)}))

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

#_(defn bstr [s]
  (node :string s))

(def bstring-set
  {:tag :string-set})


;;; records

#_(def bstruct (partial bstructy :struct))
(defn bstruct [& id-types]
  (reduce
    (fn [res [id type]]
      (assoc res id type))
    {:tag :struct}
    id-types))

(def brecord (partial bstructy :record))


;;; sequences

(defn bsequence
  ([] {:tag :empty-seq})
  ([& args]
   {:tag    :sequence
    :values args}))

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

(defn bconcat [& args]
  (left-associative :concat args))

(defn b-> [el seq]
  (left-right :prepend el seq))

(defn b<- [seq el]
  (left-right :append seq el))

(defn breverse [seq]
  (on-sequence :reverse seq))

(defn bfirst [seq]
  (on-sequence :first seq))

(defn blast [seq]
  (on-sequence :last seq))

(defn bfront [seq]
  (on-sequence :front seq))

(defn btail [seq]
  (on-sequence :tail seq))

(defn brestrict-front [seq n]
  {:tag :restrict-front
   :seq seq
   :number n})

(defn brestrict-tail [seq n]
  {:tag :restrict-tail
   :seq seq
   :number n})

(defn bconc [seq-of-seqs]
  {:tag :conc
   :seq-of-seqs seq-of-seqs})

;;; functions
; TODO: examine multi arity
(defn b+-> [left right]
  (left-right :partial-fn left right))

; TODO: examine multi arity
(defn b--> [left right]
  (left-right :total-fn left right))

; TODO: examine multi arity
(defn b+->> [left right]
  (left-right :partial-surjection left right))

; TODO: examine multi arity
(defn b-->> [left right]
  (left-right :total-surjection left right))

; TODO: examine multi arity
(defn b>+> [left right]
  (left-right :partial-injection left right))

; TODO: examine multi arity
(defn b>-> [left right]
  (left-right :total-injection left right))

; TODO: examine multi arity
(defn b>+>> [left right]
  (left-right :partial-bijection left right))

; TODO: examine multi arity
(defn b>->> [left right]
  (left-right :total-bijection left right))

(defn blambda [identifiers predicate expression]
  {:tag :lambda
   :identifiers identifiers
   :predicate predicate
   :expression expression})

#_(defn bapply [f & args]
  (apply node :fn-application f args))
(defn bcall [f & args]
  {:tag :call
   :f f
   :args args})


;;; relations

; TODO: examine multi arity
(defn b<-> [left right]
  (left-right :relation left right))

; TODO: examine multi arity
(defn btotal-relation [left right]
  (left-right :total-relation left right))

; TODO: examine multi arity
(defn bsurjective-relation [left right]
  (left-right :surjective-relation left right))

; TODO: examine multi arity
(defn btotal-surjective-relation [left right]
  (left-right :total-surjective-relation left right))

(defn bcouple [& expressions]
  {:tag :couple
   :expressions expressions})

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
  (left-associative :relational-override relations))

(defn b>< [& relations]
  (left-associative :direct-product relations))

(defn bcomp [& relations]
  (left-associative :relational-composition relations))

(defn b|| [& relations]
  (left-associative :parallel-product relations))

(defn bprj1 [set1 set2]
  {:tag :proj1
   :set1 set1
   :set2 set2})

(defn bprj2 [set1 set2]
  {:tag :proj2
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
  {:tag :interval, :from from, :to to})
(defn brange [from to]
  (cond
    (number? to) (binterval from (dec to))
    (= :succ (:tag to)) (binterval from (:number to))
    :else (binterval from (bdec to))))

(def bmin-int
  {:tag :min-int})

(def bmax-int
  {:tag :max-int})

(defn b< [& args]
  (left-associative :less args))

(defn b> [& args]
  (left-associative :greater args))

(defn b<= [& args]
  (left-associative :less-eq args))

(defn b>= [& args]
  (left-associative :greater-eq args))

(defn bmax
  ([set]
   {:tag :max, :set set})
  ([a b & r]                                                ; adds enumeration to max
   (let [args (conj (set r) b a)]
     (bmax args))))

(defn bmin
  ([set]
   {:tag :min, :set set})
  ([a b & r]                                                ; adds enumeration to min
   (let [args (conj (set r) b a)]
     (bmin args))))

(defn b+ [& args]
  (left-associative :plus args))

(defn b- [& args]
  (if (= 1 (count args))
    {:tag :unary-minus, :value (first args)}
    (left-associative :minus args)))

(defn b* [& args]
  (left-associative :* args))

(defn bdiv [& args]
  (left-associative :div args))

(defn b** [base exp]
  {:tag :pow
   :base base
   :exp exp})

(defn bmod [& args]
  (left-associative :mod args))

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

; TODO:
(defn bsucc []
  {:tag :succ})
; lisb succ
(defn binc [n]
  {:tag :inc
   :number n})

(defn bpred []
  {:tag :pred})
; lisb pred
(defn bdec [n]
  {:tag :dec
   :number n})


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
  {:tag :finite-subset
   :set set})

(defn bfin1 [set]
  {:tag :finite1-subset
   :set set})

(defn bcard [set]
  {:tag :card
   :set set})

(defn bunion [& sets]
  (left-associative :union sets))

(defn bintersection [& sets]
  (left-associative :intersection sets))

(defn bset- [& args]
  (left-associative :set- args))

(defn bmember [element set]
  {:tag :member
   :element element
   :set set})

(defn bnot-member [element set]
  {:tag :not-member
   :element element
   :set set})

(defn bsubset [subset set]
  {:tag :subset
   :subset subset
   :set set})

(defn bnot-subset [not-subset set]
  {:tag :not-subset
   :not-subset not-subset
   :set set})

(defn bsubset-strict [subset-strict set]
  {:tag :subset-strict
   :subset-strict subset-strict
   :set set})

(defn bnot-subset-strict [not-subset-strict set]
  {:tag :not-subset-strict
   :not-subset-strict not-subset-strict
   :set set})

; adds superset to lisb
(defn bsuperset [superset set]
  (bsubset set superset))

; adds superset-strict to lisb
(defn bsuperset-strict [superset-strict set]
  (bsubset-strict set superset-strict))

; adds not-superset to lisb
(defn bnot-superset [not-superset set]
  (bnot-subset set not-superset))

; adds not-superset-strict to lisb
(defn bnot-superset-strict [not-superset-strict set]
  (bnot-subset-strict set not-superset-strict))

(defn bunite-sets [sets]
  {:tag :general-union
   :sets sets})

(defn bintersect-sets [sets]
  {:tag :general-intersection
   :sets sets})

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

(defn bpred->bool [arg]
  {:tag :pred->bool
   :predicate arg})


;;; equality predicates

; TODO: examine chaining (left-associative)
(defn b= [left right]
  {:tag :equal
   :left left
   :right right})

; TODO: examine chaining  (mindestens ein Element ist nicht gleich!)
(defn bnot= [left right]
  {:tag :not-equal
   :left left
   :right right})

; TODO: Keines ist gleich => paarweise verschieden
;(defn bnone=


;;; logical predicates

(defn band [& predicates]
  (left-associative :and predicates))

(defn bor [& predicates]
  (left-associative :or predicates))

(defn b=> [& predicates]
  (left-associative :implication predicates))

(defn b<=> [& predicates]
  (left-associative :equivalence predicates))

(defn bnot [arg]
  {:tag :not
   :value arg})

(defn bfor-all [identifiers assignment implication]
  {:tag :for-all
   :identifiers identifiers
   :implication (b=> assignment implication)})

(defn bexists [identifiers predicate]
  {:tag :exists
   :identifiers identifiers
   :predicate predicate})


;;; misc

(defn bexpr [s]
  (node :bexpr s))

(defn brec-get [r e]
  (node :record-get r e))

(defn btuple [l r]
  (node :tuple l r))

#_(defn bmap-set [p s]
  (bran (blambda [:x] (bmember :x s) (p :x))))


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
         ~'invariants binvariants
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
         ~'var bvar
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
         ~'rec-get brec-get
         ~'struct bstruct

         ; sequences
         ~'on-sequence bsequence
         ~'seq bseq
         ~'seq1 bseq1
         ~'iseq biseq
         ~'iseq1 biseq1
         ~'perm bperm
         ~'count-seq bsize
         ~'concat bconcat
         ~'-> b->
         ~'<- b<-
         ~'reverse breverse
         ~'first bfirst
         ~'last blast
         ~'front bfront                                   ; butlast?
         ~'tail btail                                       ; rest?
         ~'conc bconc
         ~'restrict-front brestrict-front                                       ; take?
         ~'restrict-tail brestrict-tail                                      ; drop?

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
         ~'count bcard
         ~'union bunion
         ~'intersection bintersection
         ~'set- bset-                                 ; difference
         ~'member bmember                                   ; contains?
         ~'not-member bnot-member
         ~'subset bsubset
         ~'not-subset bnot-subset
         ~'subset-strict bsubset-strict
         ~'not-subset-strict bnot-subset-strict
         ~'superset bsuperset
         ~'not-superset bnot-superset
         ~'superset-strict bsuperset-strict
         ~'not-superset-strict bnot-superset-strict
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

#_(defn wrap [ctx node]
  (cond
    (keyword? node) `(~node ~ctx)
    (map? node) (into {} (map (fn f [[k v]]  [k (wrap ctx v)]) node))
    (set? node) (set (map (partial wrap ctx) node))
    (list? node) (apply list (map (partial wrap ctx) node))
    (vector? node) (vec  (map (partial wrap ctx) node))
    :otherwise node))

#_(defn almost-flatten [x]
  (remove coll? (rest (tree-seq coll? seq x))))

#_(defmacro pred [name & args]
  (let [body (last args)
        params (drop-last args)
        ctx (gensym 'lisb_ctx_)
        wrapped-body (wrap ctx body)
        keywords (set (filter keyword? (almost-flatten body)))]
    `(fn ~name ~@params
       (let [~ctx (into {} (mapv (fn [x#] [x# (keyword (gensym "lisb_"))]) ~keywords))]
         (do (lisb->node-repr ~wrapped-body))))))

#_(defmacro defpred [name & args]
  `(def ~name (pred ~name ~@args)))
