(ns lisb.representation)


(defn node [tag & children]
  {:tag tag
   :children (if children children [])})

(declare b=)
(declare band)
(declare bnot)
#_(declare blambda)

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

(defn bcontraints [predicate]
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

(defn bskip []
  {:tag :skip})

; TODO: BlockSubstition müsste entfernt werden können
(defn bblock [p-substitution]
  {:tag :block
   :p-substitution p-substitution})

(defn bassign [& idvals]
  {:tag :assign
   :idvals idvals})

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
  {:tag :bassert
   :predicate predicate
   :substitution substitution})

(defn bchoice [& substitutions]
  {:tag :choice
   :substitutions substitutions})

(defn bif-sub
  ([predicate then] (bif {:tag :if-sub} predicate then))
  ([predicate then else] (bif {:tag :if-sub} predicate then else)))

#_([predicate then & else-ifs]
   (if (= (mod (count else-ifs) 2) 0)
     {:tag :if, :predicate predicate, :then then, :else-ifs else-ifs}
     {:tag :if, :predicate predicate, :then then, :else-ifs (drop-last else-ifs), :else (last else-ifs)}))

(defn bselect
  ([condition then] {:tag :operation, :condition condition, :then then})
  ([condition then else]
   (let [select (bselect condition then)]
     (if else
       (assoc select :else else)
       select))))


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

(defn bstring-set []
  (node :string-set))


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

(defn bsequence [& args]
  (apply node :sequence args))

(defn bseq [s]
  (node :seq s))

(defn bseq1 [s]
  (node :seq1 s))

(defn biseq [s]
  (node :iseq s))

(defn biseq1 [s]
  (node :iseq1 s))

(defn bperm [s]
  (node :perm s))

(defn bcount [s]
  (node :card s))

(defn bconcat [& args]
  (apply node :concat args))

(defn b-> [e s]
  (node :prepend e s))

(defn b<- [& args]
  (apply node :append args))

(defn breverse [s]
  (node :reverse s))

(defn bfirst [s]
  (node :first s))

(defn blast [s]
  (node :last s))

(defn bfront [s]
  (node :front s))

(defn btail [s]
  (node :tail s))

(defn brestrict-front [s n]
  (node :restrict-front s n))
; clojure syntax
(defn btake [n s]
  (brestrict-front s n))

(defn brestrict-tail [s n]
  (node :restrict-tail s n))
; clojure syntax
(defn bdrop [n s]
  (brestrict-tail s n))

(defn bconc [s]
  (node :conc s))

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

(defn blambda [ids pred expr]
  {:tag :lambda
   :ids ids
   :pred pred
   :expr expr})

#_(defn bapply [f & args]
  (apply node :fn-application f args))
(defn bcall [f & args]
  {:tag :call
   :f f
   :args args})


;;; relations

(defn b<-> [& sets]
  {:tag :relation
   :sets sets})

(defn btotal-relation [left right]
                   {:tag :total-relation
                    :left left
                    :right right})

(defn bsurjective-relation [left right]
  {:tag :surjective-relation
   :left left
   :right right})

(defn btotal-surjective-relation [left right]
  {:tag :total-surjective-relation
   :left left
   :right right})

(defn bcouple [& expressions]
  {:tag :couple
   :expressions expressions})

(defn bdom [r]
  {:tag :domain
   :relation r})

(defn bran [r]
  {:tag :range
   :relation r})

(defn bid [s]
  {:tag :identity-relation
   :set s})

(defn b<| [s r]
  {:tag :domain-restriction
   :set s
   :relation r})

(defn b<<| [s r]
  {:tag :domain-subtraction
   :set s
   :relation r})

(defn b|> [r s]
  {:tag :range-restriction
   :relation r
   :set s})

(defn b|>> [r s]
  {:tag :range-subtraction
   :relation r
   :set s})

(defn binverse [r]
  {:tag :inverse-relation
   :relation r})

(defn bimage [r s]
  {:tag :relational-image
   :relation r
   :set s})

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

(defn bprj1 [s t]
  {:tag :proj1
   :set1 s
   :set2 t})

(defn bprj2 [s t]
  {:tag :proj2
   :set1 s
   :set2 t})

(defn bclosure1 [r]
  {:tag :closure1
   :relation r})

(defn bclosure [r]
  {:tag :closure
   :relation r})

(defn biterate [r n]
  (node :iterate r n))

(defn bfnc [r]
  (node :functionise r))

(defn brel [r]
  (node :relationise r))


;;; numbers

(defn binteger-set []
  (node :integer-set))

(defn bnatural-set []
  (node :natural-set))

(defn bnatural1-set []
  (node :natural1-set))

(defn bint-set []
  (node :int-set))

(defn bnat-set []
  {:tag :nat-set})

(defn bnat1-set []
  (node :nat1-set))

(defn binterval [from to]
  (node :interval from to))
; clojure syntax
#_(defn brange [from to]
  (node :interval from (bdec to)))

(defn bmin-int []
  {:tag :min-int})

(defn bmax-int []
  {:tag :max-int})

(defn b< [& args]
  (apply node :less args))

(defn b> [& args]
  (apply node :greater args))

(defn b<= [& args]
  (apply node :less-eq args))

(defn b>= [& args]
  (apply node :greater-eq args))

(defn bmax
  ([s]
   (node :max s))
  ([a b & r]
   (let [args (conj (set r) b a)]
     (bmax args))))

(defn bmin
  ([s]
   (node :min s))
  ([a b & r]
   (let [args (conj (set r) b a)]
     (bmin args))))

(defn b+ [& args]
  (apply node :plus args))

(defn b- [a & r]
  (if (seq r)
    (apply node :minus (conj r a))
    (node :unaryminus a)))

(defn b* [& args]
  (apply node :mul args))

(defn bdiv [& args]
  (apply node :div args))

(defn b** [base exp]
  {:tag :pow
   :base base
   :exp exp})

(defn bmod [& args]
  {:tag :mod
   :values args})

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
   :predicate predicate
   })

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

(defn bunion [& sets]
  {:tag :union
   :sets sets})

(defn bintersection [& sets]
  {:tag :set-intersection
   :sets sets})

(defn bset- [& args]
  (apply node :set-difference args))

#_(defn bminus-or-set-subtract
  "For generated code only. I am allowed to use this. You are not."
  [& args]
  (apply node :minus-or-set-subtract args))

(defn bmember [left right]
  {:tag :member
   :left left
   :right right})
; clojure syntax
(defn bcontains? [set element]
  {:tag :contains?
   :set set
   :element element})

(defn bnot-member [element set]
  {:tag :not-member
   :element element
   :set set})

(defn bsubset [subset set]
  {:tag :subset
   :subset subset
   :set set})
; adds superset to lisb
(defn bsuperset [superset set]
  (bsubset set superset))

(defn bnot-subset [not-subset set]
  {:tag :not-subset
   :not-subset not-subset
   :set set})
; adds not-superset to lisb
(defn bnot-superset [not-superset set]
  (bnot-subset set not-superset))

(defn bsubset-strict [subset-strict set]
  {:tag :subset-strict
   :subset-strict subset-strict
   :set set})
; adds superset-strict to lisb
(defn bsuperset-strict [superset-strict set]
  (bsubset-strict set superset-strict))

(defn bnot-subset-strict [not-subset-strict set]
  {:tag :not-subset-strict
   :not-subset-strict not-subset-strict
   :set set})
; adds not-superset-strict to lisb
(defn bnot-superset-strict [not-superset-strict set]
  (bnot-subset-strict set not-superset-strict))

(defn bunite-sets [sets]
  {:tag :general-union
   :sets sets})

(defn bintersect-sets [sets]
  {:tag :general-intersection
   :sets sets})

(defn bunion-pe [identifiers pred expr]
    (node :union-pe (apply node :list identifiers) pred expr))

(defn bintersection-pe [identifiers pred expr]
    (node :intersection-pe (apply node :list identifiers) pred expr))


;;; booleans

(defn bbool-set []
  {:tag :bool-set})

(defn bpred->bool [arg]
  {:tag :to-bool
   :value arg})


;;; equality predicates

(defn b= [& args]
  {:tag :equal
   :values args})

(defn bnot= [& args]
  {:tag :not
   :values args})


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

(defn bnot [arg]
  {:tag :not
   :value arg})

(defn bfor-all [identifiers assignment implication]
  {:tag :for-all
   :identifiers identifiers
   :assignment assignment
   :implication implication})

#_(defn bforall
  ([identifiers impl]
   {:tag :forall
    :identifiers identifiers
    :impl impl})
  ([identifiers impl-left impl-right]
   (bforall identifiers (b=> impl-left impl-right))))

(defn bexists [identifiers pred]
  (node :exists (apply node :list identifiers) pred))


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
(defmacro b
  [repr]
  `(let [~'machine bmachine
         ~'machine-variant bmachine-variant
         ~'machine-header bmachine-header
         ~'+ b+
         ~'- b-
         ~'* b*
         ~'/ bdiv
         ~'** b**
         ~'max bmax
         ~'min bmin
         ~'mod bmod
         ~'inc binc
         ~'dec bdec
         ~'range brange
         ~'interval binterval
         ~'< b<
         ~'> b>
         ~'<= b<=
         ~'>= b>=
         ~'sigma bsigma
         ~'pi bpi

         ~'and band
         ~'or bor
         ~'not bnot
         ~'none= bnone=
         ~'not= bnot=
         ~'= b=
         ~'<=> b<=>
         ~'=> b=>
         ~'bool bpred->bool
         ~'forall bforall
         ~'exists bexists

         ~'count bcount
         ~'union bunion
         ~'unite-sets bunite-sets
         ~'intersection bintersection
         ~'intersect-sets bintersect-sets
         ~'difference bset-
         ~'contains? bcontains
         ~'member? bmember
         ~'subset? bsubset
         ~'superset? bsuperset

         ~'<-> b<->
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
         ~'closure bclosure
         ~'closure1 bclosure1
         ~'iterate biterate
         ~'fnc bfnc
         ~'rel brel
         ~'+-> b+->
         ~'--> b-->
         ~'+->> b+->>
         ~'-->> b-->>
         ~'>+> b>+>
         ~'>-> b>->
         ~'>+>> b>+>>
         ~'>->> b>->>
         ; ~'fn blambda
         ;~'apply bapply

         ~'sequence bsequence
         ~'iseq biseq
         ~'iseq1 biseq1
         ~'perm bperm
         ~'concat bconcat
         ~'-> b->
         ~'<- b<-
         ~'reverse breverse
         ~'first bfirst
         ~'last blast
         ~'butlast bfront
         ~'rest btail
         ~'take btake
         ~'drop bdrop

         ~'struct bstruct
         ~'record brecord

         ~'if bif
         ]
    ~repr))


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
