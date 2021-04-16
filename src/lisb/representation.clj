(ns lisb.representation)


(defn node [tag & children]
  {:tag tag
   :children (if children children [])})

(declare bif)
(declare blet)


;;; machine definition

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

(defn bcontraints [& predicates]
  {:tag :contraints
   :children predicates})

(defn bsets [& set-definitions]
  {:tag :sets
   :children set-definitions})
(defn bdeferred-set [identifier]
  {:tag :deferred-set
   :identifier identifier})
(defn benumerated-set [identifier elements]
  {:tag :enumerated-set-set
   :identifier identifier
   :elements elements})

(defn bconstants [& identifiers]
  {:tag :constants
   :children identifiers})

(defn bproperties [predicate]
  {:tag :properties
   :predicate predicate})

(defn bdefinitions [& definitions]
  {:tag :definitions
   :children definitions})

(defn bvariables [& identifiers]
  {:tag :variables
   :children identifiers})

(defn binvariants [predicate]
  {:tag :invariants
   :predicate predicate})

(defn bassertions [& predicates]
  {:tag :assertions
   :children predicates})

(defn binit [substitution]
  {:tag :init
   :substitution substitution})

(defn boperations [& operations]
  {:tag :operations
   :children operations})

;;; substitutions

(defn bskip []
  {:tag :skip})

; TODO: BlockSubstition müsste entfernt werden können
(defn bblock [p-substitution]
  {:tag :block
   :p-substitution p-substitution})

; TODO: Kann wahrscheinlich besser dargestellt werden
(defn bassign [lhs-exprs rhs-exprs]
  {:tag :assign
   :lhs-exprs lhs-exprs
   :rhs-exprs rhs-exprs})

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

(defn bif-sub
  ([predicate then] (bif {:tag :if-sub} predicate then))
  ([predicate then else] (bif {:tag :if-sub} predicate then else)))

#_([predicate then & else-ifs]
   (if (= (mod (count else-ifs) 2) 0)
     {:tag :if, :predicate predicate, :then then, :else-ifs else-ifs}
     {:tag :if, :predicate predicate, :then then, :else-ifs (drop-last else-ifs), :else (last else-ifs)}))

(defn b= [left right]
  {:tag :equal
   :left left
   :right right})

(defn bmember [left right]
  {:tag :member
   :left left
   :right right})

(defn bnat-set []
  {:tag :nat-set})

(defn brelation [left right]
  {:tag :relation
   :left left
   :right right})

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

(defn b< [& args]
  (apply node :less args))

(defn b> [& args]
  (apply node :greater args))

(defn b<= [& args]
  (apply node :less-eq args))

(defn b>= [& args]
  (apply node :greater-eq args))

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

(defn band [& args]
  (apply node :and args))

#_(defn b= [& args]
  (apply node :equals args))

(defn b<=> [& args]
  (apply node :equivalence args))

(defn bor [& args]
  (apply node :or args))

(defn bnot [a]
  (node :not a))

(defn bnone= [& args]
  (apply node :not-equals args))

(defn bpred->bool [a]
  (node :to-bool a))

;;; Sets

(defn bcomp-set [identifiers predicate]
  {:tag :comp-set
   :identifiers identifiers
   :predicate predicate
   })

(defn bset-enum [& args]
  (apply node :enumerated-set args))

(defn bpow [s]
  (node :power-set s))

(defn bpow1 [s]
  (node :power1-set s))

(defn bfin [s]
  (node :finite-subset s))

(defn bfin1 [s]
  (node :finite1-subset s))

(defn bcount [s]
  (node :card s))

(defn bunion [& args]
  (apply node :set-union args))

(defn bunite-sets [s]
  (node :general-union s))

(defn bunion-pe [identifiers pred expr]
  (node :union-pe (apply node :list identifiers) pred expr))

(defn bintersection [& args]
  (apply node :set-intersection args))

(defn bintersect-sets [s]
  (node :general-intersection s))

(defn bintersection-pe [identifiers pred expr]
  (node :intersection-pe (apply node :list identifiers) pred expr))

(defn bset- [& args]
  (apply node :set-difference args))

(defn bminus-or-set-subtract
  "For generated code only. I am allowed to use this. You are not."
  [& args]
  (apply node :minus-or-set-subtract args))

(defn bnot-member [e s]
  (bnot (bmember e s)))

(defn bcontains [s e]
  (node :member e s))

(defn bsubset [& args]
  (apply node :subset args))

(defn bnot-subset [& args]
  (apply node :not-subset args))

(defn bsuperset [& args]
  (apply bsubset (reverse args)))

(defn bsubset-strict [& args]
  (apply node :subset-strict args))

(defn bnot-subset-strict [& args]
  (apply node :not-subset-strict args))

(defn bsuperset-strict [& args]
  (apply bsubset-strict (reverse args)))

(defn bbool-set []
  (node :bool-set))

;;; Numbers

(defn bnatural-set []
  (node :natural-set))

(defn bnatural1-set []
  (node :natural1-set))

(defn bint-set []
  (node :int-set))

(defn binteger-set []
  (node :integer-set))

(defn bnat1-set []
  (node :nat1-set))

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

(defn bmod [n m]
  (node :mod n m))

(defn binc [n]
  (b+ n 1))

(defn bdec [n]
  (b- n 1))

;;; Relations

(defn b<-> [& sets]
  {:tag :relation
   :sets sets})

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

(defn bclosure [r]
  {:tag :closure
   :relation r})

(defn bclosure1 [r]
  {:tag :closure1
   :relation r})

(defn biterate [r n]
  (node :iterate r n))

(defn bfnc [r]
  (node :functionise r))

(defn brel [r]
  (node :relationise r))

;;; Functions

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

(defn blambda [identifiers pred expr]
  (let [vars (apply node :list identifiers)]
    (node :lambda vars pred expr)))

(defn bapply [f & args]
  (apply node :fn-application f args))


(defn b=> [& args]
  (apply node :implication args))

(defn bforall
  ([identifiers impl]
   {:tag :forall
    :identifiers identifiers
    :impl impl})
  ([identifiers impl-left impl-right]
   (bforall identifiers (b=> impl-left impl-right))))

(defn bexists [identifiers pred]
  (node :exists (apply node :list identifiers) pred))

(defn binterval [from to]
  (node :interval from to))

;;; Sequences

(defn bsequence [& args]
  (apply node :sequence args))

(defn biseq [s]
  (node :iseq s))

(defn biseq1 [s]
  (node :iseq1 s))

(defn bperm [s]
  (node :perm s))

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

(defn btake [n s]
  (brestrict-front s n))

(defn brestrict-tail [s n]
  (node :restrict-tail s n))

(defn bdrop [n s]
  (brestrict-tail s n))

(defn b** [& args]
  (apply node :pow args))

(defn bsigma [identifiers p e]
  (node :sigma (apply node :list identifiers) p e))

(defn bpi [identifiers p e]
  (node :pi (apply node :list identifiers) p e))

(defn bseq [s]
  (node :seq s))

(defn bseq1 [s]
  (node :seq1 s))

(defn bconc [s]
  (node :conc s))

(defn- bstructy 
  ([k m]
   (node k 
         (apply node :list (map name (keys m)))
         (apply node :list (vals m))))
  ([k k1 v1 & keyvals]
   (let [m (apply hash-map k1 v1 keyvals)]
     (bstructy k m))))

(def bstruct (partial bstructy :struct))
(def brecord (partial bstructy :record))

(defn brec-get [r e]
  (node :record-get r e))



(defn btuple [l r]
  (node :tuple l r))


(defn bmap-set [p s]
  (bran (blambda [:x] (bmember :x s) (p :x))))

(defn brange [from to]
  (node :interval from (bdec to)))

(defn bnot= [& args]
  (bnot (apply b= args)))


(defn bexpr [s]
  (node :bexpr s))

(defn bpred [s]
  (node :bpred s))

(defn bstr [s]
  (node :string s))

(defn bstring-set []
  (node :string-set))


(defn bcall [f & args]
  (apply node :fn-call f args))

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
; TODO: - negations for subset/superset, strict/non-strict





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
         ~'fn blambda
         ~'apply bapply

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
