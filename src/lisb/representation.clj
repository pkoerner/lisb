(ns lisb.representation)


(defn node [tag & children]
  {:tag tag
   :children (if children children [])})


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

(defn b= [& args]
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

(defn bset [v p]
  (node :comp-set (apply node :list v) p))

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

(defn bmember [e s]
  (node :member e s))

(defn bnot-member [e s]
  (bnot (bmember e s)))

(defn bcontains [s e]
  (node :member e s))

(defn bsubset [& args]
  (apply node :subset args))

(defn bsuperset [& args]
  (apply bsubset (reverse args)))

(defn bsubset-strict [& args]
  (apply node :subset-strict args))

(defn bsuperset-strict [& args]
  (apply bsubset-strict (reverse args)))

(defn bbool-set []
  (node :bool-set))

(defn bnatural-set []
  (node :natural-set))

(defn bnatural1-set []
  (node :natural1-set))

(defn bint-set []
  (node :int-set))

(defn binteger-set []
  (node :integer-set))

(defn bnat-set []
  (node :nat-set))

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

(defn b<-> [& args]
  (apply node :relation args))

(defn bdom [r]
  (node :domain r))

(defn bran [r]
  (node :range r))

(defn bid [s]
  (node :identity-relation s))


(defn b<| [s r]
  (node :domain-restriction s r))

(defn b<<| [s r]
  (node :domain-subtraction s r))

(defn b|> [r s]
  (node :range-restriction r s))

(defn b|>> [r s]
  (node :range-subtraction r s))

(defn binverse [r]
  (node :inverse-relation r))

(defn bimage [r s]
  (node :relational-image r s))

(defn b<+ [& args]
  (apply node :relational-override args))

(defn b>< [& args]
  (apply node :direct-product args))

(defn bcomp [& args]
  (apply node :relational-composition args))

(defn b|| [& args]
  (apply node :parallel-product args))

(defn bprj1 [s t]
  (node :proj1 s t))

(defn bprj2 [s t]
  (node :proj2 s t))

(defn bclosure [r]
  (node :closure r))

(defn bclosure1 [r]
  (node :closure1 r))

(defn biterate [r n]
  (node :iterate r n))

(defn bfnc [r]
  (node :functionise r))

(defn brel [r]
  (node :relationise r))

(defn b+-> [& args]
  (apply node :partial-fn args))

(defn b--> [& args]
  (apply node :total-fn args))

(defn b+->> [& args]
  (apply node :partial-surjection args))

(defn b-->> [& args]
  (apply node :total-surjection args))

(defn b>+> [& args]
  (apply node :partial-injection args))

(defn b>-> [& args]
  (apply node :total-injection args))

(defn b>+>> [& args]
  (apply node :partial-bijection args))

(defn b>->> [& args]
  (apply node :total-bijection args))

(defn blambda [identifiers pred expr]
  (let [vars (apply node :list identifiers)]
    (node :lambda vars pred expr)))

(defn bapply [f & args]
  (apply node :fn-application f args))

(defn b=> [& args]
  (apply node :implication args))

(defn bforall
  ([identifiers impl]
    (node :forall (apply node :list identifiers) impl))
  ([identifiers impl-left impl-right]
   (bforall identifiers (b=> impl-left impl-right))))

(defn bexists [identifiers pred]
  (node :exists (apply node :list identifiers) pred))

(defn binterval [from to]
  (node :interval from to))

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

(defn bif [condition then else]
  (node :if condition then else))

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


(defn blet [tag kvs p]
  (let [kv-pairs (partition 2 kvs)
        identifiers (map first kv-pairs)]
    (node tag (apply node :list identifiers)
              (reduce band (map (partial apply b=) kv-pairs))
              p))) 

(defn blet-pred [kvs p] 
  (blet :let-pred kvs p))


(defn blet-expr [kvs e] 
  (blet :let-expr kvs e))

; TODO: - negations for subset/superset, strict/non-strict













;; TODO: bset, bpow, bpow1, bfin, bfin1,
;;       sets of bools, naturals, ints, nats
(defmacro b
  [repr]
  `(let [~'+ b+
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
