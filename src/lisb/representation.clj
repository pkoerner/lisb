(ns lisb.representation
  (:require [clojure.math.combinatorics :refer [combinations]]))


(defn node [tag & children]
  {:tag tag
   :children (if children children [])})


(defn chain [tag tuples]
  (reduce (partial node :and) (map (partial apply node tag) tuples)))

(defn chain-arity-two [tag nodes]
  (chain tag (partition 2 1 nodes)))

(defn combine-and-chain [tag nodes]
  (chain tag (combinations nodes 2)))

(defn interleave-arity-two [tag nodes]
  (reduce (partial node tag) nodes))



(defn b< [& args]
  (chain-arity-two :less args))

(defn b> [& args]
  (chain-arity-two :greater args))

(defn b<= [& args]
  (chain-arity-two :less-eq args))

(defn b>= [& args]
  (chain-arity-two :greater-eq args))

(defn b+ [& args]
  (interleave-arity-two :plus args))

(defn b- [a & r]
  (if (seq r)
    (interleave-arity-two :minus (conj r a))
    (node :unaryminus a)))

(defn b* [& args]
  (interleave-arity-two :mul args))

(defn bdiv [& args]
  (interleave-arity-two :div args))

(defn band [& args]
  (interleave-arity-two :and args))

(defn b= [& args]
  (chain-arity-two :equals args))

(defn b<=> [& args]
  (chain-arity-two :equivalence args))

(defn bor [& args]
  (interleave-arity-two :or args))

(defn bnot [a]
  (node :not a))

(defn bnot= [& args]
  (combine-and-chain :not-equals args))

(defn bpred->bool [a]
  (node :to-bool a))

(defn bset [v p]
  (node :comp-set (apply node :var-list v) p))

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

(defn bx [& args]
  (interleave-arity-two :cartesian-product args))

(defn bunion [& args]
  (interleave-arity-two :set-union args))

(defn bintersect [& args]
  (interleave-arity-two :set-intersection args))

(defn bset- [& args]
  (interleave-arity-two :set-difference args))

(defn bmember [e & sets]
  (chain :member (map (fn [s] [e s]) sets)))

(defn bmembers [s & elements]
  (chain :member (map (fn [e] [e s]) elements)))

(defn bsubset [& args]
  (chain-arity-two :subset args))

(defn bsuperset [& args]
  (apply bsubset (reverse args)))

(defn bsubset-strict [& args]
  (chain-arity-two :subset-strict args))

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
  (interleave-arity-two :relation args))

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
  (interleave-arity-two :relational-override args))

(defn b>< [& args]
  (interleave-arity-two :direct-product args))

(defn bcomp [& args]
  (interleave-arity-two :relational-composition args))

(defn b|| [& args]
  (interleave-arity-two :parallel-product args))

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
  (interleave-arity-two :partial-fn args))

(defn b--> [& args]
  (interleave-arity-two :total-fn args))

(defn b+->> [& args]
  (interleave-arity-two :partial-surjection args))

(defn b-->> [& args]
  (interleave-arity-two :total-surjection args))

(defn b>+> [& args]
  (interleave-arity-two :partial-injection args))

(defn b>-> [& args]
  (interleave-arity-two :total-injection args))

(defn b>+>> [& args]
  (interleave-arity-two :partial-bijection args))

(defn b>->> [& args]
  (interleave-arity-two :total-bijection args))

(defn blambda [identifiers pred expr]
  (let [vars (apply node :var-list identifiers)]
    (node :lambda vars pred expr)))

(defn bapply [f & args]
  (apply node :fn-application f args))

(defn b=> [& args]
  (interleave-arity-two :implication args))

(defn bforall [identifiers impl]
  (node :forall (apply node :var-list identifiers) impl))

(defn bexists [identifiers pred]
  (node :exists (apply node :var-list identifiers) pred))

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
  (interleave-arity-two :concat args))

(defn b-> [e s]
  (node :prepend e s))

(defn b<- [& args]
  (interleave-arity-two :append args))

(defn breverse [s]
  (node :reverse s))

(defn bfirst [s]
  (node :first s))

(defn blast [s]
  (node :last s))

; TODO: - negations for subset/superset, strict/non-strict
;       - generalized union/intersection, with/without predicate
;       - power (right-associative)
;       - set product / summation
;       - seq/seq1













;; TODO: bset, bpow, bpow1, bfin, bfin1,
;;       sub- and supersets in all colours
;;       sets of bools, naturals, ints, nats
(defmacro b
  [repr]
  `(let [~'+ b+
         ~'- b-
         ~'* b*
         ~'/ bdiv
         ~'max bmax
         ~'min bmin
         ~'mod bmod
         ~'inc binc
         ~'dec bdec
         ~'range binterval
         ~'< b<
         ~'> b>
         ~'<= b<=
         ~'>= b>=

         ~'and band
         ~'or bor
         ~'not bnot
         ~'not= bnot=
         ~'= b=
         ~'<=> b<=>
         ~'=> b=>
         ~'bool bpred->bool
         ~'forall bforall
         ~'exists bexists

         ~'count bcount
         ~'x bx
         ~'union bunion
         ~'intersection bintersect
         ~'difference bset-
         ~'contains? bmembers
         ~'member? bmember

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
         ~'seq bseq
         ~'seq1 bseq1
         ~'perm bperm
         ~'concat bconcat
         ~'-> b->
         ~'<- b<-
         ~'reverse breverse
         ~'first bfirst
         ~'last blast
         ]
    ~repr))
