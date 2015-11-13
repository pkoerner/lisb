(ns lisb.representation
  (require [clojure.math.combinatorics :refer [combinations]]))


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
   (let [args (conj (conj (set r) b a))]
     (bmax args))))

(defn bmin
  ([s]
   (node :min s))
  ([a b & r]
   (let [args (conj (conj (set r) b a))]
     (bmin args))))

(defn bmod [n m]
  (node :mod n m))

(defn binc [n]
  (b+ n 1))

(defn bdec [n]
  (b- n 1))

(defn b<-> [& args]
  (interleave-arity-two :relation args))

; TODO: - implication (is it left- or right-associative?)
;       - exists
;       - forall
;       - bool-set
;       - negations for subset/superset, strict/non-strict
;       - generalized union/intersection, with/without predicate
;       - power (right-associative)
;       - set product / summation

