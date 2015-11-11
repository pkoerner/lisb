(ns lisb.representation
  (require [clojure.math.combinatorics :refer [combinations]]))


(defn node [tag & children]
  {:tag tag
   :children children})


(defn chain-arity-two
  [tag nodes]
  (let [tuples (partition 2 1 nodes)]
    (reduce (partial node :and) (map (partial apply node tag) tuples))))

(defn combine-and-chain [tag nodes]
  (let [tuples (combinations nodes 2)]
    (reduce (partial node :and) (map (partial apply node tag) tuples))))

(defn interleave-arity-two [tag nodes]
  (reduce (partial node tag) nodes))



(defn b< [& args]
  (chain-arity-two :less args))

(defn b+ [& args]
  (interleave-arity-two :plus args))

(defn b- [a & r]
  (if (seq r)
    (interleave-arity-two :minus (conj r a))
    (node :unaryminus a)))

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
  (node :comp-set v p))

(defn bpow [s]
  (node :power-set s))

(defn bpow1 [s]
  (node :power1-set s))

(defn bfin [s]
  (node :finite-subset s))

(defn bfin1 [s]
  (node :finite1-subset s))

; TODO: - implication (is it left- or right-associative?)
;       - exists
;       - forall
;       - bool-set

