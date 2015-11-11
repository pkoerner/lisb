(ns lisb.representation)


(defn node [tag & children]
  {:tag tag
   :children children})


(defn chain-arity-two
  [tag nodes]
  (let [tuples (partition 2 1 nodes)]
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
