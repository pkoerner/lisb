(ns lisb.representation)


(defn node [tag & children]
  {:tag tag
   :children children})

(defn chain-arity-two
  [tag nodes]
  (let [tuples (partition 2 1 nodes)]
    (reduce (partial node :and) (map (partial apply node tag) tuples))))


(defn b< [& args]
  (chain-arity-two :less args))


(defn b+ [& args]
  (reduce (partial node :plus) args))


(defn b- [a & r]
  (if (seq r)
    (reduce (partial node :minus) a r)
    (node :unaryminus a)))

(defn band [& args]
  (reduce (partial node :and) args))

(defn b= [& args]
  (chain-arity-two :equals args))

(defn b<=> [& args]
  (chain-arity-two :equivalence args))
