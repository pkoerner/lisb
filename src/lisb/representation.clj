(ns lisb.representation)

(defn conjunct [l r]
  {:tag :and
   :children [l r]})


(defn plus [l r]
  {:tag :plus
   :children [l r]})


(defn less [l r]
  {:tag :less
   :children [l r]})

(defn minus [l r]
  {:tag :minus
   :children [l r]})

(defn unaryminus [n]
  {:tag :unaryminus
   :children [n]})

(defn equals [l r]
  {:tag :equals
   :children [l r]})

(defn equivalence [l r]
  {:tag :equivalence
   :children [l r]})

(defn chain 
  [f nodes]
  (let [tuples (partition 2 1 nodes)]
    (reduce conjunct (map (partial apply f) tuples))))

(defn b< [& args]
  (chain less args))

(defn b+ [& args]
  (reduce plus args))

(defn b- [a & r]
  (if (seq r)
    (reduce minus a r)
    (unaryminus a)))

(defn band [& args]
  (reduce conjunct args))

(defn b= [& args]
  (chain equals args))

(defn b<=> [& args]
  (chain equivalence args))
