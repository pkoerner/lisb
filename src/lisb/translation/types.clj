(ns lisb.translation.types)

(defprotocol Flattable
  (make-flat [this]))

(deftype Tuple 
  [xs]

  Flattable
  (make-flat [this]
    (if (instance? Tuple (first xs))
      (Tuple. (into (into [] (seq (make-flat (first xs)))) (map #(if (satisfies? Flattable %) (make-flat %) %) (rest xs))))
      this))

  clojure.lang.Seqable
  (seq [this] (seq xs))

  clojure.lang.Indexed
  (nth [this i] (nth xs i))
  (nth [this i not-found] (nth xs i not-found))

  Object
  (equals [this other] (and (instance? Tuple other)  (= (.-xs (make-flat this)) (.-xs (make-flat other)))))
  (toString [this] (str \[ (clojure.string/join " -> " xs) \])))

(defmethod clojure.core/print-method Tuple [this writer]
  (print-simple
    (vec (interpose '-> (.-xs this)))
    writer))

(comment
  (->Tuple [1 2]) ;; => [1 -> 2]
  (->Tuple [1 2 3]) ;; => [1 -> 2 -> 3]
  (fixate!! (first (->Tuple [1 2])))
  (second (->Tuple [1 2]))
  (nth (seq (->Tuple [1 2 3 4])) 2)
  (nth (->Tuple [1 2 3 4]) 2)
  (map inc (->Tuple [1 2]))
  
  (make-flat (->Tuple [1 2 3]))
  (make-flat (->Tuple [(->Tuple [0 1]) 2 3]))
  (make-flat (->Tuple [(->Tuple [(->Tuple [0 1]) 2]) 3 4]))
  (make-flat (->Tuple [(->Tuple [0 (->Tuple [1 2])]) 3 4]))
  (= (->Tuple [1 2]) (->Tuple [1 3]))
  (= (->Tuple [1 2 3]) (->Tuple [(->Tuple [1 2]) 3]))
  (= (->Tuple [1 2 3]) (->Tuple [1 (->Tuple [2 3])]))
  (= (->Tuple [0 1 2 3 4]) (->Tuple [(->Tuple [(->Tuple [0 1]) 2]) 3 4]))
  (= (->Tuple [0 1 2 3 4]) (->Tuple [(->Tuple [0 (->Tuple [1 2])]) 3 4]))
  )
