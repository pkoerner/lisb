(ns lisb.translation.types)

(deftype Tuple 
  [xs]
  clojure.lang.Seqable
  (seq [this] (seq xs))
  clojure.lang.Indexed
  (nth [this i] (nth xs i))
  (nth [this i not-found] (nth xs i not-found))
  Object
  (equals [this other] (and (instance? Tuple other)  (= xs (.-xs other)))) ;; TODO: are nested and flat tuples equal? I think so.
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
  (map inc (->Tuple [1 2])))
