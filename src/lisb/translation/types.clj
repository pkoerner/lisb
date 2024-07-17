(ns lisb.translation.types)

(deftype Tuple 
  [xs]
  clojure.lang.Seqable
  (seq [this] (seq xs))
  clojure.lang.Indexed
  (nth [this i] (nth xs i))
  (nth [this i not-found] (nth xs i not-found))
  Object
  (toString [this] (str \[ (clojure.string/join " -> " xs) \])))

(defmethod clojure.core/print-method Tuple [this writer]
  (print-simple  ;; TODO: load clojure.pprint
    (vec (interpose '-> (.-xs this)))
    writer))

(comment
  (->Tuple [1 2]) ;; => [1 -> 2]
  (->Tuple [1 2 3]) ;; => [1 -> 2 -> 3]
  (first (->Tuple [1 2]))
  (second (->Tuple [1 2]))
  (nth (seq (->Tuple [1 2 3 4])) 2)
  (nth (->Tuple [1 2 3 4]) 2)
  (map inc (->Tuple [1 2])))
