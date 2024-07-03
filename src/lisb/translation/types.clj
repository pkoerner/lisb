(ns lisb.translation.types)

(deftype Tuple [left right]
  clojure.lang.Seqable
  (seq [this] (seq [left right]))
  Object
  (toString [this] (str \[ left " -> " right \])))

(defmethod clojure.core/print-method Tuple [this writer]
  (print-simple  ;; TODO: load clojure.pprint
    `[~(.-left this) ~'-> ~(.-right this)]
    writer))

(comment
  (->Tuple 1 2) ;; => [1 -> 2]
  (first (->Tuple 1 2))
  (second (->Tuple 1 2))
  (seq (->Tuple 1 2))
  (map inc (->Tuple 1 2)))
