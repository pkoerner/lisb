(ns lisb.prob.retranslate
  (:require [lisb.translation.types :refer [->Tuple]])
  (:import
    (de.hhu.stups.prob.translator BAtom BBoolean BNumber BRecord BSet BString BTuple BReal)
    (de.hhu.stups.prob.translator.interpretations BFunction BRelation BSequence)))


(defn retranslate [data]
  (condp instance? data
    ; value types
    BAtom (.stringValue data)
    BBoolean (.booleanValue data)
    BNumber (try (.longValueExact data)
                 (catch ArithmeticException _ (.bigIntegerValue data)))
    BString (.stringValue data)
    BReal (.floatValue data)
    ; interpreted collection types of set
    BSequence (mapv retranslate (.toList data))
    BFunction (reduce
                (fn [m e]
                  (assoc m (retranslate (.getKey e)) (retranslate (.getValue e))))
                {}
                (.toMap data))
    BRelation (reduce
                (fn [m e]
                  (assoc m (retranslate (.getKey e)) (mapv retranslate (.getValue e))))
                {}
                (.toRelationalMap data))
    ; collection types
    BSet (set (map retranslate (.toSet data)))
    BTuple (if (instance? BTuple (.getFirst data))
             (->Tuple (conj (vec (seq (retranslate (.getFirst data)))) (retranslate (.getSecond data))))
             (->Tuple [(retranslate (.getFirst data)) (retranslate (.getSecond data))]))
    BRecord (reduce
              (fn [m e]
                (assoc m (keyword (.getKey e)) (retranslate (.getValue e))))
              {}
              (.toMap data))
    (throw (IllegalArgumentException. (str "unexpected value to retranslate: " data)))))
