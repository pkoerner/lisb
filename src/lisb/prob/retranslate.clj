(ns lisb.prob.retranslate
  (:require [wall.hack :refer [method]])
  (:import
    (de.hhu.stups.prob.translator BAtom BBoolean BNumber BRecord BSet BString BTuple BReal)
    (de.hhu.stups.prob.translator.interpretations BFunction BRelation BSequence)))


(defn retranslate [data]
  (condp instance? data
    ; value types
    BAtom (.stringValue data)
    BBoolean (.booleanValue data)
    BNumber (.longValue data)
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
    BTuple [(retranslate (.getFirst data)) (retranslate (.getSecond data))]
    BRecord (reduce
              (fn [m e]
                (assoc m (retranslate (.getKey e)) (retranslate (.getValue e))))
              {}
              (.toMap data))
    de.hhu.stups.prob.translator.TranslatingVisitor$RecordEntry [(method de.hhu.stups.prob.translator.TranslatingVisitor$RecordEntry 'getKey [] data)
                                                                 (retranslate (method de.hhu.stups.prob.translator.TranslatingVisitor$RecordEntry 'getValue [] data))]))

