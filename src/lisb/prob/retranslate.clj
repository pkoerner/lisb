(ns lisb.prob.retranslate
  (:import
    (de.hhu.stups.prob.translator BAtom BBoolean BNumber BRecord BSet BString BTuple)
    (de.hhu.stups.prob.translator.interpretations BFunction BRelation BSequence)))


(defn retranslate [data]
  (condp instance? data
    ; value types
    BAtom (.stringValue data)
    BBoolean (.booleanValue data)
    BNumber (.longValue data)
    BString (.stringValue data)
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
    BTuple [(.getFirst data) (.getSecond data)]
    BRecord (reduce
              (fn [m e]
                (assoc m (retranslate (.getKey e)) (retranslate (.getValue e))))
              {}
              (.toMap data))))
