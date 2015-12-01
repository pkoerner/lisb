(ns lisb.retranslate)


(defn retranslate [data]
  (condp instance? data
    de.prob.translator.types.BigInteger (long data) ;; FIXME: could be a bigint...
    de.prob.translator.types.Boolean    (.booleanValue data)
    de.prob.translator.types.Sequence   (set (map retranslate data)) 
    de.prob.translator.types.Set        (set (map retranslate data))
    de.prob.translator.types.Tuple      (mapv retranslate data)
    de.prob.translator.types.String     (.getValue data)
    ))
