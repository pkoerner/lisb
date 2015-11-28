(ns lisb.data-conversion
  (:require [lisb.representation :refer [bsequence bset-enum btuple brecord
                                         b+-> b-->
                                         b+->> b-->>
                                         b>+> b>->
                                         b>+>> b>->>]]))

(defn convert [data btype inner]
  (let [inner (if (nil? inner) :literal inner)]
    (case btype
      :literal data
      :set (apply bset-enum data)
      :tuple (apply btuple data)
      :sequence (apply bsequence data)
      :record (apply brecord [data])
      ;; TODO: other functions
      :fn (apply bset-enum (map #(convert % :tuple (first inner)) data)))))
