(ns lisb.data-conversion
  (:require [lisb.representation :refer [bsequence bset-enum btuple brecord
                                         b+-> b-->
                                         b+->> b-->>
                                         b>+> b>->
                                         b>+>> b>->>]]))

(defn convert [data btype [inner-type inner-rest]]
  (case btype
    nil data
    :bool data
    :int data
    :number data
    :literal data
    :set (apply bset-enum (map #(convert % inner-type inner-rest) data))
    :tuple (let [[l r] data
                 [lt lr] inner-type
                 [rt rr] inner-rest]
             (btuple (convert l lt lr) (convert r rt rr)))
    :sequence (apply bsequence (map #(convert % inner-type inner-rest) data))
    :record (let [ks (keys inner-type)]
              (apply brecord (mapcat (fn [k] [k (convert (data k) (first (inner-type k)) (second (inner-type k)))]) ks)))
    :fn (apply bset-enum (map #(convert % :tuple [inner-type inner-rest]) data))))
