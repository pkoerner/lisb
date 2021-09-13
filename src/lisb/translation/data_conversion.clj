(ns lisb.translation.data-conversion
  (:require [lisb.translation.representation :refer [b bsequence bset-enum brecord]]))

(defn ensure-list [maybe-k]
  (if (keyword? maybe-k) [maybe-k] maybe-k))

(defn convert [data btype type-arg #_[inner-type inner-rest]]
  (if (or (not (#{:fn :set :tuple :sequence :record} btype)))
    data
    (let [[typel typer] (ensure-list type-arg)]
      (case btype
        :set (apply bset-enum (map #(convert % typel typer) data))
        :tuple (let [[l r] data
                     [lt lr] (ensure-list typel)
                     [rt rr] (ensure-list typer)]
                 [(convert l lt lr) (convert r rt rr)])
        :sequence (apply bsequence (map #(convert % typel typer) data))
        :record (let [ks (keys typel)]
                  (apply brecord
                         (mapcat (fn [k]
                                   (let [[typl typr] (ensure-list (typel k))]
                                     [k (convert (data k) typl typr)])) ks)))
        :fn (apply bset-enum (map #(convert % :tuple [typel typer]) data))))))
