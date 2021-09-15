(ns lisb.fset.transform
  (:require
    [com.rpl.specter :as s]
    [lisb.core :refer [eval-ir-formula]]))

(defn get-node-by-tag
  [el-tag ir]
  (s/select [(s/walker #(= (:tag %) el-tag))] ir))

(defn update-node-by-tag
  [el-tag update-fn m]
  (s/transform [:ir (s/walker #(= (:tag %) el-tag))] update-fn m))

(defn transform-sets
  [m]
  m)

(defn transform-definitions
  [m]
  m)

(defn transform-invariant
  [m]
  m)

(defn transform-operations
  [m]
  m)

(defn transform
  [m]
  (->> (assoc m :meta {})
       (transform-sets)
       (transform-definitions)
       (transform-invariant)
       (transform-operations)))
