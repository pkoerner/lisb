(ns lisb.translation.eventb.specter-util
  (:require [com.rpl.specter :as s]
            [lisb.translation.irtools :refer [CLAUSE]]))

(defn NAME [n] (s/pred #(= (:name %) n)))
(defn INCLUDES [n] (s/path [(CLAUSE :includes) :values s/ALL (NAME n)]))
