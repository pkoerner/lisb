(ns lisb.translation.eventb.specter-util
  (:require [com.rpl.specter :refer [if-path path must ALL]]))

(defn TAG [t]  #(= (:tag %) t))
(defn NAME [n]  #(= (:name %) n))
(def CLAUSES (if-path (must :ir)
                        [:ir :clauses]
                        [:machine-clauses]))
(defn CLAUSE [name] (path [CLAUSES ALL (TAG name)]))

(defn INCLUDES [n] (path [(CLAUSE :includes) :values ALL (NAME n)] ))
