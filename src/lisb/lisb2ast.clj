(ns lisb.lisb2ast
  (:require [lisb.representation :refer [lisb->node-repr]]
            [lisb.translation :refer [node-repr->ast node-repr->predicate-ast]]))

(defn lisb->ast [lisb]
  (node-repr->ast (lisb->node-repr lisb)))

(defn lisb-predicate->ast [lisb]
  (node-repr->predicate-ast (lisb->node-repr lisb)))
