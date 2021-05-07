(ns lisb.lisb2ast
  (:require [lisb.representation :refer [lisb->node-repr, lisb->node-repr1]]
            [lisb.translation :refer [node-repr->ast node-repr->predicate-ast node-repr->expression-ast node-repr->substitution-ast node-repr->machine-clause-ast]]))

(defn lisb->ast [lisb]
  (node-repr->ast (lisb->node-repr lisb)))

(defn lisb-predicate->ast [lisb]
  (node-repr->predicate-ast (lisb->node-repr lisb)))

(defn lisb-expression->ast [lisb]
  (node-repr->expression-ast (lisb->node-repr lisb)))

(defn lisb-substitution->ast [lisb]
  (node-repr->substitution-ast (lisb->node-repr lisb)))

(defn lisb-machine-clause->ast [lisb]
  (node-repr->machine-clause-ast (lisb->node-repr lisb)))
