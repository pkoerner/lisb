(ns lisb.translation.util
  (:require
    [lisb.translation.b2ast :refer [b->ast b-predicate->ast b-expression->ast b-formula->ast b-substitution->ast b-operation->ast]]
    [lisb.translation.ast2lisb :refer [ast->lisb]]
    [lisb.translation.lisb2ir :refer [lisb->ir]]
    [lisb.translation.ir2ast :refer [ir->ast]]
    [lisb.translation.ast2b :refer [ast->b]]))

(defn b->lisb [b] (ast->lisb (b->ast b)))
(defn b-predicate->lisb [b-predicate] (ast->lisb (b-predicate->ast b-predicate)))
(defn b-expression->lisb [b-expression] (ast->lisb (b-expression->ast b-expression)))
(defn b-formula->lisb [b-formula] (ast->lisb (b-formula->ast b-formula)))
(defn b-substitution->lisb [b-substitution] (ast->lisb (b-substitution->ast b-substitution)))
(defn b-operation->lisb [b-operation] (ast->lisb (b-operation->ast b-operation)))

(defn b->ir [b] (lisb->ir (b->lisb b)))
(defn b-predicate->ir [b-predicate] (lisb->ir (b-predicate->lisb b-predicate)))
(defn b-expression->ir [b-expression] (lisb->ir (b-expression->lisb b-expression)))
(defn b-formula->ir [b-formula] (lisb->ir (b-formula->lisb b-formula)))
(defn b-substitution->ir [b-substitution] (lisb->ir (b-substitution->lisb b-substitution)))
(defn b-operation->ir [b-operation] (lisb->ir (b-operation->lisb b-operation)))

(defn lisb->ast [lisb] (ir->ast (lisb->ir lisb)))

(defn lisb->b [lisb] (ast->b (lisb->ast lisb)))

(defn ir->b [ir]
  (ast->b (ir->ast ir)))

(defn ast->ir [ast]
  (lisb->ir (ast->lisb ast)))
