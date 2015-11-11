(ns lisb.translation
  (:require [clojure.walk :refer [walk]])
  (:import (de.be4.classicalb.core.parser.node AAddExpression
                                               AMinusExpression
                                               AUnaryMinusExpression
                                               AIntegerExpression
                                               AIdentifierExpression
                                               TIntegerLiteral
                                               TIdentifierLiteral
                                               AConjunctPredicate
                                               ADisjunctPredicate
                                               AEqualPredicate
                                               AEquivalencePredicate
                                               ALessPredicate)))


(defn identifier [n]
  (let [token (TIdentifierLiteral. n)]
    (AIdentifierExpression. [token])))

(defn integer [n]
  (let [token (TIntegerLiteral. (str n))]
    (AIntegerExpression. token)))

(defn less-node [l r]
  (ALessPredicate. l r))

(defn plus-node [l r]
  (AAddExpression. l r))

(defn minus-node [l r]
  (AMinusExpression. l r))

(defn unaryminus-node [n]
  (AUnaryMinusExpression. n))

(defn conjunction-node [l r]
  (AConjunctPredicate. l r))

(defn equal-node [l r]
  (AEqualPredicate. l r))

(defn equivalence-node [l r]
  (AEquivalencePredicate. l r))

(defn disjunction-node [l r]
  (ADisjunctPredicate. l r))

(defn literal [x]
  (cond (keyword? x) (identifier (name x))
        (number? x) (integer x)))

(def to-ast-map {:less less-node
                 :plus plus-node
                 :and conjunction-node
                 :minus minus-node
                 :unaryminus unaryminus-node
                 :equals equal-node
                 :equivalence equivalence-node
                 :or disjunction-node
                 })


(defn to-ast-inner [data]
  (if (map? data)
      (let [processed-args (walk to-ast-inner identity (:children data))]
        (apply (to-ast-map (:tag data)) processed-args))
      (literal data)))


(defn to-ast [data]
  (walk to-ast-inner  identity [ data]))
