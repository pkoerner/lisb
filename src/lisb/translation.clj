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

(defn lessnode [l r]
  (ALessPredicate. l r))

(defn plusnode [l r]
  (AAddExpression. l r))

(defn minusnode [l r]
  (AMinusExpression. l r))

(defn unaryminusnode [n]
  (AUnaryMinusExpression. n))

(defn conjunctionnode [l r]
  (AConjunctPredicate. l r))

(defn equalnode [l r]
  (AEqualPredicate. l r))

(defn equivalencenode [l r]
  (AEquivalencePredicate. l r))

(defn disjunction-node [l r]
  (ADisjunctPredicate. l r))

(defn literal [x]
  (cond (keyword? x) (identifier (name x))
        (number? x) (integer x)))

(def to-ast-map {:less lessnode
                 :plus plusnode
                 :and conjunctionnode
                 :minus minusnode
                 :unaryminus unaryminusnode
                 :equals equalnode
                 :equivalence equivalencenode
                 :or disjunction-node
                 })


(defn to-ast-inner [data]
  (if (map? data)
      (let [processed-args (walk to-ast-inner identity (:children data))]
        (apply (to-ast-map (:tag data)) processed-args))
      (literal data)))


(defn to-ast [data]
  (walk to-ast-inner  identity [ data]))
