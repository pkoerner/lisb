(ns lisb.translation
  (:require [clojure.walk :refer [walk]])
  (:import (de.be4.classicalb.core.parser.node AAddExpression
                                               AMinusExpression
                                               AUnaryMinusExpression
                                               AIntegerExpression
                                               ABooleanTrueExpression
                                               ABooleanFalseExpression
                                               AConvertBoolExpression
                                               AIdentifierExpression
                                               AEmptySetExpression
                                               ASetExtensionExpression
                                               AComprehensionSetExpression
                                               TIntegerLiteral
                                               TIdentifierLiteral
                                               AConjunctPredicate
                                               ADisjunctPredicate
                                               ANegationPredicate
                                               AEqualPredicate
                                               ANotEqualPredicate
                                               AEquivalencePredicate
                                               ALessPredicate)))

(declare to-ast)

(defn identifier [n]
  (let [token (TIdentifierLiteral. (name n))]
    (AIdentifierExpression. [token])))

(defn integer [n]
  (let [token (TIntegerLiteral. (str n))]
    (AIntegerExpression. token)))

(defn set-literal [s]
  (if-not (seq s)
    (AEmptySetExpression.)
    (ASetExtensionExpression. (mapcat to-ast s))))


(defn boolean-true []
  (ABooleanTrueExpression.))

(defn boolean-false []
  (ABooleanFalseExpression.))

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

(defn not-node [p]
  (ANegationPredicate. p))

(defn not-equals-node [l r]
  (ANotEqualPredicate. l r))

(defn to-bool-node [p]
  (AConvertBoolExpression. p))

(defn comprehension-set-node [v p]
  (let [vars (map identifier v)]
    (AComprehensionSetExpression. vars p)))

(defn literal [x]
  (cond (keyword? x) (identifier x)
        (number? x) (integer x)
        (true? x) (boolean-true)
        (false? x) (boolean-false)
        (set? x) (set-literal x)
        :otherwise x))

(def to-ast-map {:less less-node
                 :plus plus-node
                 :and conjunction-node
                 :minus minus-node
                 :unaryminus unaryminus-node
                 :equals equal-node
                 :equivalence equivalence-node
                 :or disjunction-node
                 :not not-node
                 :not-equals not-equals-node
                 :to-bool to-bool-node
                 :comp-set comprehension-set-node
                 })


(defn to-ast-inner [data]
  (if (map? data)
      (let [processed-args (walk to-ast-inner identity (:children data))]
        (apply (to-ast-map (:tag data)) processed-args))
      (literal data)))


(defn to-ast [data]
  (walk to-ast-inner  identity [ data]))
