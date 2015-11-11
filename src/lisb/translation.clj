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
                                               APowSubsetExpression
                                               APow1SubsetExpression
                                               AFinSubsetExpression
                                               AFin1SubsetExpression
                                               ACardExpression
                                               ACartesianProductExpression
                                               AUnionExpression
                                               AIntersectionExpression
                                               ASetSubtractionExpression
                                               AMemberPredicate
                                               ASubsetPredicate
                                               ASubsetStrictPredicate
                                               ABoolSetExpression
                                               ANaturalSetExpression
                                               ANatural1SetExpression
                                               AIntegerSetExpression
                                               ANatSetExpression
                                               ANat1SetExpression
                                               TIntegerLiteral
                                               TIdentifierLiteral
                                               AConjunctPredicate
                                               ADisjunctPredicate
                                               ANegationPredicate
                                               AEqualPredicate
                                               ANotEqualPredicate
                                               AEquivalencePredicate
                                               ALessPredicate
                                               AGreaterPredicate
                                               ALessEqualPredicate
                                               AGreaterEqualPredicate
                                               AMaxExpression
                                               )))

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

(defn greater-node [l r]
  (AGreaterPredicate. l r))

(defn less-eq-node [l r]
  (ALessEqualPredicate. l r))

(defn greater-eq-node [l r]
  (AGreaterEqualPredicate. l r))

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
  (let [vars (map identifier v)] ;; FIXME: see literal
    (AComprehensionSetExpression. vars p)))

(defn power-set-node [s]
  (APowSubsetExpression. s))

(defn power1-set-node [s]
  (APow1SubsetExpression. s))

(defn finite-subset-node [s]
  (AFinSubsetExpression. s))

(defn finite1-subset-node [s]
  (AFin1SubsetExpression. s))

(defn card-node [s]
  (ACardExpression. s))

(defn cartesian-product-node [l r]
  (ACartesianProductExpression. l r))

(defn set-union-node [l r]
  (AUnionExpression. l r))

(defn set-intersection-node [l r]
  (AIntersectionExpression. l r))

(defn set-difference-node [l r]
  (ASetSubtractionExpression. l r))

(defn member-node [e s]
  (AMemberPredicate. e s))

(defn subset-node [l r]
  (ASubsetPredicate. l r))

(defn subset-strict-node [l r]
  (ASubsetStrictPredicate. l r))

(defn bool-set-node []
  (ABoolSetExpression.))

(defn natural-set-node []
  (ANaturalSetExpression.))

(defn natural1-set-node []
  (ANatural1SetExpression.))

(defn int-set-node []
  (AIntegerSetExpression.))

(defn nat-set-node []
  (ANatSetExpression.))

(defn nat1-set-node []
  (ANat1SetExpression.))

(defn max-node [s]
  (AMaxExpression. s))

(defn literal [x]
  (cond (keyword? x) (identifier x)
        (number? x) (integer x)
        (true? x) (boolean-true)
        (false? x) (boolean-false)
        (set? x) (set-literal x)
        :otherwise x)) ;; FIXME: can/should lists just be passed? looking at comprehension-sets

(def to-ast-map {:less less-node
                 :greater greater-node
                 :less-eq less-eq-node
                 :greater-eq greater-eq-node
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
                 :power-set power-set-node
                 :power1-set power1-set-node
                 :finite-subset finite-subset-node
                 :finite1-subset finite1-subset-node
                 :card card-node
                 :cartesian-product cartesian-product-node
                 :set-union set-union-node
                 :set-intersection set-intersection-node
                 :set-difference set-difference-node
                 :member member-node
                 :subset subset-node
                 :subset-strict subset-strict-node
                 :bool-set bool-set-node
                 :natural-set natural-set-node
                 :natural1-set natural1-set-node
                 :int-set int-set-node
                 :nat-set nat-set-node
                 :nat1-set nat1-set-node
                 :max max-node
                 })


(defn to-ast-inner [data]
  (if (map? data)
      (let [processed-args (walk to-ast-inner identity (:children data))]
        (apply (to-ast-map (:tag data)) processed-args))
      (literal data)))


(defn to-ast [data]
  (walk to-ast-inner  identity [ data]))
