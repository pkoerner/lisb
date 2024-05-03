(ns lisb.generation.lisb-gens
  (:require [lisb.translation.lisb2ir :refer :all]
            [lisb.examples.simple :as simple])
  (:require [clojure.test.check.generators :as gen]))


;; numbers

;; TODO: product/sum

(def min-max-integer-gen
  (gen/elements '[min-int max-int]))

(def number-literal-gen
  (gen/one-of [gen/large-integer
               (gen/fmap keyword
                         (gen/not-empty (gen/resize 10 gen/string-alphanumeric)))]))

(def number-neighbour-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[successor inc predecessor dec])
                       (gen/one-of [min-max-integer-gen
                                    number-literal-gen]))))

(def simple-number-gen
  (gen/one-of [min-max-integer-gen
               number-literal-gen
               number-neighbour-gen]))

(def number-expression-gen
  (gen/resize 3 (gen/recursive-gen
                 (fn [inner]
                   (gen/bind (gen/elements '[+ - * / ** mod])
                             (fn [op]
                               (gen/fmap (partial cons op)
                                         (gen/vector inner 2 5)))))
                 simple-number-gen)))

(def integer-set-gen
  (gen/elements '[integer-set natural-set natural1-set
                  int-set nat-set nat1-set]))

(def min-max-set-gen
  (gen/bind (gen/elements '[min max])
            (fn [op]
              (gen/one-of [(gen/fmap (partial cons op)
                                     (gen/tuple integer-set-gen))
                           (gen/fmap (partial cons op)
                                     (gen/vector simple-number-gen 2 5))]))))

(def number-gen
  (gen/one-of [simple-number-gen
               number-expression-gen
               min-max-set-gen]))

(def interval-set-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[interval range])
                       number-gen
                       number-gen)))

(def number-predicate-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[> < >= <=])
                       number-gen
                       number-gen)))


;; booleans

(def true-false-gen
  (gen/elements '[true false]))

(def boolean-set-gen
  (gen/return 'bool-set))

(def predicate-to-boolean-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'pred->bool)
                       number-predicate-gen)))

(def boolean-gen
  (gen/one-of [true-false-gen
               predicate-to-boolean-gen]))


;; sets

;; TODO: comprehension-set; union/intersect with predicate

(def basic-set-gen
  (gen/one-of [(gen/fmap set (gen/vector number-gen))
               integer-set-gen
               interval-set-gen
               (gen/fmap set (gen/vector boolean-gen))
               boolean-set-gen]))

(def pow-fin-set-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[pow pow1 fin fin1])
                       basic-set-gen)))

(def simple-set-gen
  (gen/one-of [basic-set-gen
               pow-fin-set-gen]))

(def set-expression-gen
  (gen/bind (gen/elements '[cartesian-product
                            union
                            intersection
                            set-])
            (fn [op] (gen/fmap (partial cons op)
                               (gen/vector simple-set-gen 2 5)))))

(def set-gen
  (gen/one-of [simple-set-gen
               set-expression-gen]))

(def card-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'card)
                       set-gen)))

(def member-gen
  (gen/one-of [(gen/fmap list*
                         (gen/tuple (gen/elements '[member? in])
                                    (gen/one-of [number-gen boolean-gen])
                                    basic-set-gen))
               (gen/fmap list*
                         (gen/tuple (gen/return 'contains?)
                                    basic-set-gen
                                    (gen/one-of [number-gen boolean-gen])))]))

(def subset-superset-gen
  (gen/bind (gen/elements '[subset? superset?
                            strict-subset? strict-superset?])
            (fn [op] (gen/fmap (partial cons op)
                               (gen/vector set-gen 2 5)))))

(def pos-set-predicate-gen
  (gen/one-of [member-gen
               subset-superset-gen]))

(def neg-set-predicate-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'not)
                       pos-set-predicate-gen)))

(def set-predicate-gen
  (gen/one-of [pos-set-predicate-gen
               neg-set-predicate-gen]))

(def union-inter-set
  (gen/fmap list*
            (gen/tuple (gen/elements '[unite-sets intersect-sets])
                       (gen/set set-gen {:max-elements 2}))))


;; logical predicates

(def single-pred-gen
  (gen/one-of [number-predicate-gen set-predicate-gen]))

(def pred-gen
  (gen/resize 3 (gen/recursive-gen (fn [inner]
                                     (gen/one-of [(gen/bind (gen/elements '[and or => <=>])
                                                            (fn [op] (gen/fmap (partial cons op)
                                                                               (gen/vector inner 2 5))))
                                                  (gen/fmap list*
                                                            (gen/tuple (gen/return 'not)
                                                                       inner))]))
                                   single-pred-gen)))


;; relations

(def set-rel-gen
  (gen/bind (gen/elements '[<-> <<-> <->> <<->>])
            (fn [op] (gen/fmap (partial cons op)
                               (gen/vector set-gen 2 5)))))

(def domain-rel-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[<| <<|])
                       set-gen
                       set-rel-gen)))

(def range-rel-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[|> |>>])
                       set-rel-gen
                       set-gen)))

(def rel-gen
  (gen/one-of [set-rel-gen domain-rel-gen range-rel-gen]))


;; testing

(defn test-gen [g]
  (let [x (gen/generate g)]
    (lisb->ir x)))

(test-gen min-max-integer-gen)
(test-gen number-literal-gen)
(test-gen number-neighbour-gen)
(test-gen simple-number-gen)
(test-gen number-expression-gen)
(test-gen integer-set-gen)
(test-gen min-max-set-gen)
(test-gen number-gen)
(test-gen interval-set-gen)
(test-gen number-predicate-gen)

(test-gen true-false-gen)
(test-gen boolean-set-gen)
(test-gen predicate-to-boolean-gen)
(test-gen boolean-gen)

(test-gen basic-set-gen)
(test-gen pow-fin-set-gen)
(test-gen simple-set-gen)
(test-gen set-expression-gen)
(test-gen set-gen)
(test-gen card-gen)
(test-gen member-gen)
(test-gen subset-superset-gen)
(test-gen pos-set-predicate-gen)
(test-gen neg-set-predicate-gen)
(test-gen set-predicate-gen)
(test-gen union-inter-set)

(test-gen single-pred-gen)
(test-gen pred-gen)

(test-gen set-rel-gen)
(test-gen domain-rel-gen)
(test-gen range-rel-gen)
(test-gen rel-gen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gens calling each other
;; (declare gen-c-func)
;; (def pre-gen-c
;;   (gen/bind (gen/return nil)
;;             (fn [_] (gen-c-func))))
;; (def gen-a (gen/one-of [gen/keyword pre-gen-c]))
;; (def gen-b (gen/one-of [gen/int pre-gen-c]))
;; (def gen-c (gen/one-of [gen/string gen-a gen-b]))
;; (defn gen-c-func [] gen-c)
;; (gen/generate gen-a)
;; (gen/generate gen-b)
;; (gen/generate gen-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gens calling each other doesn't work; problem: not enough heap?
;; ;; forward declaration of gens
;; (declare simple-number-gen-func number-gen-func)
;; (def simple-number-pre-gen
;;   (gen/bind (gen/return nil)
;;             (fn [_] (simple-number-gen-func))))
;; (def number-pre-gen
;;   (gen/bind (gen/return nil)
;;             (fn [_] (number-gen-func))))


;; ;; numbers

;; (def integer-set-gen
;;   (gen/elements '[integer-set natural-set natural1-set
;;                   int-set nat-set nat1-set]))

;; (def interval-set-gen
;;   (gen/fmap list*
;;             (gen/tuple (gen/elements '[interval range])
;;                        number-pre-gen
;;                        number-pre-gen)))

;; (def min-max-integer-gen
;;   (gen/elements '[min-int max-int]))

;; (def number-predicate-gen
;;   (gen/fmap list*
;;             (gen/tuple (gen/elements '[> < >= <=])
;;                        number-pre-gen
;;                        number-pre-gen)))

;; ; TODO: min/max of set

;; (def number-expression-gen
;;   (gen/resize 3 (gen/recursive-gen
;;                  (fn [inner]
;;                    (gen/bind (gen/elements '[+ - * / ** mod])
;;                              (fn [op] (gen/fmap (partial cons op)
;;                                                 (gen/vector inner 2 5)))))
;;                  simple-number-pre-gen)))

;; ; TODO: product/sum

;; (def number-neighbour-gen
;;   (gen/fmap list*
;;             (gen/tuple (gen/elements '[successor inc predecessor dec])
;;                        number-pre-gen)))

;; (def number-literal-gen
;;   (gen/one-of [gen/large-integer
;;                (gen/fmap keyword
;;                          (gen/not-empty (gen/resize 10 gen/string-alphanumeric)))]))

;; (def simple-number-gen
;;   (gen/one-of [min-max-integer-gen
;;                number-literal-gen
;;                number-neighbour-gen]))
;; (defn simple-number-gen-func [] simple-number-gen)

;; (def number-gen
;;   (gen/one-of [min-max-integer-gen
;;                number-literal-gen
;;                number-neighbour-gen
;;                number-expression-gen]))
;; (defn number-gen-func [] number-gen)
