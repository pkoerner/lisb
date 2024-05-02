(ns lisb.generation.lisb-gens
  (:require [lisb.translation.lisb2ir :refer :all])
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


;; sets

(def simple-num-set-gen
  (gen/one-of [(gen/fmap set (gen/vector gen/large-integer))
               integer-set-gen
               interval-set-gen]))

(def complex-num-set-gen (gen/resize 3 (gen/recursive-gen
                                        (fn [inner]
                                          (gen/one-of
                                           [(gen/fmap list*
                                                      (gen/tuple (gen/elements '[pow pow1 fin fin1])
                                                                 inner))
                                            (gen/bind (gen/elements '[cartesian-product
                                                                      union
                                                                      intersection
                                                                      set-])
                                                      (fn [op] (gen/fmap (partial cons op)
                                                                         (gen/vector inner 2 5))))]))
                                        simple-num-set-gen)))

(def num-set-pos-pred-gen
  (gen/one-of [(gen/bind (gen/elements '[subset? superset?
                                         strict-subset? strict-superset?])
                         (fn [op] (gen/fmap (partial cons op)
                                            (gen/vector complex-num-set-gen 2 5))))
               (gen/fmap list*
                         (gen/tuple (gen/return 'member?)
                                    number-gen
                                    complex-num-set-gen))]))

(def num-set-neg-pred-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'not)
                       num-set-pos-pred-gen)))

(def num-set-pred-gen (gen/one-of [num-set-pos-pred-gen num-set-neg-pred-gen]))


;; logical predicates

(def single-pred-gen
  (gen/one-of [number-predicate-gen num-set-pred-gen]))

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
                               (gen/vector complex-num-set-gen 2 5)))))

(def domain-rel-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[<| <<|])
                       complex-num-set-gen
                       set-rel-gen)))

(def range-rel-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[|> |>>])
                       set-rel-gen
                       complex-num-set-gen)))

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

(test-gen simple-num-set-gen)
(test-gen complex-num-set-gen)
(test-gen num-set-pos-pred-gen)
(test-gen num-set-neg-pred-gen)
(test-gen num-set-pred-gen)

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
