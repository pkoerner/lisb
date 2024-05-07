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
  (gen/resize 2 (gen/recursive-gen
                 (fn [inner]
                   (gen/bind (gen/elements '[+ - * / ** mod])
                             (fn [op]
                               (gen/fmap (partial cons op)
                                         (gen/vector inner 2 3)))))
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
  (gen/fmap (partial cons 'not)
            (gen/fmap list pos-set-predicate-gen)))

(def set-predicate-gen
  (gen/one-of [pos-set-predicate-gen
               neg-set-predicate-gen]))

(def union-inter-set
  (gen/fmap list*
            (gen/tuple (gen/elements '[unite-sets intersect-sets])
                       (gen/set set-gen {:max-elements 2}))))


;; logical predicates

;; TODO: universal/existanial quantification

(def basic-predicate-gen
  (gen/one-of [number-predicate-gen
               set-predicate-gen]))

(def pos-logical-predicate-gen
  (gen/resize 2 (gen/recursive-gen
                 (fn [inner]
                   (gen/bind (gen/elements '[and or => 'implication <=> 'equivalence])
                             (fn [op]
                               (gen/fmap (partial cons op)
                                         (gen/vector inner 2)))))
                 basic-predicate-gen)))

(def neg-logical-predicate-gen
  (gen/fmap (partial cons 'not)
            (gen/fmap list pos-logical-predicate-gen)))

(def logical-predicate-gen
  (gen/one-of [pos-logical-predicate-gen
               neg-logical-predicate-gen]))


;; relations

(def set-rel-gen
  (gen/bind (gen/elements '[<-> <<-> <->> <<->>])
            (fn [op] (gen/fmap (partial cons op)
                               (gen/vector set-gen 2 3)))))

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


;; machine name / operation name

(def machine-name-gen
  (gen/fmap keyword
            (gen/not-empty (gen/resize 10 gen/string-alphanumeric))))

(def machine-arg-gen
  (gen/one-of [simple-number-gen
               true-false-gen]))

(def machine-name-with-args-gen
  (gen/bind machine-name-gen
            (fn [name]
              (gen/fmap vec
                        (gen/fmap (partial cons name)
                                  (gen/vector machine-arg-gen 0 3))))))

(def operation-name-gen
  (gen/fmap keyword
            (gen/not-empty (gen/resize 10 gen/string-alphanumeric))))


;; machine inclusion

(def uses-sees-gen
  (gen/bind (gen/elements '[uses sees])
            (fn [type]
              (gen/fmap (partial cons type)
                        (gen/vector machine-name-gen 1 3)))))

(def includes-extends-gen
  (gen/bind (gen/elements '[includes extends])
            (fn [type]
              (gen/fmap (partial cons type)
                        (gen/vector machine-name-with-args-gen 1 3)))))

(def promotes-gen
  (gen/fmap (partial cons 'promotes)
            (gen/vector operation-name-gen 1 3)))

(def inclusion-gen
  (gen/one-of [uses-sees-gen
               includes-extends-gen
               promotes-gen]))


;; definitions

(def definitions-name-gen
  (gen/fmap keyword
            (gen/not-empty (gen/resize 10 gen/string-alphanumeric))))

(def definitions-arg-gen
  (gen/one-of [simple-number-gen
               true-false-gen]))

(def expression-definition-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'expression-definition)
                       definitions-name-gen
                       (gen/vector definitions-arg-gen 0 3)
                       (gen/one-of [number-expression-gen
                                    set-expression-gen]))))

(def predicate-definition-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'predicate-definition)
                       definitions-name-gen
                       (gen/vector definitions-arg-gen 0 3)
                       (gen/one-of [number-predicate-gen
                                    set-predicate-gen]))))

(def substitution-definition-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'substitution-definition)
                       definitions-name-gen
                       (gen/vector definitions-arg-gen 0 3)
                       (gen/return nil)))) ;; TODO: add real substitutions

(def file-definition-gen
  (gen/fmap (partial cons 'file-definition)
            (gen/fmap (fn [file-name]
                        (list (apply str (concat file-name ".dev"))))
                      (gen/not-empty (gen/resize 10 gen/string-alphanumeric)))))

(def definitions-gen
  (gen/fmap (partial cons 'definitions)
            (gen/vector (gen/one-of [expression-definition-gen
                                     predicate-definition-gen
                                     substitution-definition-gen
                                     file-definition-gen])
                        1
                        3)))


;; machine clauses

(def machine-clause-gen
  (gen/one-of [inclusion-gen
               definitions-gen
               ;; TODO: add more
               ]))


;; machine

(def machine-gen
  (gen/bind (gen/fmap list*
                      (gen/tuple (gen/elements '[machine model system])
                                 machine-name-with-args-gen))
            (fn [type]
              (gen/fmap (partial concat type)
                        (gen/vector machine-clause-gen 0 3)))))

(def refinement-implementation-gen
  (gen/bind (gen/fmap list*
                      (gen/tuple (gen/elements '[refinement
                                                 implementation])
                                 machine-name-with-args-gen
                                 machine-name-gen))
            (fn [type]
              (gen/fmap (partial concat type)
                        (gen/vector machine-clause-gen 0 3)))))


;; generate lisb

(def lisb-gen
  (gen/one-of [machine-gen
               refinement-implementation-gen
               ;; TODO: remove when used in some other generator
               card-gen
               union-inter-set
               logical-predicate-gen
               rel-gen]))


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

(test-gen basic-predicate-gen)
(test-gen pos-logical-predicate-gen)
(test-gen neg-logical-predicate-gen)
(test-gen logical-predicate-gen)

(test-gen set-rel-gen)
(test-gen domain-rel-gen)
(test-gen range-rel-gen)
(test-gen rel-gen)

(test-gen machine-name-gen)
(test-gen machine-arg-gen)
(test-gen machine-name-with-args-gen)
(test-gen operation-name-gen)

(test-gen uses-sees-gen)
(test-gen includes-extends-gen)
(test-gen promotes-gen)
(test-gen inclusion-gen)

(test-gen definitions-name-gen)
(test-gen definitions-arg-gen)
(test-gen expression-definition-gen)
(test-gen predicate-definition-gen)
(test-gen substitution-definition-gen)
(test-gen file-definition-gen)
(test-gen definitions-gen)

(test-gen machine-clause-gen)

(test-gen machine-gen)
(test-gen refinement-implementation-gen)

(test-gen lisb-gen)

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
