(ns lisb.generation.lisb-gens
  (:require [lisb.translation.lisb2ir :refer :all])
  (:require [clojure.test.check :as tc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.properties :as prop]))


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

(def union-inter-set-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[unite-sets intersect-sets])
                       (gen/set set-gen {:max-elements 2}))))


;; relations

(def set-relation-gen
  (gen/bind (gen/elements '[<-> relation <<-> <->> <<->>])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/vector set-gen 2 3)))))

(def maplet-relation-gen
  (gen/bind (gen/elements '[|-> maplet])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/vector (gen/one-of [number-gen
                                                 boolean-gen])
                                    1 3)))))

(def identity-relation-gen
  (gen/fmap (partial cons 'id)
            (gen/fmap list set-gen)))

(def projection-relation-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[prj1 prj2])
                       set-gen
                       set-gen)))

(def basic-relation-gen
  (gen/one-of [set-relation-gen
               maplet-relation-gen
               identity-relation-gen
               projection-relation-gen]))

(def domain-relation-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[<| domain-restriction
                                       <<| domain-subtraction])
                       set-gen
                       basic-relation-gen)))

(def range-relation-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[|> range-restriction
                                       |>> range-subtraction])
                       basic-relation-gen
                       set-gen)))

(def inverse-relation-gen
  (gen/fmap (partial cons 'inverse)
            (gen/fmap list basic-relation-gen)))

(def closure-relation-gen
  (gen/bind (gen/elements '[closure1 closure])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/fmap list basic-relation-gen)))))

(def simple-relation-gen
  (gen/one-of [basic-relation-gen
               domain-relation-gen
               range-relation-gen
               inverse-relation-gen
               closure-relation-gen]))

(def relation-expression-gen
  (gen/bind (gen/elements '[override <+
                            direct-product ><
                            composition
                            parallel-product])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/vector simple-relation-gen 0 3)))))

(def relation-gen
  (gen/one-of [simple-relation-gen
               relation-expression-gen]))

(def domain-range-gen
  (gen/bind (gen/elements '[dom ran])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/fmap list relation-gen)))))

(def image-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'image)
                       relation-gen
                       set-gen)))

(def iterate-relation-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'iterate)
                       relation-gen
                       simple-number-gen)))

(def translate-relation-gen
  (gen/bind (gen/elements '[fnc rel])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/fmap list relation-gen)))))


;; functions

(def set-function-gen
  (gen/bind (gen/elements '[+-> partial-function
                            --> total-function
                            +->> partial-surjection
                            -->> total-surjection
                            >+> partial-injection
                            >-> total-injection
                            >+>> partial-bijection
                            >->> total-bijection])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/vector set-gen 0 3)))))

(def lambda-id-gen
  (gen/fmap keyword
            (gen/not-empty (gen/resize 5 gen/string-alphanumeric))))

(def lambda-function-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'lambda)
                       (gen/vector lambda-id-gen 1 3)
                       (gen/one-of [number-predicate-gen set-predicate-gen])
                       (gen/one-of [number-expression-gen set-expression-gen]))))

(def function-gen
  (gen/one-of [set-function-gen
               lambda-function-gen]))

(def function-name-gen
  (gen/fmap keyword
            (gen/not-empty (gen/resize 10 gen/string-alphanumeric))))

(def function-arg-gen
  (gen/one-of [simple-number-gen
               true-false-gen]))

(def function-call-gen
  (gen/fmap (partial apply concat)
            (gen/tuple (gen/fmap list (gen/return 'fn-call))
                       (gen/fmap list (gen/one-of [function-name-gen
                                                   function-gen]))
                       (gen/vector function-arg-gen 0 3))))


;; sequences

(def sequence-element-gen
  (gen/one-of [simple-number-gen
               true-false-gen]))

(def basic-sequence-gen
  (gen/fmap (partial cons 'sequence)
            (gen/vector sequence-element-gen 0 3)))

(def concat-sequence-gen
  (gen/fmap (partial cons 'concat)
            (gen/vector basic-sequence-gen 2 3)))

(def prepend-sequence-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[-> prepend])
                       sequence-element-gen
                       basic-sequence-gen)))

(def append-sequence-gen
  (gen/bind (gen/elements '[<- append])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/vector sequence-element-gen 1 3)))))

(def reverse-front-tail-gen
  (gen/bind (gen/elements '[reverse
                            front drop-last
                            tail rest])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/fmap list basic-sequence-gen)))))

(def take-drop-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[take drop])
                       simple-number-gen
                       basic-sequence-gen)))

(def sequence-gen
  (gen/one-of [basic-sequence-gen
               concat-sequence-gen
               prepend-sequence-gen
               append-sequence-gen
               reverse-front-tail-gen
               take-drop-gen]))

(def sequence-set-gen
  (gen/bind (gen/elements '[seq seq1 iseq iseq1 perm])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/fmap list set-gen)))))

(def sequence-size-gen
  (gen/fmap (partial cons 'size)
            (gen/fmap list sequence-gen)))

(def first-last-gen
  (gen/bind (gen/elements '[first last])
            (fn [op]
              (gen/fmap (partial cons op)
                        (gen/fmap list sequence-gen)))))

(def concat-sequence-of-sequences-gen
  (gen/fmap (partial cons 'conc)
            (gen/fmap list
                      (gen/fmap (partial cons 'sequence)
                                (gen/vector sequence-gen 0 3)))))


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


;; equality

(def equality-element-gen
  (gen/one-of [number-gen
               boolean-gen
               basic-predicate-gen
               logical-predicate-gen]))

(def equal-not-euqal-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[= not=])
                       equality-element-gen
                       equality-element-gen)))

(def distinct-gen
  (gen/fmap (partial cons 'distinct?)
            (gen/vector equality-element-gen 2 3)))

(def equality-gen
  (gen/one-of [equal-not-euqal-gen
               distinct-gen]))


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
  (gen/one-of [;; machines
               machine-gen
               refinement-implementation-gen
               ;; basics
               number-gen
               boolean-gen
               set-gen
               relation-gen
               function-gen
               sequence-gen
               logical-predicate-gen
               equality-gen
               ;; TODO: remove when used in some other generator
               card-gen
               union-inter-set-gen
               domain-range-gen
               image-gen
               iterate-relation-gen
               translate-relation-gen
               function-call-gen
               sequence-set-gen
               sequence-size-gen
               first-last-gen
               concat-sequence-of-sequences-gen]))


;; testing
(def last-lisb (atom ()))
(defn test-gen [gen]
  (let [x (gen/generate gen)]
    (reset! last-lisb x)
    (lisb->ir x)))

(defn check-gen [num-tests gen]
  (let [gen-prop (prop/for-all [v gen]
                               (lisb->ir v)
                               true)]
    (get-in (tc/quick-check num-tests gen-prop) [:shrunk :smallest])))

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
(test-gen union-inter-set-gen)

(test-gen set-relation-gen)
(test-gen maplet-relation-gen)
(test-gen identity-relation-gen)
(test-gen projection-relation-gen)
(test-gen basic-relation-gen)
(test-gen domain-relation-gen)
(test-gen range-relation-gen)
(test-gen inverse-relation-gen)
(test-gen closure-relation-gen)
(test-gen simple-relation-gen)
(test-gen relation-expression-gen)
(test-gen relation-gen)
(test-gen domain-range-gen)
(test-gen image-gen)
(test-gen iterate-relation-gen)
(test-gen translate-relation-gen)

(test-gen set-function-gen)
(test-gen lambda-id-gen)
(test-gen lambda-function-gen)
(test-gen function-gen)
(test-gen function-name-gen)
(test-gen function-arg-gen)
(test-gen function-call-gen)

(test-gen sequence-element-gen)
(test-gen basic-sequence-gen)
(test-gen concat-sequence-gen)
(test-gen prepend-sequence-gen)
(test-gen append-sequence-gen)
(test-gen reverse-front-tail-gen)
(test-gen take-drop-gen)
(test-gen sequence-gen)
(test-gen sequence-set-gen)
(test-gen sequence-size-gen)
(test-gen first-last-gen)
(test-gen concat-sequence-of-sequences-gen)

(test-gen basic-predicate-gen)
(test-gen pos-logical-predicate-gen)
(test-gen neg-logical-predicate-gen)
(test-gen logical-predicate-gen)

(test-gen equality-element-gen)
(test-gen equal-not-euqal-gen)
(test-gen distinct-gen)
(test-gen equality-gen)

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
(check-gen 1000 lisb-gen)

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
