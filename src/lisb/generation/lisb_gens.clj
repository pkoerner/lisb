(ns lisb.generation.lisb-gens
  (:require [lisb.translation.lisb2ir :refer :all])
  (:require [clojure.test.check.generators :as gen]))


;; numbers

(def num-gen (gen/one-of 
              [(gen/fmap keyword (gen/not-empty (gen/resize 10 gen/string-alphanumeric)))
               gen/large-integer]))
(def num-expr-gen (gen/recursive-gen
                   (fn [inner]
                     (gen/fmap list*
                               (gen/tuple (gen/elements '[+ - * /])
                                          inner
                                          inner)))
                   num-gen))
(def num-pred-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[> < >= <=])
                       num-expr-gen
                       num-expr-gen)))


;; sets

(def simple-num-set-gen
  (gen/fmap set
            (gen/vector gen/large-integer)))

(def complex-num-set-gen (gen/recursive-gen
                      (fn [inner]
                        (gen/one-of
                         [(gen/fmap list*
                                     (gen/tuple (gen/elements '[pow pow1 fin fin1])
                                                inner))
                          (gen/fmap list*
                                    (gen/tuple (gen/elements '[cartesian-product
                                                               union
                                                               intersection
                                                               set-])
                                               inner
                                               inner))]))
                     simple-num-set-gen))

(def num-set-pos-pred-gen
  (gen/one-of [(gen/fmap list*
                         (gen/tuple (gen/elements '[subset? superset?
                                                    strict-subset? strict-superset?])
                                    complex-num-set-gen
                                    complex-num-set-gen))
               (gen/fmap list*
                         (gen/tuple (gen/return 'member?)
                                    complex-num-set-gen
                                    num-expr-gen))]))

(def num-set-neg-pred-gen
  (gen/fmap list*
            (gen/tuple (gen/return 'not)
                       num-set-pos-pred-gen)))

(def num-set-pred-gen (gen/one-of [num-set-pos-pred-gen num-set-neg-pred-gen]))


;; logical predicates

(def single-pred-gen
  (gen/one-of [num-pred-gen num-set-pred-gen]))

(def pred-gen
  (gen/recursive-gen (fn [inner]
                       (gen/one-of [(gen/fmap list*
                                              (gen/tuple (gen/elements '[and or => <=>])
                                                         inner
                                                         inner))
                                    (gen/fmap list*
                                              (gen/tuple (gen/return 'not)
                                                         inner))]))
                     single-pred-gen))


;; relations

(def set-rel-gen
  (gen/fmap list*
            (gen/tuple (gen/elements '[<-> <<-> <->> <<->>])
                       complex-num-set-gen
                       complex-num-set-gen)))

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

(gen/generate simple-num-set-gen)
(gen/generate complex-num-set-gen)
(gen/generate num-set-neg-pred-gen)

(defn test-gen [g]
  (let [x (gen/generate g)]
    (lisb->ir x)))
(test-gen num-gen)
(test-gen num-expr-gen)
(test-gen num-pred-gen)

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

(lisb->ir '(not (subset? #{2} #{1 2})))
(lisb->ir '(pow (pow #{1 2 3})))
