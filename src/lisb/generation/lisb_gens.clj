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


;; testing

(gen/generate simple-num-set-gen)
(gen/generate complex-num-set-gen)

(defn test-gen [g]
  (let [x (gen/generate g)]
    (lisb->ir x)))
(test-gen num-gen)
(test-gen num-expr-gen)
(test-gen num-pred-gen)
(test-gen simple-num-set-gen)
(test-gen complex-num-set-gen)
(lisb->ir '('x #{1 2} #{1 2}))
(lisb->ir '(pow (pow #{1 2 3})))
