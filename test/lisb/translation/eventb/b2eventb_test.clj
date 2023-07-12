(ns lisb.translation.eventb.b2eventb-test
  (:require [clojure.test :refer [deftest are is]]
            [lisb.translation.util :refer [b]]
            [lisb.translation.eventb.util :refer [eventb]]
            [com.rpl.specter :as s]
            [lisb.translation.eventb.b2eventb :refer [sub->events]]))



(deftest sub->events-compare-clauses
  (are [clauses sub] (= (set clauses) (set (sub->events (eventb (event :test)) sub)))
    [(eventb (event :test (then (assign :x 42))))]
    (b (assign :x 42))
    [(eventb (event :test (then (becomes-element-of :x nat-set))))]
    (b (becomes-element-of :x nat-set))
    [(eventb (event :test (then (becomes-such :x (> :x' 10)))))]
    (b (becomes-such :x (> :x' 10)))
    [(eventb (event :test-then (then (assign :x 10)) (when (= :p 1))))
     (eventb (event :test-else (then (assign :x 5)) (when (not (= :p 1)))))]
    (b (if-sub (= :p 1) (assign :x 10) (assign :x 5)))
    ))

(def m0 (b (machine :m0
                    (sees :c0)
                    (variables :a)
                    (operations
                     (:op0 [:a] (if-sub :cond (assign :b :c) (assign :b :a)))
                     ))))

(def m1 (b (machine :m2
                    (sets :A :SET #{:c :d})
                    (constants :b)
                    (properties (= :a 3))
                    (includes :m0)
                    (operations
                     (:op1 [s] (becomes-element-of :c s))))))

(def m2 (b (machine :m2
                    (sets :A)
                    (constants :e)
                    (properties (= :a 3))
                    (includes :m0 :1)
                    (operations
                     (:op2 [] (|| (op-call :op0 3)
                                  (op-call :op1 nat-set)))))))


(comment


  (clojure.pprint/pprint (extract-machine (includes->inline m1 m0)))
  (clojure.pprint/pprint (extract-context (includes->inline m1 m0)))

  (extract-machine (includes->refinement m1 m0))
  (extract-context (includes->refinement m1 m0))

  (sub->events (eventb-event :foo)
               (b (|| (assign :a 1)
                      (assign :b 2)
                      (if-sub :cond1
                              (assign :c 1)
                              (assign :d 1))
                      (if-sub :cond2
                              (assign :e 1)
                              (assign :f 1)))))

  (sub->events (eventb-event :foo)
               (b (case :x
                    1 (assign :y 2)
                    2 (assign :y 3)
                    :eof (assign :y 4)
                    4 (assign :y 5)
                    )))

  (clojure.pprint/pp)

  (-> (sub->events (eventb (event :test))
                   (b (if-sub (= :p 1) (assign :x 10) (assign :x 5)))
                   )
      set
      #_clojure.pprint/pprint
      (= (set [(eventb (event :test-then (then (assign :x 10)) (when (= :p 1))))
          (eventb (event :test-else (then (assign :x 5)) (when (not (= :p 1)))))])))

#{(eventb (event :test-then (when (= :p 1)) (then (assign :x 10))))
     (eventb (event :test-else (when (not (= :p 1))) (then (assign :x 5))))}

  (sub->events (eventb-event :foo)
               (eventb (select :cond1 (assign :a 1)
                               :cond2 (assign :a 1)
                               (assign :else 42))))

  (sub->events (eventb-event :foo)
               (eventb (select :cond1 (assign :a 1)
                               :cond2 (assign :a 1)
                               :cond3 (assign :a 1)
                               :cond4 (if-sub :cond
                                              (assign :then 1)
                                              (if-sub :cond-2
                                                      (assign :else 1)
                                                      (assign :else 2))))))

  )
