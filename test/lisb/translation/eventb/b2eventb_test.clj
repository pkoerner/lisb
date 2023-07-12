(ns lisb.translation.eventb.b2eventb-test
  (:require [clojure.test :refer [deftest are is]]
            [lisb.translation.util :refer [b]]
            [lisb.translation.eventb.util :refer [eventb]]
            [lisb.translation.eventb.specter-util :refer :all]
            [com.rpl.specter :as s]
            [lisb.translation.eventb.b2eventb :refer [sub->events]]))

(def visit-all-sequences (s/recursive-path [] p (s/cond-path
                                         seq? (s/stay-then-continue [s/ALL p])
                                         ;;vector? (s/stay-then-continue [s/ALL p])
                                         map? [s/MAP-VALS p])))

(defn- cmp-events
  "Compares to list of events"
  [e1 e2]
  (let [path (s/stay-then-continue s/ALL :clauses)]
    (= (s/transform path set e1)
       (s/transform path set e2))))

(def cmp-path (s/stay-then-continue s/ALL :clauses))

(deftest sub->events-compare-clauses
  (are [sub events] (= (s/transform cmp-path set events)
                       (s/transform cmp-path set (sub->events (eventb (event :test)) sub)))
    (b (assign :x 42))
    [(eventb (event :test (then (assign :x 42))))]

    (b (becomes-element-of :x nat-set))
    [(eventb (event :test (then (becomes-element-of :x nat-set))))]

    (b (becomes-such :x (> :x' 10)))
    [(eventb (event :test (then (becomes-such :x (> :x' 10)))))]

    (b (if-sub (= :p 1) (assign :x 10) (assign :x 5)))
    [(eventb (event :test-then
                    (when (= :p 1))
                    (then (assign :x 10))))
     (eventb (event :test-else
                    (when (not (= :p 1)))
                    (then (assign :x 5))))]

    (b (case :x
         2 (assign :x 1)
         3 (assign :y 4 :x 2)
         4 (assign :y 5)
         (assign :x -1)
         ))
    [(eventb (event :test-2
                    (when (= 2 :x))
                    (then (assign :x 1))))
     (eventb (event :test-3
                    (when (= 3 :x))
                    (then (assign :y 4 :x 2))))
     (eventb (event :test-4
                    (when (= 4 :x))
                    (then (assign :y 5))))
     (eventb (event :test-caseelse
                    (when (not (in :x #{2, 3, 4})))
                    (then (assign :x -1))))]

    (b (select (> :x 10) (assign :a 1)
               (< :x 0) (assign :a -1)
               (assign :a 42)))
    [(eventb (event :test-select0
                    (when (> :x 10))
                    (then (assign :a 1))))
     (eventb (event :test-select1
                    (when (< :x 0))
                    (then (assign :a -1) )))
     (eventb (event :test-selectelse
                    (when
                     (not (> :x 10))
                      (not (< :x 0)))
                    (then (assign :a 42))))]

    (b (select :cond1 (assign :a 1)
               :cond2 (assign :a 2)
               :cond3 (if-sub :cond4
                              (assign :a 3)
                              (assign :a 4)
                              )))
    [(eventb (event :test-select0
                    (when :cond1)
                    (then (assign :a 1))))
     (eventb (event :test-select1
                    (when :cond2)
                    (then (assign :a 2) )))
     (eventb (event :test-select2-then
                    (when
                     :cond3
                     :cond4)
                    (then (assign :a 3))))
     (eventb (event :test-select2-else
                    (when
                     :cond3
                     (not :cond4))
                    (then (assign :a 4))))]

    (b (select :cond (assign :a 1)))
    [(eventb (event :test (when :cond) (then (assign :a 1))))]
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
                     (:op1 [:s] (becomes-element-of :c :s))))))

(def m2 (b (machine :m2
                    (sets :A)
                    (constants :e)
                    (properties (= :a 3))
                    (includes :m0 :1)
                    (operations
                     (:op2 [] (|| (op-call :op0 3)
                                  (op-call :op1 nat-set)))))))


(comment
  (require '[clojure.pprint :refer [pp pprint]])

  (clojure.pprint/pprint (extract-machine (includes->inline m1 m0)))
  (clojure.pprint/pprint (extract-context (includes->inline m1 m0)))

  (extract-machine (includes->refinement m1 m0))
  (extract-context (includes->refinement m1 m0))

  (pprint
       (s/transform visit-all-sequences set m1))
(s/select visit-all-sequences m1)

  (->> (sub->events (eventb ( event :foo))
               (b (|| (assign :a 1)
                      (assign :b 2)
                      (if-sub :cond1
                              (assign :c 1)
                              (assign :d 1))
                      (if-sub :cond2
                              (assign :e 1)
                              (assign :f 1)))))
       (s/transform visit-all-sequences set)
       pprint)

  (s/transform [(s/stay-then-continue s/ALL :clauses)] set (sub->events (eventb ( event :foo))
               (b (case :x
                    1 (assign :y 2)
                    2 (assign :y 3)
                    :eof (assign :y 4)
                    4 (assign :y 5)
                    ))))

  (clojure.pprint/pp)

  (-> (sub->events (eventb (event :test))
                   (b (if-sub (= :p 1) (assign :x 10) (assign :x 5)))
                   )
      set
      (cmp-events [(eventb (event :test-then (when (= :p 1)) (then (assign :x 10))))
          (eventb (event :test-else (when (not (= :p 1))) (then (assign :x 5))))]))

#{(eventb (event :test-then (when (= :p 1)) (then (assign :x 10))))
     (eventb (event :test-else (when (not (= :p 1))) (then (assign :x 5))))}


(sub->events (eventb-event :foo)
             (eventb (select :cond1 (assign :a 1)
                             :cond2 (assign :a 2)
                             :cond3 (if-sub :cond
                                            (assign :then 3)
                                            (assign :else 4)
                                            ))))

  )
