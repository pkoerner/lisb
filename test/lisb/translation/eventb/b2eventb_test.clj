(ns lisb.translation.eventb.b2eventb-test
  (:require [clojure.test :refer :all]
            [lisb.translation.util :refer [b]]
            [lisb.translation.eventb.util :refer [eventb]]
            [lisb.translation.eventb.specter-util :refer [CLAUSE]]
            [com.rpl.specter :as s]
            [lisb.translation.eventb.b2eventb :refer [sub->events] :as sut]))

(defn- cmp-events
  "Compares to list of events"
  [e1 e2]
  (let [path (s/stay-then-continue s/ALL :clauses)]
    (= (s/transform path set e1)
       (s/transform path set e2))))


(deftest sub->events-compare-clauses
  (are [sub events] (cmp-events (sub->events (eventb (event :test)) sub) events)

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

    (b (any [:x :y] (> :x 2) (assign :foo :x)))
    [(eventb (event :test
                    (any :x :y)
                    (when (> :x 2))
                    (then (assign :foo :x))))]

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
    [(eventb (event :test (when :cond) (then (assign :a 1))))])
    )


(deftest parallel-sub-test
  (are [sub events] (cmp-events (sub->events (eventb (event :test)) sub) events)

    (b (|| (assign :x 1) (becomes-element-of :y nat-set) (becomes-such [:z] (> :z' 10))))
    [(eventb (event :test (then (assign :x 1) (becomes-element-of :y nat-set) (becomes-such [:z] (> :z' 10)))))]

    (b (|| (if-sub :cond
                   (assign :x 1)
                   (assign :x 2))
             (assign :y 3)))
    [(eventb (event :test-then (when :cond) (then (assign :x 1) (assign :y 3))))
     (eventb (event :test-else (when (not :cond)) (then (assign :x 2) (assign :y 3))))]
    ))

(def train
  (b (machine :train
              (sets :PERSON :TRAIN #{:t1 :t2 :t3})
              (constants :c)
              (properties (= :c 10))
              (variables :inside)
              (invariants
               (subset? :inside :PERSON)
               (< (card :inside) :c))
              (operations
               (:enter [:p]
                       (pre (in :p :PERSON)
                            (if-sub (= (+ (card :inside) 1) :c)
                              (becomes-such [:inside] (subset? :inside' :inside))
                              (assign (union :inside :inside #{:p})))))))))

(def machine-cmp-path [:machine-clauses
                (s/stay-then-continue
                 s/ALL :values
                 (s/stay-then-continue
                  s/ALL (s/if-path #(and (map? %) (:clauses %))
                                   :clauses)))])

(deftest extract-context-test
  (are [in out] (= (s/transform machine-cmp-path set (sut/extract-context in))
                   (s/transform machine-cmp-path set out))
    (b (machine :m0
                (sets :PERSON)
                (constants :invited)
                (properties (subset? :invited :PERSON))))
    (eventb (context :m0-ctx
              (sets :PERSON)
              (constants :invited)
              (axioms (subset? :invited :PERSON))))

    (b (machine :m0
                (sets :TRAIN #{:t1 :t2 :t3})))
    (eventb (context :m0-ctx
              (sets :TRAIN)
              (constants :t1 :t2 :t3)
              (axioms (partition :TRAIN #{:t2} #{:t3} #{:t1}))))

    train
    (eventb (context :train-ctx
              (sets :PERSON :TRAIN)
              (constants :c :t1 :t2 :t3)
              (axioms
                (= :c 10)
                (partition :TRAIN #{:t2} #{:t3} #{:t1}))))))

(deftest extract-context-test2
  (are [in out] (= (s/transform machine-cmp-path set (sut/extract-context in))
                   (s/transform machine-cmp-path set out))
    (b (machine :m0
                (sets :PERSON)
                (constants :invited)
                (properties (subset? :invited :PERSON))))
    (eventb (context :m0-ctx
              (sets :PERSON)
              (constants :invited)
              (axioms (subset? :invited :PERSON))))

    (b (machine :m0
                (sets :TRAIN #{:t1 :t2 :t3})))
    (eventb (context :m0-ctx
              (sets :TRAIN)
              (constants :t1 :t2 :t3)
              (axioms (partition :TRAIN #{:t2} #{:t3} #{:t1}))))

    train
    (eventb (context :train-ctx
              (sets :PERSON :TRAIN)
              (constants :c :t1 :t2 :t3)
              (axioms
                (= :c 10)
                (partition :TRAIN #{:t2} #{:t3} #{:t1}))))))

(deftest extract-machine-test
  (are [in out] (= (s/transform machine-cmp-path set (sut/extract-machine in))
                   (s/transform machine-cmp-path set out))
    (b (machine :m0
                (sets :PERSON)
                (constants :invited)
                (properties (subset? :invited :PERSON))))
    (eventb (machine :m0 (sees :m0-ctx)))


    train
    (eventb (machine :train
                     (sees :train-ctx)
                     (variables :inside)
                     (invariants
                      (subset? :inside :PERSON)
                      (< (card :inside) :c))
                     (events
                      (event :enter-then
                             (any :p )
                             (when (in :p :PERSON)
                               (= (+ (card :inside) 1) :c))
                             (then
                              (becomes-such [:inside] (subset? :inside' :inside))))
                      (event :enter-else
                             (any :p)
                             (when
                               (in :p :PERSON)
                               (not (= (+ (card :inside) 1) :c)))
                             (then (assign (union :inside :inside #{:p})))))))))

(def m0 (b (machine :m0
                    (constants :c)
                    (variables :a)
                    (operations
                     (:op0 [:a] (if-sub :cond (assign :b :c) (assign :b :a)))
                     ))))

(def m1 (b (machine :m1
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

(deftest inclusion-test
  (are [in out] (= (s/transform machine-cmp-path set (sut/extract-machine in))
                   (s/transform machine-cmp-path set out))
    (sut/includes->inline (b (machine :m1
                                      (includes :m0 :m01)
                                      (promotes :op0 :op01)))
                          m0)
    (sut/extract-machine (assoc m0 :name :m1))))

(comment
  (require '[clojure.pprint :refer [pprint]])

  (def visit-all-sequences (s/recursive-path [] p (s/cond-path
                                                   seq? (s/stay-then-continue [s/ALL p])
                                           ;;vector? (s/stay-then-continue [s/ALL p])
                                                   map? [s/MAP-VALS p])))
  
  (pprint (sut/extract-machine (sut/includes->inline m1 m0)))

  (sut/update-clause-values m0 :operations concat (map {:op1 {:tag :op :name :op1} :op2 {:tag :op :name :op2}} [:op1 :op2]))

  (apply concat (s/select [(CLAUSE :operations) :values s/ALL] m0) [[:op1 :op2]])

  (sub->events (eventb (event :foo))
               (b (if-sub (> :a 1)
                    (any [:x] (> :x 2) (select :cond1 (assign :a 1)
                               :cond2 (assign :a 2)
                               :cond3 (if-sub :cond
                                              (assign :then 3)
                                              (assign :else 4)
                                              )))
                    (assign :then 42))))
  )
