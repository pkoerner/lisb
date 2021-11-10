(ns lisb.examples.simple
  (:require [lisb.translation.lisb2ir :refer [lisb->ir b]]))

(def lift (b (machine
               :Lift
               (variables :etage)
               (invariants (contains? (interval 0 99) :etage))
               (init (assign :etage 4))
               (operations
                 (:inc [] (pre (< :etage 99) (assign :etage (+ :etage 1))))
                 (:dec [] (pre (> :etage 0) (assign :etage (- :etage 1))))))))

(def a-counter (b (machine
                    :ACounter
                    (variables :ii :jj)
                    (invariants (contains? (interval 0 10) :ii)
                                (contains? (interval 0 10) :jj)
                                (< :ii 11)
                                (>= :jj 0))
                    (init (assign :ii 2 :jj 10))
                    (operations
                      (:inc [] (select (> :jj 0) (parallel-sub
                                                                (assign :ii (+ :ii 1))
                                                                (assign :jj (- :jj 1)))))
                      (<-- [:result] (:res [] (assign :result :ii)))))))

(def gcd (b (machine
              :GCD
              (variables :x :y)
              (invariants (contains? nat-set :x) (contains? nat-set :y))
              (init (parallel-sub (assign :x 70) (assign :y 40)))
              (operations
                (<--[:s] (:GCDSolution [] (if-sub (= :y 0) (assign :s :x) (assign :s -1))))
                (:Step [] (if-sub (> :y 0) (parallel-sub (assign :x :y) (assign :y (mod :x :y)))))
                (:Restart [:w1 :w2] (pre (and (contains? nat1-set :w1) (contains? nat1-set :w2))
                                                      (if-sub (> :w1 :w2)
                                                              (assign :x :w1 :y :w2)
                                                              (assign :y :w1 :x :w2))))))))

(def knights-knaves (b (machine
                         :KnightsKnaves
                         (constants :A :B :C)
                         (properties
                           (contains? bool-set :A)
                           (contains? bool-set :B)
                           (contains? bool-set :C)
                           (<=> (= :A true) (or (= :B false) (= :C false)))
                           (<=> (= :B true) (= :A true))))))

(def bakery0 (b (machine
                  :Bakery0
                  (variables :aa)
                  (invariants (contains? (interval 0 2) :aa))
                  (init (assign :aa 0))
                  (operations
                    (:enter1 [] (select (= :aa 0) (assign :aa 1)))
                    (:enter2 [] (select (= :aa 0) (assign :aa 2)))
                    (:leave1 [] (select (= :aa 1) (assign :aa 0)))
                    (:leave2 [] (select (= :aa 2) (assign :aa 0)))
                    (:try1 [] skip)
                    (:try2 [] skip)))))

(def bakery1 (b (machine
                  :Bakery1
                  (variables :p1 :p2 :y1 :y2)
                  (invariants (contains? (interval 0 2) :p1)
                              (contains? (interval 0 2) :p2)
                              (contains? natural-set :y1)
                              (contains? natural-set :y2)
                              (=> (= :p1 2) (< :p2 2))
                              (=> (= :p2 2) (< :p1 2)))
                  (init (assign :p1 0 :p2 0 :y1 0 :y2 0))
                  (operations
                    (:try1 [] (select (= :p1 0) (parallel-sub (assign :p1 1) (assign :y1 (+ :y2 1)))))
                    (:enter1 [] (select (and (= :p1 1) (or (= :y2 0) (< :y1 :y2))) (assign :p1 2)))
                    (:leave1 [] (select (= :p1 2) (parallel-sub (assign :p1 0) (assign :y1 0))))
                    (:try2 [] (select (= :p2 0) (parallel-sub (assign :p2 1) (assign :y2 (+ :y1 1)))))
                    (:enter2 [] (select (and (= :p2 1) (or (= :y1 0) (< :y2 :y1))) (assign :p2 2)))
                    (:leave2 [] (select (= :p2 2) (parallel-sub (assign :p2 0) (assign :y2 0))))))))
