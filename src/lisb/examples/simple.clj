(ns lisb.examples.simple)

(def lift '(machine
             (machine-variant)
             (machine-header :Lift ())
             (variables :etage)
             (invariants (member :etage (interval 0 99)))
             (init (block (assign :etage 4)))
             (operations
               (operation () :inc () (pre (< :etage 99) (assign :etage (+ :etage 1))))
               (operation () :dec () (pre (> :etage 0) (assign :etage (- :etage 1)))))))

(def a-counter '(machine
                  (machine-variant)
                  (machine-header :ACounter ())
                  (variables :ii :jj)
                  (invariants (and (member :ii (interval 0 10))
                                     (member :jj (interval 0 10))
                                     (< :ii 11)
                                     (>= :jj 0)))
                  (init (assign :ii 2 :jj 10))
                  (operations
                    (operation () :inc () (select (> :jj 0) (parallel-substitution
                                                                 (assign :ii (+ :ii 1))
                                                                 (assign :jj (- :jj 1)))))
                    (operation (:result) :res () (assign :result :ii)))))

(def gcd '(machine
            (machine-variant)
            (machine-header :GCD ())
            (variables :x :y)
            (invariants (and (member :x (nat-set)) (member :y (nat-set))))
            (init (parallel-substitution (assign :x 70) (assign :y 40)))
            (operations
              (operation (:s) :GCDSolution () (if-sub (= :y 0) (assign :s :x) (assign :s -1)))
              (operation () :Step () (if-sub (> :y 0) (parallel-substitution (assign :x :y) (assign :y (mod :x :y)))))
              (operation () :Restart (:w1 :w2) (pre (and (member :w1 (nat1-set)) (member :w2 (nat1-set)))
                                                                (if-sub (> :w1 :w2)
                                                                         (assign :x :w1 :y :w2)
                                                                         (assign :y :w1 :x :w2)))))))

(def knights-knaves '(machine
                       (machine-variant)
                       (machine-header :KnightsKnaves ())
                       (constants :A :B :C)
                       (properties (and
                                      (member :A (bool-set))
                                      (member :B (bool-set))
                                      (member :C (bool-set))
                                      (<=> (= :A true) (or (= :B false) (= :C false)))
                                      (<=> (= :B true) (= :A true))))))

(def bakery0 '(machine
                (machine-variant)
                (machine-header :Bakery0 ())
                (variables :aa)
                (invariants (member :aa (interval 0 2)))
                (init (assign :aa 0))
                (operations
                  (operation () :enter1 () (select (= :aa 0) (assign :aa 1)))
                  (operation () :enter2 () (select (= :aa 0) (assign :aa 2)))
                  (operation () :leave1 () (select (= :aa 1) (assign :aa 0)))
                  (operation () :leave2 () (select (= :aa 2) (assign :aa 0)))
                  (operation () :try1 () (skip))
                  (operation () :try2 () (skip)))))

(def bakery1 '(machine
                (machine-variant)
                (machine-header :Bakery1 ())
                (variables :p1 :p2 :y1 :y2)
                (invariants (and (member :p1 (interval 0 2))
                                 (member :p2 (interval 0 2))
                                 (member :y1 (natural-set))
                                 (member :y2 (natural-set))
                                 (=> (= :p1 2) (< :p2 2))
                                 (=> (= :p2 2) (< :p1 2))))
                (init (assign :p1 0 :p2 0 :y1 0 :y2 0))
                (operations
                  (operation () :try1 () (select (= :p1 0) (parallel-substitution (assign :p1 1) (assign :y1 (+ :y2 1)))))
                  (operation () :enter1 () (select (and (= :p1 1) (or (= :y2 0) (< :y1 :y2))) (assign :p1 2)))
                  (operation () :leave1 () (select (= :p1 2) (parallel-substitution (assign :p1 0) (assign :y1 0))))
                  (operation () :try2 () (select (= :p2 0) (parallel-substitution (assign :p2 1) (assign :y2 (+ :y1 1)))))
                  (operation () :enter2 () (select (and (= :p2 1) (or (= :y1 0) (< :y2 :y1))) (assign :p2 2)))
                  (operation () :leave2 () (select (= :p2 2) (parallel-substitution (assign :p2 0) (assign :y2 0)))))))