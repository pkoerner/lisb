(ns lisb.adl.adl2lisb-test
  (:require [clojure.test :refer :all])
  (:require [lisb.adl.adl2lisb :refer :all]))

(deftest multiply-example-test
  (testing "multiply-example"
    (is (= '(machine
              :Multiply
              (variables :pc :x :y :p)
              (invariants (in :pc nat-set) (in :x nat-set) (in :y nat-set) (in :p nat-set))
              (init (assign :pc 0) (assign :x 5) (assign :y 3) (assign :p 0))
              (operations
                (:while0_enter [] (pre (and (= :pc 0) (> :x 0)) (assign :pc 2)))
                (:while0_exit [] (pre (and (= :pc 0) (not (> :x 0))) (assign :pc 1)))
                (:if0_then [] (pre (and (= :pc 2) (not= 0 (mod :x 2))) (assign :pc 4)))
                (:if0_else [] (pre (and (= :pc 2) (not (not= 0 (mod :x 2)))) (assign :pc 3)))
                (:assign0 [] (pre (= :pc 4) (sequential-sub (assign :p (+ :p :y)) (assign :pc 3))))
                (:assign1 [] (pre (= :pc 3) (sequential-sub (assign :x (/ :x 2) :y (* :y 2)) (assign :pc 0))))))
           (adl->lisb '(adl
                         :Multiply
                         (var :x (in :x nat-set) 5)
                         (var :y (in :y nat-set) 3)
                         (var :p (in :p nat-set) 0)
                         (algorithm
                           (while (> :x 0)
                             #_(assert (= (+ :p (* :x :y)) (* 5 3)))
                             (if (not= 0 (mod :x 2))
                               (assign :p (+ :p :y)))
                             (assign :x (/ :x 2) :y (* :y 2)))
                           #_(assert (= :p (* :x :y))))))))))

(deftest do-test
  (testing "equality-predicates"
    (is (= '(machine
              :ADL
              (variables :pc :x :y)
              (invariants (in :pc nat-set) (in :x nat-set) (in :y nat-set))
              (init (assign :pc 0) (assign :x 0) (assign :y 0))
              (operations
                (:if0_then [] (pre (and (= :pc 0) (> :x 0)) (assign :pc 2)))
                (:assign0 [] (pre (= :pc 2) (sequential-sub (assign :x 0) (assign :pc 3))))
                (:assign1 [] (pre (= :pc 3) (sequential-sub (assign :y 2) (assign :pc 1))))
                (:if0_else [] (pre (and (= :pc 0) (> :x 0)) (assign :pc 4)))
                (:assign2 [] (pre (= :pc 4) (sequential-sub (assign :x 1) (assign :pc 1))))))
           (adl->lisb '(adl
                         :ADL
                         (var :x (in :x nat-set) 0)
                         (var :y (in :y nat-set) 0)
                         (algorithm
                           (if (> :x 0)
                             (do
                               (assign :x 0)
                               (assign :y 2))
                             (assign :x 1)))))))))

(deftest if-test
  (testing "equality-predicates"
    (is (= '(machine
              :ADL
              (variables :pc :x)
              (invariants
                (in :pc nat-set)
                (in :x nat-set))
              (init
                (assign :pc 0)
                (assign :x 0))
              (operations
                (:if0_then [] (pre (and (= :pc 0) (> :x 0)) (assign :pc 2)))
                (:assign0 [] (pre (= :pc 2) (sequential-sub (assign :x 0) (assign :pc 1))))
                (:if0_else [] (pre (and (= :pc 0) (> :x 0)) (assign :pc 3)))
                (:assign1 [] (pre (= :pc 3) (sequential-sub (assign :x 1) (assign :pc 1))))))
           (adl->lisb '(adl
                         :ADL
                         (var :x (in :x nat-set) 0)
                         (algorithm
                           (if (> :x 0)
                             (assign :x 0)
                             (assign :x 1)))))))))

(deftest assign-test
    (testing "equality-predicates"
      (is (= '(machine
                :ADL
                (variables :pc :x :y)
                (invariants
                  (in :pc nat-set)
                  (in :x nat-set)
                  (in :y nat-set))
                (init
                  (assign :pc 0)
                  (assign :x 5)
                  (assign :y 3))
                (operations
                  (:assign0 [] (pre (= :pc 0) (sequential-sub (assign :x 1) (assign :pc 1))))
                  (:assign1 [] (pre (= :pc 1) (sequential-sub (assign :y 4) (assign :pc 2))))))
             (adl->lisb '(adl
                           :ADL
                           (var :x (in :x nat-set) 5)
                           (var :y (in :y nat-set) 3)
                           (algorithm
                             (assign :x 1)
                             (assign :y 4))))))))
(deftest var-test
  (testing "equality-predicates"
    (is (= '(machine
              :ADL
              (variables :pc :x :y)
              (invariants
                (in :pc nat-set)
                (in :x nat-set)
                (in :y nat-set))
              (init
                (assign :pc 0)
                (assign :x 5)
                (assign :y 3))
              (operations))
           (adl->lisb '(adl
                         :ADL
                         (var :x (in :x nat-set) 5)
                         (var :y (in :y nat-set) 3)))))))

(deftest default-machine-test
  (testing "equality-predicates"
    (is (= '(machine
              :ADL
              (variables :pc)
              (invariants (in :pc nat-set))
              (init
                (assign :pc 0))
              (operations))
           (adl->lisb '(adl :ADL))))))
