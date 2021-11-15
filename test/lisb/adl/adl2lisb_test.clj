(ns lisb.adl.adl2lisb-test
  (:require [clojure.test :refer :all])
  (:require [lisb.adl.adl2lisb :refer :all])
  (:require [lisb.translation.lisb2ir :refer :all]))

#_(deftest multiply-example-test
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

#_(deftest var-test
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

#_(deftest default-machine-test
  (testing "equality-predicates"
    (is (= '(machine
              :ADL
              (variables :pc)
              (invariants (in :pc nat-set))
              (init
                (assign :pc 0))
              (operations))
           (adl->lisb '(adl :ADL))))))

(deftest multiply-example-test
  (testing "multiply-example"
    (is (=
          {:invariants [(bimplication (b= :pc 1) (b= (b+ :p (b* :x :y)) (b* 5 3)))
                        (bimplication (b= :pc 5) (b= :p (b* :x :y)))]
           :operations [
                        (bop :while_enter0 [] (bprecondition (band (b= :pc 0) (b> :x 0)) (bassign :pc 1)))
                        (bop :while_exit0 [] (bprecondition (band (b= :pc 0) (bnot (b> :x 0))) (bassign :pc 5)))
                        (bop :assert1 [] (bprecondition (b= :pc 1) (bassign :pc 2)))
                        (bop :ifte-then2 [] (bprecondition (band (b= :pc 2) (bnot= 0 (bmod :x 2))) (bassign :pc 3)))
                        (bop :ifte-else2 [] (bprecondition (band (b= :pc 2) (bnot (bnot= 0 (bmod :x 2)))) (bassign :pc 4)))
                        (bop :assign3 [] (bprecondition (b= :pc 3) (bsequential-sub (bassign :p (b+ :p :y)) (bassign :pc 4))))
                        (bop :assign4 [] (bprecondition (b= :pc 4) (bsequential-sub (bassign :x (bdiv :x 2) :y (b* :y 2)) (bassign :pc 0))))
                        (bop :assert5 [] (bprecondition (b= :pc 5) (bassign :pc 6)))]}
          (algorithm
            (while (> :x 0)
              (assert (= (+ :p (* :x :y)) (* 5 3)))
              (if (not= 0 (mod :x 2))
                (assign :p (+ :p :y)))
              (assign :x (/ :x 2) :y (* :y 2)))
            (assert (= :p (* :x :y))))))))

(deftest do-test
  (testing "do"
    (is (= {:invariants []
            :operations [(bop :assign0 [] (bprecondition (b= :pc 0) (bsequential-sub (bassign :x 0) (bassign :pc 1))))
                         (bop :ifte-then1 [] (bprecondition (band (b= :pc 1) (b> :x 1)) (bassign :pc 2)))
                         (bop :ifte-else1 [] (bprecondition (band (b= :pc 1) (bnot (b> :x 1))) (bassign :pc 4)))
                         (bop :assign2 [] (bprecondition (b= :pc 2) (bsequential-sub (bassign :x 2) (bassign :pc 3))))
                         (bop :assign3 [] (bprecondition (b= :pc 3) (bsequential-sub (bassign :x 3) (bassign :pc 4))))]}
           (algorithm
             (do
               (assign :x 0))
             (if (> :x 1)
               (do
                 (assign :x 2)
                 (assign :x 3))))))))

(deftest if-test
  (testing "if"
    (is (= {:invariants []
            :operations [
                         (bop :ifte-then0 [] (bprecondition (band (b= :pc 0) (b> :x 0)) (bassign :pc 1)))
                         (bop :ifte-else0 [] (bprecondition (band (b= :pc 0) (bnot (b> :x 0))) (bassign :pc 2)))
                         (bop :assign1 [] (bprecondition (b= :pc 1) (bsequential-sub (bassign :x 1) (bassign :pc 2))))
                         (bop :ifte-then2 [] (bprecondition (band (b= :pc 2) (b> :x 2)) (bassign :pc 3)))
                         (bop :ifte-else2 [] (bprecondition (band (b= :pc 2) (bnot (b> :x 2))) (bassign :pc 4)))
                         (bop :assign3 [] (bprecondition (b= :pc 3) (bsequential-sub (bassign :x 3) (bassign :pc 5))))
                         (bop :assign4 [] (bprecondition (b= :pc 4) (bsequential-sub (bassign :y 4) (bassign :pc 5))))]}
           (algorithm
             (if (> :x 0)
               (assign :x 1))
             (if (> :x 2)
               (assign :x 3)
               (assign :y 4)))))))

(deftest while-test
  (testing "while"
    (is (= {:invariants []
            :operations [
                         (bop :while_enter0 [] (bprecondition (band (b= :pc 0) (b> :x 0)) (bassign :pc 1)))
                         (bop :while_exit0 [] (bprecondition (band (b= :pc 0) (bnot (b> :x 0))) (bassign :pc 2)))
                         (bop :assign1 [] (bprecondition (b= :pc 1) (bsequential-sub (bassign :x 1) (bassign :pc 0))))
                         (bop :while_enter2 [] (bprecondition (band (b= :pc 2) (b> :x 2)) (bassign :pc 3)))
                         (bop :while_exit2 [] (bprecondition (band (b= :pc 2) (bnot (b> :x 2))) (bassign :pc 5)))
                         (bop :assign3 [] (bprecondition (b= :pc 3) (bsequential-sub (bassign :x 3) (bassign :pc 4))))
                         (bop :assign4 [] (bprecondition (b= :pc 4) (bsequential-sub (bassign :y 4) (bassign :pc 2))))]}
           (algorithm
             (while (> :x 0)
               (assign :x 1))
             (while (> :x 2)
               (assign :x 3)
               (assign :y 4)))))))

(deftest assign-test
    (testing "assign"
      (is (=
            {:invariants []
             :operations [(bop :assign0 [] (bprecondition (b= :pc 0) (bsequential-sub (bassign :x 1) (bassign :pc 1))))
                          (bop :assign1 [] (bprecondition (b= :pc 1) (bsequential-sub (bassign :x 2 :y 3) (bassign :pc 2))))
                          (bop :assign2 [] (bprecondition (b= :pc 2) (bsequential-sub (bassign :x {:tag :successor, :num :x}) (bassign :pc 3))))]}
            (algorithm
               (assign :x 1)
               (assign :x 2 :y 3)
               (assign :x (inc :x)))))))

(deftest assert-test
  (testing "assert"
    (is (=
          {:invariants [(bimplication (b= :pc 0) (b= :p (b* :x :y)))]
           :operations [(bop :assert0 [] (bprecondition (b= :pc 0) (bassign :pc 1)))]}
          (algorithm
            (assert (= :p (* :x :y))))))))
