(ns lisb.adl.adl2lisb-test
  (:refer-clojure :exclude [assert while])
  (:require [clojure.test :refer :all])
  #_{:clj-kondo/ignore #{:refer-all}}
  (:require [lisb.adl.adl2lisb :refer :all])
  (:require [lisb.translation.lisb2ir :refer :all]))

(deftest multiply-example-test
  (testing "multiply-example"
    (is (= (bmachine
              :Multiply
              (bvariables :pc :x :y :p)
              (binvariants
                (bmember? :pc bnat-set)
                (bmember? :x bnat-set)
                (bmember? :y bnat-set)
                (bmember? :p bnat-set)
                (bimplication (b= :pc 1) (b= (b+ :p (b* :x :y)) (b* 5 3)))
                (bimplication (b= :pc 5) (b= :p (b* :x :y))))
              (binit (bassign :pc 0) (bassign :x 5) (bassign :y 3) (bassign :p 0))
              (boperations
                (bop :while-enter0 [] (bprecondition (band (b= :pc 0) (b> :x 0)) (bassign :pc 1)))
                (bop :while-exit0 [] (bprecondition (band (b= :pc 0) (bnot (b> :x 0))) (bassign :pc 5)))
                (bop :assert1 [] (bprecondition (b= :pc 1) (bassign :pc 2)))
                (bop :if-then2 [] (bprecondition (band (b= :pc 2) (bnot= 0 (bmod :x 2))) (bassign :pc 3)))
                (bop :if-else2 [] (bprecondition (band (b= :pc 2) (bnot (bnot= 0 (bmod :x 2)))) (bassign :pc 4)))
                (bop :assign3 [] (bprecondition (b= :pc 3) (bsequential-sub (bassign :p (b+ :p :y)) (bassign :pc 4))))
                (bop :assign4 [] (bprecondition (b= :pc 4) (bsequential-sub (bassign :x (bdiv :x 2) :y (b* :y 2)) (bassign :pc 0))))
                (bop :assert5 [] (bprecondition (b= :pc 5) (bassign :pc 6)))))
           (adl
             :Multiply
             (var :x (in :x nat-set) 5)
             (var :y (in :y nat-set) 3)
             (var :p (in :p nat-set) 0)
             (algorithm
               (while (> :x 0)
                 (assert (= (+ :p (* :x :y)) (* 5 3)))
                 #_{:clj-kondo/ignore [:missing-else-branch]}
                 (if (not= 0 (mod :x 2))
                   (assign :p (+ :p :y)))
                 (assign :x (/ :x 2) :y (* :y 2)))
               (assert (= :p (* :x :y)))))))))

(deftest var-test
  (testing "equality-predicates"
    (is (= (bmachine
             :ADL
             (bvariables :pc :x :y)
             (binvariants
               (bmember? :pc bnat-set)
               (bmember? :x bnat-set)
               (bmember? :y bnat-set))
             (binit
               (bassign :pc 0)
               (bassign :x 5)
               (bassign :y 3))
             (boperations (bop :assign0 [] (bprecondition (b= :pc 0) (bsequential-sub (bassign :x 1) (bassign :pc 1))))))
           (adl
             :ADL
             (var :x (member? :x nat-set) 5)
             (var :y (member? :y nat-set) 3)
             (algorithm
               (assign :x 1)))))))

(deftest empty-machine-test
  (testing "empty-machine"
    (is (= (bmachine
              :ADL
              (bvariables :pc)
              (binvariants (bmember? :pc bnat-set))
              (binit
                (bassign :pc 0))
              (boperations))
           (adl :ADL)))))

(deftest do-test
  (testing "do"
    (is (= {:invariants []
            :operations [(bop :assign0 [] (bprecondition (b= :pc 0) (bsequential-sub (bassign :x 0) (bassign :pc 1))))
                         (bop :if-then1 [] (bprecondition (band (b= :pc 1) (b> :x 1)) (bassign :pc 2)))
                         (bop :if-else1 [] (bprecondition (band (b= :pc 1) (bnot (b> :x 1))) (bassign :pc 4)))
                         (bop :assign2 [] (bprecondition (b= :pc 2) (bsequential-sub (bassign :x 2) (bassign :pc 3))))
                         (bop :assign3 [] (bprecondition (b= :pc 3) (bsequential-sub (bassign :x 3) (bassign :pc 4))))]}
           (algorithm
             (do
               (assign :x 0))
             #_{:clj-kondo/ignore [:missing-else-branch]}
             (if (> :x 1)
               (do
                 (assign :x 2)
                 (assign :x 3))))))))

(deftest if-test
  (testing "if"
    (is (= {:invariants []
            :operations [
                         (bop :if-then0 [] (bprecondition (band (b= :pc 0) (b> :x 0)) (bassign :pc 1)))
                         (bop :if-else0 [] (bprecondition (band (b= :pc 0) (bnot (b> :x 0))) (bassign :pc 2)))
                         (bop :assign1 [] (bprecondition (b= :pc 1) (bsequential-sub (bassign :x 1) (bassign :pc 2))))
                         (bop :if-then2 [] (bprecondition (band (b= :pc 2) (b> :x 2)) (bassign :pc 3)))
                         (bop :if-else2 [] (bprecondition (band (b= :pc 2) (bnot (b> :x 2))) (bassign :pc 4)))
                         (bop :assign3 [] (bprecondition (b= :pc 3) (bsequential-sub (bassign :x 3) (bassign :pc 5))))
                         (bop :assign4 [] (bprecondition (b= :pc 4) (bsequential-sub (bassign :y 4) (bassign :pc 5))))]}
           (algorithm
             #_{:clj-kondo/ignore [:missing-else-branch]}
             (if (> :x 0)
               (assign :x 1))
             (if (> :x 2)
               (assign :x 3)
               (assign :y 4)))))))

(deftest while-test
  (testing "while"
    (is (= {:invariants []
            :operations [
                         (bop :while-enter0 [] (bprecondition (band (b= :pc 0) (b> :x 0)) (bassign :pc 1)))
                         (bop :while-exit0 [] (bprecondition (band (b= :pc 0) (bnot (b> :x 0))) (bassign :pc 2)))
                         (bop :assign1 [] (bprecondition (b= :pc 1) (bsequential-sub (bassign :x 1) (bassign :pc 0))))
                         (bop :while-enter2 [] (bprecondition (band (b= :pc 2) (b> :x 2)) (bassign :pc 3)))
                         (bop :while-exit2 [] (bprecondition (band (b= :pc 2) (bnot (b> :x 2))) (bassign :pc 5)))
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
