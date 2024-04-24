(ns lisb.core-test
  (:require [clojure.test :refer :all]
            [lisb.core :refer [unsat-core unsat-core-predicate choose-rest sat-conjuncts?]]
            [lisb.translation.lisb2ir :refer :all]))


(deftest choose-rest-test
  (testing "choose-rest extracts one element of a collection
           and returns all others in a collection"
    (is (= (set (choose-rest [1 2 3 4]))
           #{[1 [2 3 4]]
             [2 [3 4 1]]
             [3 [4 1 2]]
             [4 [1 2 3]]}))))

(deftest unsat-core-test
  (testing "if the input is satisfiable, unsat-core does not apply"
    (is (thrown? AssertionError
                 (unsat-core (b= :a 1)
                             (b= :b 1)))))
  (testing "unsat-core finds a trivial unsat core"
    (is (= #{(b= :a 1) (b= :a 2)}
           (unsat-core (b= :a 1)
                       (b= :a 2)))))
  (testing "unsat-core finds a non-trivial unsat core"
      (is (=  #{(b= :a 1) (b= :a 3)}
              (unsat-core (b= :a 1)
                         (b= :b 2)
                         (b= :a 3))))
      (is (let [uc (unsat-core (b= :a 2)
                               (b= :b 3)
                               (b= :c (b+ :a :b))
                               (bsubset? #{:a :b :c} #{1 2 3}))]
            (and
              (apply (complement sat-conjuncts?) uc)
              (every? #(apply sat-conjuncts? %) (map second (choose-rest uc))))))))

(deftest unsat-core-predicate-test
  (testing "unsat core works with predicates that minimize a parameter set"
    (is (= (unsat-core-predicate (pred pp [c] (not (subset? #{4 5 7} c)))
                                 (set (range 10)))
           #{4 5 7}))))
