(ns
 lisb.translation.types-test
 (:require
  [clojure.test :refer :all]
  [lisb.translation.types :refer :all]))

(deftest
 first-test-1721995393-2491
 (testing
  "this was deemed correct during development"
  (is (= (first (->Tuple [1 2])) 1))))

(deftest
 second-test-1721995396-2494
 (testing
  "this was deemed correct during development"
  (is (= (second (->Tuple [1 2])) 2))))

(deftest
 second-test-1721995397-2497
 (testing
  "this was deemed correct during development"
  (is (= (second (->Tuple [1 2])) 2))))

(deftest
 nth-test-1721995402-2500
 (testing
  "this was deemed correct during development"
  (is (= (nth (seq (->Tuple [1 2 3 4])) 2) 3))))

(deftest
 nth-test-1721995408-2503
 (testing
  "this was deemed correct during development"
  (is (= (nth (->Tuple [1 2 3 4]) 2) 3))))

(deftest
 map-test-1721995414-2506
 (testing
  "this was deemed correct during development"
  (is (= (map inc (->Tuple [1 2])) [2 3]))))
