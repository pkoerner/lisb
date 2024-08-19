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


(deftest
 make-flat-test-1724068141-12566
 (testing
  "this was deemed correct during development"
  (is (= (make-flat (->Tuple [1 2 3])) (->Tuple [1 2 3])))))

(deftest
 make-flat-test-1724068142-12567
 (testing
  "this was deemed correct during development"
  (is
   (=
    (make-flat (->Tuple [(->Tuple [0 1]) 2 3]))
    (->Tuple [0 1 2 3])))))

(deftest
 make-flat-test-1724068148-12574
 (testing
  "this was deemed correct during development"
  (is
   (=
    (make-flat (->Tuple [(->Tuple [(->Tuple [0 1]) 2]) 3 4]))
    (->Tuple [0 1 2 3 4])))))

(deftest
 make-flat-test-1724068152-12577
 (testing
  "this was deemed correct during development"
  (is
   (=
    (make-flat (->Tuple [(->Tuple [0 (->Tuple [1 2])]) 3 4]))
    (->Tuple [0 (->Tuple [1 2]) 3 4])))))


(deftest
 =-test-1724068287-13233
 (testing
  "this was deemed correct during development"
  (is (= (= (->Tuple [1 2]) (->Tuple [1 3])) false))))

(deftest
 =-test-1724068288-13236
 (testing
  "this was deemed correct during development"
  (is (= (= (->Tuple [1 2 3]) (->Tuple [(->Tuple [1 2]) 3])) true))))

(deftest
 =-test-1724068288-13239
 (testing
  "this was deemed correct during development"
  (is (= (= (->Tuple [1 2 3]) (->Tuple [1 (->Tuple [2 3])])) false))))

(deftest
 =-test-1724068288-13242
 (testing
  "this was deemed correct during development"
  (is
   (=
    (=
     (->Tuple [0 1 2 3 4])
     (->Tuple [(->Tuple [(->Tuple [0 1]) 2]) 3 4]))
    true))))

(deftest
 =-test-1724068288-13245
 (testing
  "this was deemed correct during development"
  (is
   (=
    (=
     (->Tuple [0 1 2 3 4])
     (->Tuple [(->Tuple [0 (->Tuple [1 2])]) 3 4]))
    false))))
