(ns lisb.blojure-integration-test
  (:require [clojure.test :refer :all])
  (:require [lisb.blojure :refer [eval state-space]])
  (:require [lisb.frontends.blojure :refer :all])
  (:require [lisb.translations.blojure-translation :refer [to-ast]]))

(defonce te-ss-t (state-space))

(deftest integration
  (testing "no exception is thrown when executing implemented command;
            this test only checks that a valid (i.e. accepted) AST is constructed
            without concern about semantics
            for now only 'truish' predicates are evaluated because eval cannot handle
            ComputationNotCompletedResults yet"
    (let [ss te-ss-t]
      (is (eval ss (to-ast (blj (= 1 1)))))
      (is (eval ss (to-ast (blj (= :x 1)))))
      (is (eval ss (to-ast (blj (= 1 1 1)))))
      (is (eval ss (to-ast (blj (= :x true)))))
      (is (eval ss (to-ast (blj (= :x false)))))

      ; (is (eval ss (to-ast (blj (none= 0 1)))))
      ; (is (eval ss (to-ast (blj (none= 0 1 2)))))
      ; (is (eval ss (to-ast (blj (not (none= 0 1 1))))))

      (is (eval ss (to-ast (blj (not= 0 1)))))
      (is (eval ss (to-ast (blj (not= 0 1 2)))))
      (is (eval ss (to-ast (blj (not= 0 1 1)))))

      (is (eval ss (to-ast (blj (< 1 2)))))
      (is (eval ss (to-ast (blj (< 1 2 3)))))

      (is (eval ss (to-ast (blj (> 2 1)))))
      (is (eval ss (to-ast (blj (> 3 2 1)))))

      (is (eval ss (to-ast (blj (<= 1 2)))))
      (is (eval ss (to-ast (blj (<= 1 1 3)))))

      (is (eval ss (to-ast (blj (>= 2 1)))))
      (is (eval ss (to-ast (blj (>= 3 3 1)))))

      (is (eval ss (to-ast (blj (< 1 (+ 1 2))))))
      (is (eval ss (to-ast (blj (< 1 (+ 1 2 3))))))

      (is (eval ss (to-ast (blj (< 1 (- 10 2))))))
      (is (eval ss (to-ast (blj (< 1 (- 10 2 3))))))

      (is (eval ss (to-ast (blj (= 6 (* 2 3))))))
      (is (eval ss (to-ast (blj (= 24 (* 2 3 4))))))

      (is (eval ss (to-ast (blj (= 6 (quot 24 4))))))
      (is (eval ss (to-ast (blj (= 2 (quot 24 4 3))))))

      (is (eval ss (to-ast (blj (and (< 1 2) (< 2 3))))))
      (is (eval ss (to-ast (blj (and (< 1 2) (< 2 3) (< 3 4))))))

      (is (eval ss (to-ast (blj (or (< 1 2) (< 2 3))))))
      (is (eval ss (to-ast (blj (or (< 1 2) (< 2 3) (< 3 4))))))

      (is (eval ss (to-ast (blj (not (< 2 1))))))

      (is (eval ss (to-ast (blj (= :x #{1 2})))))
      (is (eval ss (to-ast (blj (= :x #{1 2 (+ 1 2)})))))

      (is (eval ss (to-ast (blj (= 2 (count #{1 2}))))))

      (is (eval ss (to-ast (blj (= #{1 2 3} (union #{1 2} #{2 3}))))))
      (is (eval ss (to-ast (blj (= #{1 2 3 4} (union #{1 2} #{2 3} #{3 4}))))))

      (is (eval ss (to-ast (blj (= #{2} (intersection #{1 2} #{2 3}))))))
      (is (eval ss (to-ast (blj (= #{3 4} (intersection #{1 2 3 4} #{2 3 4} #{3 4}))))))

      (is (eval ss (to-ast (blj (= #{1} (difference #{1 2} #{2 3}))))))
      (is (eval ss (to-ast (blj (= #{1} (difference #{1 2} #{2 3} #{3 4}))))))

      (is (eval ss (to-ast (blj (contains? #{1 2 3} 1)))))

      (is (eval ss (to-ast (blj (= 3 (max #{1 2 3}))))))
      (is (eval ss (to-ast (blj (= 3 (max 1 2 3))))))
      (is (eval ss (to-ast (blj (= 3 (max 2 3))))))

      (is (eval ss (to-ast (blj (= 1 (min #{1 2 3}))))))
      (is (eval ss (to-ast (blj (= 1 (min 1 2 3))))))
      (is (eval ss (to-ast (blj (= 2 (min 2 3))))))

      (is (eval ss (to-ast (blj (= 2 (mod 5 3))))))

      (is (eval ss (to-ast (blj (= 2 (inc 1))))))

      (is (eval ss (to-ast (blj (= 0 (dec 1))))))

      (is (eval ss (to-ast (blj (= :x #{[1 2]})))))

      (is (eval ss (to-ast (blj (= #{1 2} (keys {1 1 2 42}))))))

      (is (eval ss (to-ast (blj (= #{1 42} (vals {1 1 2 42}))))))

      (is (eval ss (to-ast (blj (= {1 1} (comp {0 1} {1 0}))))))
      (is (eval ss (to-ast (blj (= {2 1} (comp {2 1} {5 2} {2 5}))))))

      #_(is (eval ss (to-ast (blj (= {3 9 4 16} (fn [x] (< 2 x 5) (* x x)))))))

      (is (eval ss (to-ast (blj (= (range 1 5) [1 2 3 4])))))

      (is (eval ss (to-ast (blj (= [3 4 1 5] (concat [3 4] [1 5]))))))
      (is (eval ss (to-ast (blj (= (vector 3 4 1 5 2) (concat (vector 3 4) (vector 1 5) (vector 2)))))))

      (is (eval ss (to-ast (blj (= (reverse (vector 3 1 4)) (vector 4 1 3))))))

      (is (eval ss (to-ast (blj (= (first (vector 3 1 4)) 3)))))
      (is (eval ss (to-ast (blj (= (last (vector 3 1 4)) 4)))))

      (is (eval ss (to-ast (blj (= (butlast (vector 3 1 4)) (vector 3 1))))))
      (is (eval ss (to-ast (blj (= (rest (vector 3 1 4)) (vector 1 4))))))

      (is (eval ss (to-ast (blj (= (take 3 (vector 3 1 4 1 5)) (vector 3 1 4))))))
      (is (eval ss (to-ast (blj (= (drop 3 (vector 3 1 4 1 5)) (list 1 5))))))

      (is (eval ss (to-ast (blj (= #{1 2 3} :x)))))

      (is (eval ss (to-ast (blj (= :x "foo")))))

      )))



#_(deftest fancy-fns-test
  (testing "fancier functions"
    (let [ss te-ss-t]
      ; (is (eval ss (to-ast (blj (= [2 3 4] (map (pred [x] (+ 1 x))) #{1 2 3})))))
      ; (is (eval ss (to-ast (blj (= [2 3 4] (filter (fn [:x] (<= 2 :x 4)) (list 1 2 3 4 5)))))))
      (is (eval ss (to-ast (b= (bif (b< 1 2) 3 4) 3))))
      (is (eval ss (to-ast (b= (bif (b> 1 2) 3 4) 4))))
      (is (eval ss (to-ast (b= (brange 1 5) #{1 2 3 4}))))
      (is (eval ss (to-ast (bpred "1<2"))))
      (is (eval ss (to-ast (b= 2 (bexpr "1+1")))))
      (is (eval ss (to-ast (b= :x (bcall "CHOOSE" #{1 2 3})))))
      (is (eval ss (to-ast (blet-pred [:foo 1] (b< :foo 2)))))
      (is (eval ss (to-ast (blet-pred [:foo 1 :bar 2] (b< :foo :bar)))))
      (is (eval ss (to-ast (b= 1 (blet-expr [:foo 1] :foo)))))
      (is (eval ss (to-ast (b= 3 (blet-expr [:foo 1 :bar 2] (b+ :foo :bar))))))
      (is (eval ss (to-ast (b= -1 (bminus-or-set-subtract 1 2)))))
      (is (eval ss (to-ast (b= #{1} (bminus-or-set-subtract #{1 2} #{2})))))
          )))


#_(deftest b-haviour
  (let [ss (state-space)]
    (testing "satisfiable predicates should have truthy return values"
      (is (eval ss (to-ast (blj (= 1 1)))))
      (is (eval ss (to-ast (blj (= :x 1))))))
    (testing "the return values should be maps"
      (is (= {} (eval ss (to-ast (blj (= 1 1))))))
      (is (= {"x" 1} (eval ss (to-ast (blj (= :x 1)))))))
    ;; TODO: test concerning the resulting data structures, i.e. sets, sequences, tuples...
    (testing "unsatisfiable predicates yield a nil value"
      (is (nil? (eval ss (to-ast (blj (= 1 2)))))))
    (testing "timeouts / no found solution should throw an exception"
      (is (thrown? Exception (eval ss (to-ast (blj (< :x :y :x)))))))))
