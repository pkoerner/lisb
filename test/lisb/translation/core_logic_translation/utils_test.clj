(ns lisb.translation.core-logic-translation.utils-test
  (:require [clojure.test :refer :all]
            [lisb.translation.core-logic-translation.lisb2ir :refer :all]))

(deftest test-primitive?
  (testing "predicate works correctly"
    (is (= true (primitive? 2)))
    (is (= true (primitive? "some string")))
    (is (= true (primitive? :somekeyword)))
    (is (= true (primitive? false)))
    (is (= true (primitive? #{1 2 3})))
    (is (= nil  (primitive? '())))
    (is (= nil  (primitive? 'symbol)))))

(deftest test-zip-with-rest
  (testing "zip-with-rest works as described"
    (is (= {} (zip-with-rest [] [])))
    (is (= {:a 1 :b 2 :c 3} (zip-with-rest [:a :b :c][1 2 3])))
    (is (= {:a 1 :b 2 :c 3 :d '(4 5 6)} (zip-with-rest [:a :b :c :d][1 2 3 4 5 6])))
    (is (= {:a 1 :b 2} (zip-with-rest [:a :b :c][1 2])))))

(deftest test-flatten-last
  (testing "flatten-last only flattens if last element is sequence but not lisb"
    (is (= '() (flatten-last '())))
    (is (= '(+ 1 2) (flatten-last '(+ (1 2)))))
    (is (= '(someoperator (1 2) 3 4 5) (flatten-last '(someoperator (1 2) (3 4 5)))))
    (is (= '(someoperator (1 2) (+ 1 2)) (flatten-last '(someoperator (1 2) (+ 1 2)))))))

(deftest test-parse-machine-reference-lisb->ir
  (testing "parse-machine-reference-lisb->ir transforms lisb correctly"
    (is (= '(refinement :somename (machine-reference :someref) (+ 1 2)) (parse-machine-reference-lisb->ir '(refinement :somename :someref (+ 1 2)))))
    (is (= '(refinement :somename (machine-reference :someref [] (< 1 2)) (+ 1 2)) (parse-machine-reference-lisb->ir '(refinement :somename (:someref [] (< 1 2)) (+ 1 2))))) ))


(deftest test-parse-machine-reference-ir->lisb
  (testing "parse-machine-reference-ir->lisb translates ir to correct lisb"
    (is (= '(refinement :somename :someref (+ 1 2))
           (parse-machine-reference-ir->lisb {:tag :refinement, 
                                              :name :somename,
                                              :abstract-machine-name {:tag :machine-reference, :name :someref},
                                              :machine-clauses '({:tag :add, :nums (1 2)})})))
    (is (= '(refinement :somename (:someref [] (> 1 2)) (+ 1 2))
           (parse-machine-reference-ir->lisb {:tag :refinement,
                                              :name :somename,
                                              :abstract-machine-name {:tag :machine-reference, 
                                                                      :name :someref,
                                                                      :args [],
                                                                      :body {:tag :greater, :nums '(1 2)}} 
                                              :machine-clauses '({:tag :add, :nums (1 2)})})))))


(deftest test-machine-reference-ir->lisb
  (testing "translate machine references correctly"
    (is (= :someref (machine-reference {:tag :machine-reference :name :someref})))
    (is (= '(:someref [] (+ 1 2)) (machine-reference {:tag :machine-reference,
                                         :name :someref,
                                         :args []
                                         :body {:tag :add, :nums '(1 2)}})))))

(deftest test-<--call
  (testing "transform <-- with op-call returns correctly"
    (is (= '(op-call [:some-return] 'someop [:arg])
            (<--call '(<-- [:some-return] (op-call 'someop [:arg])))))))

(deftest test-<--name
  (testing "transform <-- form with name correctly"
    (is (= '(op [:some-return] :somename [:arg] (+ 1 2))
            (<--name '(<-- [:some-return] (:somename [:arg] (+ 1 2))))))))


(deftest test<--nameIR
  (testing "transform op IR to <--correctly"
    (is (= '(<-- [:some-return] (:somename [:arg] (+ 1 2)))
            (<--name-IR {:tag :op, :name :somename, :returns [:some-return],
                         :args [:arg], :body {:tag :add, :nums '(1 2)}})))))

(deftest test<--callIR
  (testing "transform op IR to <--correctly"
    (is (= '(<-- [:some-return] (op-call :someop [:arg]))
            (<--op-IR {:tag :op-call, :op :someop, :returns [:some-return],
                         :args [:arg]})))))


