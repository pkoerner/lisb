(ns lisb.prob.animator-test
  (:require [lisb.prob.java-api :refer [state-space!]]
            [lisb.translation.util :refer [b->ast]])
  (:use [lisb.prob.animator]
        [clojure.test]))

(def lift-state-space 
  (delay (state-space! (b->ast (slurp (clojure.java.io/resource "machines/b//simple/Lift.mch"))))))

(deftest root-state-test
  (testing "can retrieve root state"
    (is (root-state @lift-state-space))))

(defn initialised-state [state-space]
  (get (root-state state-space) :op/$initialise_machine))

(deftest init-test
  (testing "can get initialised state"
    (is (successor (root-state @lift-state-space) :op/$initialise_machine))
    (is (get (root-state @lift-state-space) :op/$initialise_machine))))

(deftest state-access-test
  (testing "can extract information from state"
    (is (= 4 (:etage (initialised-state @lift-state-space))))))

(deftest state-navigation-test
  (testing "can navigate the state space animation-like"
    (is (= 5 (:etage (:op/inc (initialised-state @lift-state-space)))))
    (is (= 3 (:etage (:op/dec (initialised-state @lift-state-space)))))
    (is (= 7 (get-in (initialised-state @lift-state-space) [:op/inc :op/inc :op/inc :etage])))))

(deftest state-manipluation-test
  (testing "can update information in state"
    (is (= 42 (:etage (assoc (initialised-state @lift-state-space) :etage 42))))
    (is (= 5 (:etage (update (initialised-state @lift-state-space) :etage inc))))))

(deftest state-creation-test
  (testing "can create state"
    (let [state (to-state @lift-state-space {:etage 97})]
      (is (= 97 (:etage state)))
      (is (= 98 (:etage (:op/inc state)))))))
