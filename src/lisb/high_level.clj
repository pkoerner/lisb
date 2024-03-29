(ns lisb.high-level
  (:use [lisb.prob.animator])
  (:use [lisb.prob.retranslate])
  (:use [lisb.translation.util])
  (:use [lisb.core])
  (:import de.prob.statespace.Trace))

(defn load-machine-trace [m]
  (let [ss (state-space! (b->ast m))]
    (Trace. ss)))

(defn load-initialized-machine-trace [m]
  (let [ss (state-space! (b->ast m))]
    (.addTransitionWith (Trace. ss) "$initialise_machine" [])))

(defn latest-state [trace]
  (last (.getTransitionList trace)))

(defn root [trace]
  (-> trace (.getStateSpace) (.getRoot)))

(defn perform [trace operation & args]
  (.addTransitionWith trace (name operation) (vec args)))

(defn possible-ops [trace]
  (.getNextTransitions trace))
