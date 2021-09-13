(ns lisb.high-level
  (:use [lisb.representation])
  (:use [lisb.translation])
  (:use [lisb.core])
  (:import de.prob.statespace.Trace)
  )

(defn load-machine-trace [m]
  (let [ss (.b_load api (b->ast m))]
    (Trace. ss)))

(defn load-initialized-machine-trace [m]
  (let [ss (.b_load api (b->ast m))]
    (.addTransitionWith (Trace. ss) "$initialise_machine" [])))

(defn latest-state [trace]
  (last (.getTransitionList trace)))

(defn root [trace]
  (-> trace (.getStateSpace) (.getRoot)))

(defn perform [trace operation & args]
  (.addTransitionWith trace (name operation) (vec args)))

(defn possible-ops [trace]
  (.getNextTransitions trace))
