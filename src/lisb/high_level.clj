(ns lisb.high-level
  (:use [lisb.prob.animator])
  (:use [lisb.prob.retranslate])
  (:use [lisb.translation.b2ast])
  (:use [lisb.translation.ast2lisb])
  (:use [lisb.translation.data-conversion])
  (:use [lisb.translation.ast2b])
  (:use [lisb.translation.ir2ast])
  (:use [lisb.translation.lisb2ir])
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

(defn b->ir
  [input-str]
  (eval `(b ~(ast->lisb (b->ast input-str)))))

(defn ir->b
  [ir]
  (ast->b (ir->ast ir)))

(defn load-mch!
  [filename]
  (let [input-string (slurp filename)
        ast (b->ast input-string)]
    {:ir (eval `(b ~(ast->lisb ast)))
     :ss (state-space ast)}))

(defn save-mch!
  [ir target-filename]
  (spit target-filename (ir->b ir)))
