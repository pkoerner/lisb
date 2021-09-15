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

(defn lisb->ir [lisb]
  (eval `(b ~lisb)))

(defn b->ir
  [input-str]
  (-> input-str b->ast ast->lisb lisb->ir))

(defn ir->b
  [ir]
  (-> ir ir->ast ast->b))

(defn ast->ir
  [ast]
  (-> ast ast->lisb lisb->ir))

(defn load-mch!
  ([filename]
   (let [input-string (slurp filename)
         ast (b->ast input-string)]
     {:ir (ast->ir ast)
      :ss (state-space! ast)
      :meta {}}))
   ([filename meta-data]
    (let [input-string (slurp filename)
          ast (b->ast input-string)]
      {:ir (ast->ir ast)
       :ss (state-space! ast)
       :meta meta-data})))

(defn make-mch!
  ([ir]
  {:ir ir
   :ss (state-space! (ir->ast ir))
   :meta {}})
  ([ir meta-data]
   {:ir ir
    :ss (state-space! (ir->ast ir))
    :meta meta-data}))

(defn save-mch!
  [ir target-filename]
  (spit target-filename (ir->b ir)))
