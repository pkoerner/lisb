(ns lisb.core
  (:require [lisb.prob.animator :refer :all])
  (:require [lisb.translation.lisb2ir :refer :all])
  (:require [lisb.translation.ir2ast :refer [ir->ast ir->predicate-ast ir->expression-ast]]))


(defn ir-state-space! [ir]
  (state-space! (ir->ast ir)))

; if you modify this default state space and complain afterwards, you stink
; execute only commmands on this state-space which not implement IStateSpaceModifier!
(defonce ^:private secret-state-space (ir-state-space! bempty-machine))

(defn eval-ir-as-predicate
  ([state-space ir] (eval-formula state-space (ir->predicate-ast ir)))
  ([ir] (eval-ir-as-predicate secret-state-space ir)))

(defn eval-ir-as-expression
  ([state-space ir] (eval-formula state-space (ir->expression-ast ir)))
  ([ir] (eval-ir-as-expression secret-state-space ir)))


(defn choose-rest [c]
  (let [n (count c)]
    (->> (concat c c)
         (partition n 1)
         (map (fn [[h & t]] [h t]))
         butlast)))


(defn sat-conjuncts?
  ([c]
   (eval-ir-as-predicate c))
  ([c & r]
   (eval-ir-as-predicate (apply band c r))))


(defn timeout-conjuncts?
  ([c]
   (= :timeout (eval-ir-as-predicate c)))
  ([c & r]
   (= :timeout (eval-ir-as-predicate (apply band c r)))))


(defn unsat-core-aux [sat? c]
  (let [poss (choose-rest c)
        [_ r] (first (drop-while
                       (comp sat? second)
                       poss))]
    (if r
      (unsat-core-aux sat? r)
      (set c))))


(defn unsat-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (not (eval-ir-as-predicate (apply band conjuncts)))]}
  (unsat-core-aux (partial apply sat-conjuncts?) conjuncts))


(defn unsat-core-predicate [p c]
  {:pre [(not (eval-ir-as-predicate (p c)))
         (set? c)]}
  (unsat-core-aux #(eval-ir-as-predicate (p (set %))) c))


(defn timeout-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (= :timeout (eval-ir-as-predicate (apply band conjuncts)))]}
  (unsat-core-aux (partial apply (complement timeout-conjuncts?)) conjuncts))
