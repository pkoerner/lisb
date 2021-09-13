(ns lisb.core
  (:require [lisb.prob.animator :refer :all])
  (:require [lisb.translation.representation :refer :all])
  (:require [lisb.translation.translation :refer [b->ast b->predicate-ast]]))


(defn lisb-state-space
  ([ast] (state-space (b->ast ast)))
  ([] (lisb-state-space bempty-machine)))


(defn eval-lisb-predicate
  ([state-space lisb-predicate] (eval-formula state-space (b->predicate-ast lisb-predicate)))
  ([lisb-predicate] (eval-lisb-predicate (lisb-state-space) lisb-predicate)))


(defn choose-rest [c]
  (let [n (count c)]
    (->> (concat c c)
         (partition n 1)
         (map (fn [[h & t]] [h t]))
         butlast)))


(defn sat-conjuncts?
  ([c]
   (eval-lisb-predicate c))
  ([c & r]
   (eval-lisb-predicate (apply band c r))))


(defn timeout-conjuncts?
  ([c]
   (= :timeout (eval-lisb-predicate c)))
  ([c & r]
   (= :timeout (eval-lisb-predicate (apply band c r)))))


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
         (not (eval-lisb-predicate (apply band conjuncts)))]}
  (unsat-core-aux (partial apply sat-conjuncts?) conjuncts))


(defn unsat-core-predicate [p c]
  {:pre [(not (eval-lisb-predicate (p c)))
         (set? c)]}
  (unsat-core-aux #(eval-lisb-predicate (p (set %))) c))


(defn timeout-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (= :timeout (eval-lisb-predicate (apply band conjuncts)))]}
  (unsat-core-aux (partial apply (complement timeout-conjuncts?)) conjuncts))
