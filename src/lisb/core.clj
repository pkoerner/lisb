(ns lisb.core
  (:require [lisb.prob.animator :refer [eval-formula state-space!]]
            [lisb.translation.util :refer [ir->ast bempty-machine band bmember? bnot=]]))


(defn ir-state-space! [ir-machine]
  (state-space! (ir->ast ir-machine)))

; if you modify this default state space and complain afterwards, you stink
; execute only commmands on this state-space which not implement IStateSpaceModifier!
(defonce ^:private secret-state-space (delay (ir-state-space! bempty-machine)))

(defn eval-ir-formula
  ([state-space ir-formula] (eval-formula state-space (ir->ast ir-formula)))
  ([ir-formula] (eval-ir-formula @secret-state-space ir-formula)))


(defn choose-rest [c]
  (let [n (count c)]
    (->> (concat c c)
         (partition n 1)
         (map (fn [[h & t]] [h t]))
         butlast)))


(defn sat-conjuncts?
  ([c]
   (eval-ir-formula c))
  ([c & r]
   (eval-ir-formula (apply band c r))))


(defn timeout-conjuncts?
  ([c]
   (= :timeout (eval-ir-formula c)))
  ([c & r]
   (= :timeout (eval-ir-formula (apply band c r)))))


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
         (not (eval-ir-formula (apply band conjuncts)))]}
  (unsat-core-aux (partial apply sat-conjuncts?) conjuncts))


(defn unsat-core-predicate [p c]
  {:pre [(not (eval-ir-formula (p c)))
         (set? c)]}
  (unsat-core-aux #(eval-ir-formula (p (set %))) c))


(defn timeout-core [& conjuncts]
  {:pre [(seq (rest conjuncts))
         (= :timeout (eval-ir-formula (apply band conjuncts)))]}
  (unsat-core-aux (partial apply (complement timeout-conjuncts?)) conjuncts))



;; TODO: handle timeouts
(defn try-get-solutions 
  ;; TODO: n solutions can be obtained by using the external function CHOOSE_n
  ([bset] (try-get-solutions bset #{}))
  ([bset seen]
   (lazy-seq 
     (let [freshkw (keyword (gensym "lisb__internal"))
           formula (apply band (bmember? freshkw bset)
                          (map #(bnot= freshkw %) seen))
           res (eval-ir-formula formula)
;; TODO: the solution map maps *strings* to solutions
;;       even though the variables are keywords.
;;       I do not particularly like this. (pk, 05.06.2024)
           element (get res (name freshkw))] 
       (if res
         (cons element (try-get-solutions bset (conj seen element)))
         ())))))
