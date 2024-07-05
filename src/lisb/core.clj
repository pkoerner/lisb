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




(comment (set! *print-length* 100)
(use 'lisb.prob.animator)
(use 'lisb.translation.util)

(defpred square [x]
  (* x x))

(eval-formula'
  @secret-state-space
  ;(lisb->ast '(= :x (+ 1 2 3 4)))
  (lisb->ast '(lambda [:y] (member? :y (range 0 10)) (* :y :y)))
  {:val-output :value
   :val-aggression 10
   }
  )

(eval-formula'
  @secret-state-space
  (lisb->ast '(= :x (lambda [:y] (member? :y (range 0 10)) (* :y :y))))
  {:val-output :value
   :val-aggression 10
   }
  )

(eval-formula'
  @secret-state-space
  (lisb->ast 'natural-set)
  {:val-output :value
   :val-aggression :lazy
   }
  )

(eval-formula'
  @secret-state-space
  (lisb->ast '(= :x natural-set))
  {:val-output :value
   :val-aggression :lazy
   }
  )

(eval-formula'
  @secret-state-space
  (lisb->ast '(= :x (+ 1 2)))
  {:val-output :value}
  )

(eval-formula'
  @secret-state-space
  (lisb->ast '(= :x [1 -> 2]))
  {:val-output :value}
  )

;; Problem: tuples are not represented in Clojure :-(
(eval-formula'
  @secret-state-space
  (lisb->ast '(= :x (seq #{1 2})))
  {:val-output :value}
  )

(eval-formula'
  @secret-state-space
  (lisb->ast '(and (= :x (+ 1 2)) (= :x 4) ))
  {:val-output :value}
  )

(take 3 (try-get-solutions {:tag :nat-set} @secret-state-space))
(eval-formula @secret-state-space (ir->ast {:tag :member, :elem :lisb__internal10991, :set {:tag :nat-set}}))
(use 'clojure.repl)
(pst))

(eval-ir-formula {:tag :nat-set})
(eval-ir-formula 
  '{:tag :and, :preds ({:tag :member, :elem :lisb__internal10797, :set {:tag :power-set, :set #{1 2}}} {:tag :not-equals, :left :lisb__internal10797, :right #{}} {:tag :not-equals, :left :lisb__internal10797, :right #{2}} {:tag :not-equals, :left :lisb__internal10797, :right #{1}} {:tag :not-equals, :left :lisb__internal10797, :right #{1 2}})})
