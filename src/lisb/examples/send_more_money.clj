(ns lisb.examples.send-more-money
  (:require [lisb.core :refer [eval state-space]])
  (:require [lisb.translation.representation :refer :all])
  (:require [lisb.translation.translationOLD :refer [to-ast]]))

(defpred send-more-money-p [s e n d m o r y]
  (and (subset? #{s e n d m o r y} (range 0 9))
       (none= 0 s m)
       (none= s e n d m o r y)
       (= (+ (* 1000 s) (* 100 e) (* 10 n) d
             (* 1000 m) (* 100 o) (* 10 r) e)
          (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y))))

(defn send-more-money
  ([ss]
   (eval ss (to-ast (send-more-money-p :s :e :n :d :m :o :r :y))))
  ([]
   (defonce ss (state-space))
   (send-more-money ss)))

(defn send-more-money-clj []
  (for [s (range 1 10)
        e (remove #{s} (range 10))
        n (remove #{s e} (range 10))
        d (remove #{s e n} (range 10))
        m (remove #{s e n d} (range 1 10))
        o (remove #{s e n d m} (range 10))
        r (remove #{s e n d m o} (range 10))
        y (remove #{s e n d m o r} (range 10))
        :when (= (+ (* 1000 s) (* 100 e) (* 10 n) d
                    (* 1000 m) (* 100 o) (* 10 r) e)
                 (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y))]
    {"s" s, "e" e, "n" n, "d" d,
     "m" m, "o" o, "r" r, "y" y}))
