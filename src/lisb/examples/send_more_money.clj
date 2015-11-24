(ns lisb.examples.send-more-money
  (:require [lisb.core :refer [eval state-space]])
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]]))

(defpred send-more-money-p [s e n d m o r y]
  (and (subset? #{s e n d m o r y} (range 0 9))
       (not= 0 s m)
       (not= s e n d m o r y)
       (= (+ (* 1000 s) (* 100 e) (* 10 n) d
             (* 1000 m) (* 100 o) (* 10 r) e)
          (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y))))

(defn send-more-money
  ([ss]
   (eval ss (to-ast (send-more-money-p :s :e :n :d :m :o :r :y))))
  ([]
   (defonce ss (state-space))
   (send-more-money ss)))
