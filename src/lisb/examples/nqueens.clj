(ns lisb.examples.nqueens
  (:require [lisb.core :refer [eval state-space]])
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]]))



(defn nqueens
  "the n-queens problem in B using lisp-like syntax"
  ([size ss]
   (let [width (binterval 1 :n)
         repr (band (b= :n size)
                    (bmember :queens (b>-> width width))
                    (bforall [:q1 :q2]
                             (b=> (band (bmember :q1 width)
                                        (bmember :q2 width)
                                        (b> :q2 :q1))
                                  (band (bnot= (b+ (bapply :queens :q1) (b- :q2 :q1)) (bapply :queens :q2))
                                        (bnot= (b+ (bapply :queens :q1) (b- :q1 :q2)) (bapply :queens :q2))))))
         result (eval ss (to-ast repr))]
     result))
  ([size]
   (defonce ss (state-space))
   (nqueens size ss)))


(defn nqueens2
  "the n-queens problem in B using the b macro"
  ([size ss]
   (let [width (b (range 1 :n))
         q1pos (b (apply :queens :q1))
         q2pos (b (apply :queens :q2))
         repr  (b (and (= :n size)
                       (member? :queens (>-> width width))
                       (forall [:q1 :q2]
                               (=> (and (member? :q1 width)
                                        (member? :q2 width)
                                        (> :q2 :q1))
                                   (and (not= (+ q1pos (- :q2 :q1)) q2pos)
                                        (not= (+ q1pos (- :q1 :q2)) q2pos))))))
         result (eval ss (to-ast repr))]
     result))
  ([size]
   (defonce ss (state-space))
   (nqueens2 size ss)))



(defpred nqueens-p [size sol]
  (let [width (range 1 :n)
        q1pos (apply sol :q1)
        q2pos (apply sol :q2)]

        (and (= :n size)
             (member? sol (>-> width width))
             (forall [:q1 :q2]
                     (=> (and (member? :q1 width)
                              (member? :q2 width)
                              (> :q2 :q1))
                         (and (not= (+ q1pos (- :q2 :q1)) q2pos)
                              (not= (+ q1pos (- :q1 :q2)) q2pos)))))))

(defn nqueens3
  "the n-queens problem using a predicate definition"
  ([size ss]
   (eval ss (to-ast (nqueens-p 8 :queens))))
  ([size]
   (defonce ss (state-space))
   (nqueens3 size ss)))

