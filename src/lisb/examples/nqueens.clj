(ns lisb.nqueens
  (:require [lisb.core :refer [eval state-space]])
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]]))


(defn nqueens
  "the n-queens problem in B using lisp-like syntax"
  [size]
  (let [ss (state-space)
        width (binterval 1 :n)
        repr (band (b= :n size)
                   (bmember :queens (b>-> width width))
                   (bforall [:q1 :q2]
                            (b=> (band (bmember :q1 width)
                                       (bmember :q2 width)
                                       (b> :q2 :q1))
                                 (band (bnot= (b+ (bapply :queens :q1) (b- :q2 :q1)) (bapply :queens :q2))
                                       (bnot= (b+ (bapply :queens :q1) (b- :q1 :q2)) (bapply :queens :q2))))))
        result (eval ss (to-ast repr))]
    (prn result)))
