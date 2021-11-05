(ns lisb.examples.nqueens
  (:require [lisb.core :refer [eval-ir-formula]]
            [lisb.translation.lisb2ir :refer :all]))



(defn nqueens
  "the n-queens problem in B using lisp-like syntax"
  ([size1]
   (clojure.core/let [width (binterval 1 :n)
         repr (band (b= :n size1)
                    (bmember? :queens (b>-> width width))
                    (bfor-all [:q1 :q2]
                              (band (bmember? :q1 width)
                                    (bmember? :q2 width)
                                    (b> :q2 :q1))
                              (band (bnot= (b+ (bfn-call :queens :q1) (b- :q2 :q1)) (bfn-call :queens :q2))
                                    (bnot= (b+ (bfn-call :queens :q1) (b- :q1 :q2)) (bfn-call :queens :q2)))))
         result (eval-ir-formula repr)]
     result)))


(defn nqueens2
  "the n-queens problem in B using the b macro"
  ([size1]
   (clojure.core/let [width (b (range 1 :n))
         q1pos (b (fn-call :queens :q1))
         q2pos (b (fn-call :queens :q2))
         repr  (b (and (= :n size1)
                       (member? :queens (>-> width width))
                       (for-all [:q1 :q2]
                                (and (member? :q1 width)
                                     (member? :q2 width)
                                     (> :q2 :q1))
                                (and (not= (+ q1pos (- :q2 :q1)) q2pos)
                                     (not= (+ q1pos (- :q1 :q2)) q2pos)))))
         result (eval-ir-formula repr)]
     result)))



(defpred nqueens-p [size1 sol]
  (clojure.core/let [width (range 1 size1)
        q1pos (fn-call sol :q1)
        q2pos (fn-call sol :q2)]
        (and (member? sol (>-> width width))
             (for-all [:q1 :q2]
                      (and (member? :q1 width)
                           (member? :q2 width)
                           (> :q2 :q1))
                      (and (not= (+ q1pos (- :q2 :q1)) q2pos)
                           (not= (+ q1pos (- :q1 :q2)) q2pos))))))

(defn nqueens3
  "the n-queens problem using a predicate definition"
  ([size]
   (eval-ir-formula (nqueens-p size :queens))))







;;; a solution in Clojure
;;; stolen from https://github.com/jeffbrown/clojure-n-queens.git

(defn is-valid-addition?
    [board proposed-position]
    (let [board-size (count board)]
        (not-any? true?
            (for [row-counter (range board-size) :let [pos (board row-counter)]]
                (or
                    (= pos proposed-position)
                    (= (- board-size row-counter) (Math/abs (- proposed-position pos))))))))

(defn add-to-boards
    [boards board-size]
    (for [board boards column-counter (range 1 (inc board-size)) :when (is-valid-addition? board column-counter)]
          (conj board column-counter)))
          
(defn solve-for-board-size [board-size]
    (nth (iterate (fn [boards] (add-to-boards boards board-size)) [[]]) board-size))
