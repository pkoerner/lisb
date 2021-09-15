(ns lisb.examples.nqueens
  (:require [lisb.core :refer [eval-ir-formula]]
            [lisb.translation.lisb2ir :refer :all]))



(defn nqueens
  "the n-queens problem in B using lisp-like syntax"
  ([size]
   (let [width (binterval 1 :n)
         repr (band (b= :n size)
                    (bmember? :queens (b>-> width width))
                    (bfor-all [:q1 :q2]
                             (b=> (band (bmember? :q1 width)
                                        (bmember? :q2 width)
                                        (b> :q2 :q1))
                                  (band (bnot= (b+ (bapply :queens :q1) (b- :q2 :q1)) (bapply :queens :q2))
                                        (bnot= (b+ (bapply :queens :q1) (b- :q1 :q2)) (bapply :queens :q2))))))
         result (eval-ir-formula repr)]
     result)))


(defn nqueens2
  "the n-queens problem in B using the b macro"
  ([size]
   (let [width (b (range 1 :n))
         q1pos (b (apply :queens :q1))
         q2pos (b (apply :queens :q2))
         repr  (b (and (= :n size)
                                     (member? :queens (>-> width width))
                                     (for-all [:q1 :q2]
                               (=> (and (member? :q1 width)
                                        (member? :q2 width)
                                        (> :q2 :q1))
                                   (and (not= (+ q1pos (- :q2 :q1)) q2pos)
                                        (not= (+ q1pos (- :q1 :q2)) q2pos))))))
         result (eval-ir-formula repr)]
     result)))



(defpred nqueens-p [size sol]
  (let [width (range 1 size)
        q1pos (apply sol :q1)
        q2pos (apply sol :q2)]
        (and (member? sol (>-> width width))
             (for-all [:q1 :q2]
                     (=> (and (member? :q1 width)
                              (member? :q2 width)
                              (> :q2 :q1))
                         (and (not= (+ q1pos (- :q2 :q1)) q2pos)
                              (not= (+ q1pos (- :q1 :q2)) q2pos)))))))

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
