(ns lisb.examples.crowded-chessboard
  (:require [clojure.set :refer [union]]
            [clojure.pprint :refer [print-table]]
            [lisb.prob.animator :refer [api]]
            [lisb.core :refer [eval-ir-formula]]
            [lisb.translation.lisb2ir :refer :all]))


(defn attack-horizontal 
  "only considers fields to the right (symmetry optimisation)"
  [n i j]
  (map #(vector % j) (range (inc i) (inc n))) )

(defn attack-vertical 
  "only considers fields above (symmetry optimisation)"
  [n i j]
  (map (partial vector i) (range (inc j) (inc n))))

(defn attack-diag1
  "only considers fields to the right (symmetry optimisation)"
  [n i j]
  (for [k (range 1 #_(- n) (inc n))
        :let [a (+ i k)
              b (+ j k)]
        :when (and (<= 1 a n) (<= 1 b n) (not= n 0))]
    [a b]))

(defn attack-diag2 
  "only considers fields to the right (symmetry optimisation)"
  [n i j]
  (for [k (range 1 #_(- n) (inc n))
        :let [a (+ i k)
              b (- j k)]
        :when (and (<= 1 a n) (<= 1 b n) (not= n 0))]
    [a b]))

(defn attack-diag
  "only considers fields to the right (symmetry optimisation)"
  [n i j]
  (concat (attack-diag1 n i j)
          (attack-diag2 n i j)))

(defn attack-knight
  "only considers fields to the right (symmetry optimisation)"
  [n i j]
  (for [[x y] [[1 2] #_[-1 2] [1 -2] #_[-1 -2]
               [2 1] #_[-2 1] [2 -1] #_[-2 -1]]
        :let [a (+ x i)
              b (+ y j)]
        :when (and (<= 1 a n) (<= 1 b n))]
    [a b]))

(defn attack-queen [size i j]
  (concat (attack-horizontal size i j)
          (attack-vertical size i j)
          (attack-diag size i j)))

(defn attack-rook [size i j]
  (concat (attack-horizontal size i j)
          (attack-vertical size i j)))

(def attack-bishop attack-diag)

(defn transform-position [size i j]
  (+ i (* size (dec j))))


(defn attack [size figure attack-fn]
  (apply band (for [i (range 1 (inc size))
                    j (range 1 (inc size))
                    [a b] (attack-fn size i j)]
                (bimplication (b= figure (bfn-call :board (transform-position size i j)))
                              (bnot= figure (bfn-call :board (transform-position size a b)))))))

(defn how-many [figure amount]
  (b= amount (bcard (blambda [:pos]
                              (band (bmember? :pos (bdom :board)) (b= figure (bfn-call :board :pos)))
                              :pos))))

(defn create-machine []
  (let [tf (java.io.File/createTempFile "lisb" ".mch" nil)
        tn (.getAbsolutePath tf)]
    (.deleteOnExit tf)
    (spit tf "MACHINE chessboard \n DEFINITIONS SET_PREF_KODKOD_MAX_NR_SOLS == 1; \n SET_PREF_KODKOD_SAT_SOLVER == \"glucose\"; \n SET_PREF_KODKOD_SYMMETRY == 20; \n SETS FIGURES = {queen, rook, bishop, knight, empty} \n  END")
    tn))

(defn crowded-empty-state-space []
  (let [machine (create-machine)]
    (.b_load api machine {"KODKOD" "true"
                          "TIME_OUT" "50000"})))

(defn crowded-chessboard
  "describes the crowded chessboard puzzle"
  ([size amount-knights ss]
   (let [field (binterval 1 (b* :n :n))
         amount-bishops (if (= size 4) 5 (- (* 2 size) 2))
         repr (b (and (= :n size)
                                    (bmember? :board (b--> field :figures))
                                    (how-many :queen size)
                                    (how-many :rook size)
                                    (how-many :bishop amount-bishops)
                                    (how-many :knight amount-knights)
                                    (attack size :queen attack-queen)
                                    (attack size :rook attack-rook)
                                    (attack size :bishop attack-bishop)
                                    (attack size :knight attack-knight)))
         result (eval-ir-formula ss repr)]
     result))
  ([size amount-knights]
   (let [ss (crowded-empty-state-space)]
     (crowded-chessboard size amount-knights ss))))


(defn untransform-position [size [n v]]
  (let [n (dec n)]
    [(inc (quot n size)) (inc (mod n size)) v]))


(defn cc [size amount-knights]
  (time (let [sol ((crowded-chessboard size amount-knights) "board")
              sorted-sol (sort-by first (map (fn [x] [(first x) (.getValue (second x))]) sol))]
          (print-table [0 1 2] (map (partial untransform-position size) sorted-sol)))))

#_(cc 8 21)
