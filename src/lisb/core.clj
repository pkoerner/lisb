(ns lisb.core
  (:require [clojure.walk :refer [walk]])
  (:import de.prob.Main
           de.prob.scripting.Api
           de.prob.animator.command.CbcSolveCommand
           de.prob.animator.domainobjects.ClassicalB
           (de.be4.classicalb.core.parser.node Start
                                               EOF
                                               Node
                                               AAddExpression
                                               APredicateParseUnit
                                               AIntegerExpression
                                               AIdentifierExpression
                                               TIntegerLiteral
                                               TIdentifierLiteral
                                               AConjunctPredicate
                                               ALessPredicate)))


(defn get-api [] (.getInstance (Main/getInjector) Api))

(defn create-empty-machine []
  (let [tf (java.io.File/createTempFile "evalb" ".mch" nil)
        tn (.getAbsolutePath tf)
        ]
    (.deleteOnExit tf)
    (spit tf "MACHINE empty \n END")
    tn))

(defn state-space []
  (let [machine (create-empty-machine)
        api (get-api)]
    (.b_load api machine)))





(defn predicate [ast]
  (let [p (APredicateParseUnit. ast)
        start (Start. p (EOF.))]
    (ClassicalB. start)))

(defn identifier [n]
  (let [token (TIdentifierLiteral. n)]
    (AIdentifierExpression. [token])))

(defn integer [n]
  (let [token (TIntegerLiteral. (str n))]
    (AIntegerExpression. token)))

(defn lessnode [l r]
  (ALessPredicate. l r))

(defn plusnode [l r]
  (AAddExpression. l r))

(defn conjunctionnode [l r]
  (AConjunctPredicate. l r))



(defn conjunct [l r]
  {:tag :and
   :children [l r]})


(defn plus [l r]
  {:tag :plus
   :children [l r]})


(defn less [l r]
  {:tag :less
   :children [l r]})


(def to-ast-map {:less lessnode
                 :plus plusnode
                 :and  conjunctionnode
                 })


(defn chain 
  [f nodes]
  (let [tuples (partition 2 1 nodes)]
    (reduce conjunct (map (partial apply f) tuples))))

(defn b< [& args]
  (chain less args))

(defn b+ [& args]
  (reduce plus args))

(defn band [& args]
  (reduce conjunct args))




(defn literal [x]
  (cond (keyword? x) (identifier (name x))
        (number? x) (integer x)))



(defn to-ast-inner [data]
  (if (map? data)
      (let [processed-args (walk to-ast-inner identity (:children data))]
        (apply (to-ast-map (:tag data)) processed-args))
      (literal data)))


(defn to-ast [data]
  (walk to-ast-inner  identity [ data]))



(defn eval [state-space ast]
  (let [cmd (CbcSolveCommand. (predicate ast))
        _ (.execute state-space cmd)
        free (.getFreeVariables cmd)
        result (.. cmd getValue translate)
        ]
    (when (.. result getValue booleanValue)
      (into {} (map (fn [k][k (.getSolution result k)]) free)))))


