(ns lisb.adl.adl2lisb
  (:require [lisb.translation.util :refer :all]))

(defn assert [pc pred]
  {:pc         (inc pc)
   :ops        (fn [jump?]
                 (let [opname (keyword (str "assert" pc))
                       newpc (if jump? jump? (inc pc))]
                   [`(bop ~opname [] (bprecondition (b= :pc ~pc)
                                                    (bassign :pc ~newpc)))]))
   :invariants [`(bimplication (b= :pc ~pc) (b ~pred))]})

(defn assign [pc & kvs]
  {:pc         (inc pc)
   :ops        (fn [jump?]
                 (let [opname (keyword (str "assign" pc))
                       newpc (if jump? jump? (inc pc))]
                   [`(bop ~opname [] (bprecondition (b= :pc ~pc)
                                                    (bsequential-sub (b (bassign ~@kvs))
                                                                     (bassign :pc ~newpc))))]))
   :invariants []})

(defn do [pc & args]
  (loop [[instr & instrs :as _allinstrs] args
         pc pc
         ops []
         invariants []]
    (let [res (apply (resolve (first instr)) pc (rest instr))]
      (if (seq instrs)
        (recur instrs (:pc res) (into ops ((:ops res) nil)) (into invariants (:invariants res)))
        {:pc (:pc res)
         :ops (fn [jump] (into ops ((:ops res) jump)))
         :invariants (into invariants (:invariants res))}))))


(defn while [pc condition & body]
  (let [opname-enter (keyword (str "while-enter" pc))
        opname-exit (keyword (str "while-exit" pc))
        body-pc (inc pc)
        res (apply do body-pc body)]
    {:pc         (:pc res)
     :ops        (fn [jump?]
                   (let [exit-pc (if jump? jump? (:pc res))]
                     (into [`(bop ~opname-enter [] (bprecondition (band (b= :pc ~pc)
                                                                        (b ~condition))
                                                                  (bassign :pc ~body-pc)))
                            `(bop ~opname-exit [] (bprecondition (band (b= :pc ~pc)
                                                                       (bnot (b ~condition)))
                                                                 (bassign :pc ~exit-pc)))]
                           ((:ops res) pc))))
     :invariants (:invariants res)}))


(defn if 
  ([pc condition then] (apply if pc condition then [nil]))
  ([pc condition then else]
   (let [opname-then (keyword (str "if-then" pc))
         opname-else (keyword (str "if-else" pc))
         then-pc (inc pc)
         then-res (apply do then-pc [then])
         else-pc (:pc then-res)
         else-res (when else (apply do else-pc [else]))
         exit-pc (if else (:pc else-res) else-pc)]
     {:pc         exit-pc
      :ops        (fn [jump?]
                    (concat [`(bop ~opname-then [] (bprecondition (band (b= :pc ~pc)
                                                                        (b ~condition))
                                                                  (bassign :pc ~then-pc)))
                             `(bop ~opname-else [] (bprecondition (band (b= :pc ~pc)
                                                                        (bnot (b ~condition)))
                                                                  (bassign :pc ~(if else else-pc (if jump? jump? exit-pc)))))]
                            ((:ops then-res) (if jump? jump? exit-pc))
                            (when else ((:ops else-res) (if jump? jump? exit-pc)))))
      :invariants (into (:invariants then-res) (:invariants else-res))})))

(defmacro algorithm [& args]
  (let [res (apply lisb.adl.adl2lisb/do 0 args)
        operations ((:ops res) nil)
        invariants (:invariants res)]
    `{:invariants ~invariants
      :operations ~operations}))

(defn var [var]
  {:variables  [(first var)]
   :invariants [`(b ~(second var))]
   :init       [`(bassign ~(first var) (b ~(nth var 2)))]})

(defn process-decls [decls]
  (loop [[decl & decls] decls
         m {}]
    (if decl
      (recur decls (merge-with into m ((resolve (first decl)) (rest decl))))
      m)))

(defn build-machine [name clauses]
  `(bmachine
     ~name
     (bvariables ~@(:variables clauses))
     (binvariants ~@(:invariants clauses))
     (binit ~@(:init clauses))
     (boperations ~@(:operations clauses))))

(defmacro adl [name & args]
  (let [decls (process-decls (cons `(var :pc (bmember? :pc bnat-set) 0) (butlast args)))
        algorythm (macroexpand (last args))]
    (build-machine name (merge-with into decls algorythm))))
