(ns lisb.adl.adl2lisb
  (:require [lisb.translation.util :refer :all]))

(defn assign [pc & kvs]
  (let [opname (keyword (gensym "assign"))
        newpc (inc pc)]
    {:pc newpc
     :ops [`(~opname [] (bprecondition (b= :pc ~pc)
                                     (bsequential-sub (bassign ~@kvs)
                                                      (bassign :pc ~newpc))))]}))

(defn accumulate-block [jump? a e]
  (let [{:keys [pc ops]} (apply (resolve (first e)) (if jump? jump? (:pc a)) (rest e))]
    {:pc pc, :ops (into (:ops a) ops)}))

(defn algorithm-aux [pc & args]
  (accumulate-block
    false
    (reduce (partial accumulate-block false)
          {:pc pc, :ops []}
          (butlast args))
    (last args)))

(defn while [pc condition & body]
  (let [opname-enter (keyword (gensym "while_enter"))
        opname-exit (keyword (gensym "while_exit"))
        body-pc (inc pc)
        res (apply algorithm-aux body-pc body)
        exit-pc (inc (:pc res))]
    {:pc exit-pc
     :ops (into (:ops res)
     [`(~opname-enter [] (bprecondition (band (b= :pc ~pc)
                                              ~condition)
                                        (bassign :pc ~body-pc)))
      `(~opname-exit [] (bprecondition (band (b= :pc ~pc)
                                             (bnot ~condition))
                                       (bassign :pc ~exit-pc)))])}))

(defn if 
  ([pc condition then] (apply if [pc condition then nil]))
  ([pc condition then else]
   (let [opname-then (keyword (gensym "ifte-then"))
         opname-else (keyword (gensym "ifte-else"))
         then-pc (inc pc)
         then-res (apply algorithm-aux then-pc [then])
         else-pc (:pc then-res)
         else-res (when else (apply algorithm-aux else-pc [else]))
         exit-pc (if else (:pc else-res) else-pc)]
    {:pc exit-pc
     :ops (into (:ops then-res)
      [`(~opname-then [] (bprecondition (band (b= :pc ~pc)
                                              ~condition)
                                        (bassign :pc ~then-pc)))
       `(~opname-else [] (bprecondition (band (b= :pc ~pc)
                                              (bnot ~condition))        
                                        (bassign :pc ~(if else else-pc exit-pc))))])})))

(defn do [pc & args]
  (apply algorithm-aux pc args))

(defmacro algorithm [& args]
  (apply algorithm-aux 0 args))

(clojure.pprint/pprint (macroexpand '(algorithm 
                          (while (> :x 0)
                             (assign :p (+ :p :y))
                             (assign :x (/ :x 2) :y (* :y 2)))             )))

(clojure.pprint/pprint (macroexpand '(algorithm
                           (while (> :x 0)
                             (if (not= 0 (mod :x 2))
                               (assign :p (+ :p :y)))
                             (assign :x (/ :x 2) :y (* :y 2))))))

                           

(comment 
(def programm-counter (atom 0))

(defn get-free-pc []
  (swap! programm-counter inc))

(defn process-assign [assign machine-ctx pc next-pc]
  (let [assign-counter (:assign-counter machine-ctx)]
    (->
      machine-ctx
      (update :assign-counter inc)
      (update :operations conj (list
                                 (keyword (str "assign" assign-counter))
                                 []
                                 (list 'pre
                                       (list '= :pc pc)
                                       (list 'sequential-sub
                                             assign
                                             (list 'assign :pc next-pc))))))))

(declare process-statement)
(declare process-statements)

(defn process-if [if machine-ctx pc next-pc]
  (let [if-counter (:if-counter machine-ctx)
        next-machine-ctx (update machine-ctx :if-counter inc)
        condition (nth if 1)
        then (nth if 2)
        then-pc (get-free-pc)]
    (if (= 3 (count if))
           ; if-then
           (let [next-machine-ctx (update next-machine-ctx :operations conj
                                          (list
                                            (keyword (str "if" if-counter "_then"))
                                            []
                                            (list 'pre
                                                  (list 'and
                                                        (list '= :pc pc)
                                                        condition)
                                                  (list 'assign :pc then-pc)))
                                          (list
                                            (keyword (str "if" if-counter "_else"))
                                            []
                                            (list 'pre
                                                  (list 'and
                                                        (list '= :pc pc)
                                                        (list 'not condition))
                                                  (list 'assign :pc next-pc))))
                 next-machine-ctx (process-statement then next-machine-ctx then-pc next-pc)]
             next-machine-ctx)
           ; if-then-else
           (let [next-machine-ctx (update next-machine-ctx :operations conj
                                          (list
                                            (keyword (str "if" if-counter "_then"))
                                            []
                                            (list 'pre
                                                  (list 'and
                                                        (list '= :pc pc)
                                                        condition)
                                                  (list 'assign :pc then-pc))))
                 next-machine-ctx (process-statement then next-machine-ctx then-pc next-pc)
                 else (nth if 3)
                 else-pc (get-free-pc)
                 next-machine-ctx (update next-machine-ctx :operations conj
                                          (list
                                            (keyword (str "if" if-counter "_else"))
                                            []
                                            (list 'pre
                                                  (list 'and
                                                        (list '= :pc pc)
                                                        condition)
                                                  (list 'assign :pc else-pc))))
                 next-machine-ctx (process-statement else next-machine-ctx else-pc next-pc)]
             next-machine-ctx))))

(defn process-while [while machine-ctx pc next-pc]
  (let [while-counter (:while-counter machine-ctx)
        next-machine-ctx (update machine-ctx :while-counter inc)
        condition (nth while 1)
        body-pc (get-free-pc)
        next-machine-ctx (update next-machine-ctx :operations conj
                                 (list
                                   (keyword (str "while" while-counter "_enter"))
                                    []
                                   (list 'pre
                                         (list 'and
                                               (list '= :pc pc)
                                               condition)
                                         (list 'assign :pc body-pc)))
                                 (list
                                   (keyword (str "while" while-counter "_exit"))
                                   []
                                   (list 'pre
                                         (list 'and
                                               (list '= :pc pc)
                                               (list 'not condition))
                                         (list 'assign :pc next-pc))))
        next-machine-ctx (process-statements (drop 2 while) next-machine-ctx body-pc pc)]
    next-machine-ctx))

(defn process-statement [statement machine-ctx pc next-pc]
  (cond
    (= 'assign (first statement)) (process-assign statement machine-ctx pc next-pc)
    (= 'if (first statement)) (process-if statement machine-ctx pc next-pc)
    (= 'while (first statement)) (process-while statement machine-ctx pc next-pc)
    (= 'do (first statement)) (process-statements (rest statement) machine-ctx pc next-pc)))

(defn process-statements
  ([statements machine-ctx]
   (reset! programm-counter 0)
   (process-statements statements machine-ctx 0))
  ([statements machine-ctx pc]
   (if (empty? statements)
     machine-ctx
     (let [next-pc (get-free-pc)
           next-machine-ctx (process-statement (first statements) machine-ctx pc next-pc)]
       (recur (rest statements) next-machine-ctx next-pc))))
  ([statements machine-ctx pc last-pc]
   (if (= 1 (count statements))
     (let [last (first statements)]
       (process-statement last machine-ctx pc last-pc))
     (let [next-pc (get-free-pc)
           next-machine-ctx (process-statement (first statements) machine-ctx pc next-pc)]
       (recur (rest statements) next-machine-ctx next-pc last-pc)))))


(defn process-var [var machine-ctx]
  (-> machine-ctx
      (update :variables conj (nth var 1))
      (update :invariants conj (nth var 2))
      (update :init conj (list 'assign (nth var 1) (nth var 3)))))

(defn process-adl [definitions machine-ctx]
  (if (empty? definitions)
    machine-ctx
    (let [definition (first definitions)
          new-machine-ctx (cond
                        (= 'var (first definition)) (process-var definition machine-ctx)
                        (= 'algorithm (first definition)) (process-statements (rest definition) machine-ctx)
                        )]
      (recur (rest definitions) new-machine-ctx))))

(defn create-machine-ctx [adl]
  (assert (= 'adl (first adl)) "Algorithm-Definition-Language should look like (adl ...)")
  (process-adl (drop 2 adl) {:name (second adl)
                             :variables  [:pc]
                             :invariants ['(in :pc nat-set)]
                             :init       ['(assign :pc 0)]
                             :operations []
                             :assign-counter 0
                             :if-counter 0
                             :while-counter 0
                             :do-counter 0}))

(defn build-machine [machine-ctx]
  (list 'machine
        (:name machine-ctx)
        (list* 'variables (:variables machine-ctx))
        (list* 'invariants (:invariants machine-ctx))
        (list* 'init (:init machine-ctx))
        (list* 'operations (:operations machine-ctx)) ))

(defn adl->lisb [adl]
  (-> adl
    (create-machine-ctx)
    (build-machine)))
)
