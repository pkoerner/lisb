(ns lisb.translation.eventb.util
  (:require [potemkin :refer [import-vars]]
            [lisb.prob.animator :refer [api injector]]
            [lisb.translation.eventb.b2eventb :as b2eventb]
            [lisb.translation.eventb dsl eventb2lisb]
            [lisb.translation.eventb.ir2eventb :refer [ir->prob-with-label]]
            )
  (:import
   de.prob.model.eventb.translate.ModelToXML
   (de.prob.model.eventb
    EventBModel)
   (de.prob.model.representation 
    DependencyGraph$ERefType)
   (de.prob.animator.domainobjects
    FormulaExpand
    EventB)
   ))

(import-vars [lisb.translation.eventb.dsl eventb])
(import-vars [lisb.translation.eventb.ir2eventb ir->prob ir-pred->str ir-expr->str])
(import-vars [lisb.translation.eventb.eventb2lisb prob->lisb])

(def modelCreator (.getProvider injector EventBModel))

(defn lisb->ir [lisb]
  (eval `(eventb ~lisb)))

(defn model-with-context [model context]
  (reduce (fn [model extended]
            (.addRelationship model (.getName context) (.getName extended) DependencyGraph$ERefType/EXTENDS))
          (.addContext model context)
          (.getExtends context)))

(defn model-with-machine [model machine]
  (let [refined (.getRefinesMachine machine)
        model (reduce (fn [model context]
                        (.addRelationship model (.getName machine) (.getName context) DependencyGraph$ERefType/SEES))
                      (.addMachine model machine)
                      (.getSees machine))]
    (if refined
      (.addRelationship model (.getName machine) (.getName refined) DependencyGraph$ERefType/REFINES)
      model)))

(defn prob-model [& machines-or-contexts]
  (reduce (fn [model value]
            (condp = (type value)
              de.prob.model.eventb.Context (model-with-context model value)
              de.prob.model.eventb.EventBMachine (model-with-machine model value)))
          (.get modelCreator) machines-or-contexts))

(defn ir->prob-model [& ir] (->> ir
                                 (map  b2eventb/transform-all-expressions)
                                 (map-indexed (fn [n ir] (ir->prob-with-label ir (str n "-"))))
                                 (apply prob-model)))


(defn prob-model->rodin [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn rodin->prob-model [filename]
  (-> (.eventb_load api filename)
      .getModel))


(defn rodin->lisb [filename] (-> filename rodin->prob-model prob->lisb))

(defn get-type
   [ss ir-formula]
    (let [formula (ir-expr->str ir-formula)
        ee (EventB. formula FormulaExpand/EXPAND)]
      (.getType (.typeCheck ss ee))))

(def get-statespace
  (memoize (fn [ir]
             (let [machine (ir->prob ir)
                   model (prob-model machine)]
               (.load model machine {})))))

(comment 
  (def xx (rodin->lisb "/home/philipp/Downloads/rodin/workspace/NewProject/ClockDeepInstance2.buc"))
  xx
  (eventb (context :ClockDeepInstance2 (sets :Ev) (constants :x) (axioms (= :x (comprehension-set [:t :m :h :mp :hp] true (|-> :t (|-> (|-> :m :h) (|-> :mp :hp))))))))
  (eventb (comprehension-set [:t] true (|-> :t (|-> (|-> :m :h) (|-> :mp :hp)))))
  (eventb (|-> :t (|-> (|-> :m :h) (|-> :mp :hp))))
  (def xx (rodin->lisb "/home/philipp/tmp/rodin/workspace/NewProject/Clock.bum"))
  (eval `(eventb ~(first xx)))
  (def ir (lisb->ir xx))

(def ir (eventb (machine :Clock
           (variables :h :m)
           (invariants (and (member? :m natural-set) (member? :h natural-set)) (and (< :m 60) (< :h 24)) (theorem (or (< :m 59) (or (and (= :m 59) (< :h 23)) (and (= :m 59) (= :h 23))))))
           (variant (- (* 24 60) 1 (+ :m (* :h 60))))
           (init (becomes-such [:m :h] (and (= :m' 0) (= :h' 0))))
           (events (event :tick_min (status :convergent) (when (< :m 59)) (then (becomes-such [:m] (= :m' (+ :m 1)))))
                   (event :tick_hour (status :convergent) (when (and (= :m 59) (< :h 23))) (then (becomes-such [:m :h] (and (= :m' 0) (= :h' (+ :h 1))))))
                   (event :tick_midnight (when (and (= :m 59) (= :h 23))) (then (becomes-such [:m :h] (and (= :m' 0) (= :h' 0)))))))))
  (prob-model->rodin (ir->prob-model ir) "MyModel" "/home/philipp/tmp/rodin/workspace/")

  ir
  (def xx (rodin->lisb "/home/philipp/Downloads/rodin/workspace/NewProject/Clock.bum"))
  (use 'lisb.translation.eventb.meta-dsl)
  (def irr (eval `(eventb ~(first xx))))
  (def irr2 (eval (transform irr)))
  irr2
  (def irr2 '{:tag :context, :name :Clock, :machine-clauses ({:tag :sets, :values ({:tag :deferred-set, :id :Ev})} {:tag :constants, :values (:init :Clock :tick_min :tick_hour :tick_midnight)} 
        {:tag :properties, :values ( 
   {:tag :equals, :left :x, :right {:tag :comprehension-set, :ids [:x], :pred {:tag :equals :left 1 :right 1}, :expr {:tag :maplet, :elems ([:tag :maplet] [:elems (:t {:tag :maplet, :elems (:h :m)})])}}}   )})})
  (prob-model->rodin (ir->prob-model irr2) "MyModel" "/home/philipp/tmp/rodin/workspace/")

  )
