(ns lisb.translation.eventb.eventb2lisb
  (:require [lisb.translation.ast2lisb :refer [ast->lisb]]
            [lisb.translation.lisb2ir :refer [b bop]]
            [clojure.string :as str])
  (:import de.prob.MainModule
           de.prob.scripting.EventBFactory
           (de.prob.animator.domainobjects EventB)
           (de.prob.model.representation ModelElementList)
           (de.prob.model.eventb
            EventBModel
            EventBMachine
            EventBVariable
            EventBInvariant
            EventParameter
            EventBConstant
            EventBAxiom
            EventBGuard
            EventBAction
            Event
            Event$EventType
            Event$Inheritance
            Witness
            Variant
            Context)))


(defn name-as-keyword [node] (-> node .getName keyword))

(defn with-optional [& nodes]
  (remove nil? nodes))

(defn optional [sym lisb]
    (when (seq lisb) (conj lisb sym)))

(defmulti prob->lisb
  "converts ProB model nodes into lisb"
  (fn [node] (class node)))

(defmethod prob->lisb EventBModel [node]
  (mapv prob->lisb (concat (.getContexts node) (.getMachines node))))

;; Machine

(defmethod prob->lisb EventBMachine [node]
  (let [sees (.getSees node)
        refines (.getRefinesMachine node)
        variables (.getVariables node)
        invariants (.getInvariants node)
        variant (.getVariant node)
        init (.getEvent node "INITIALISATION")
        events (remove #(= "INITIALISATION" (.getName %)) (.getEvents node))]
    (with-optional (if (nil? refines) 'machine 'refinement)
      (name-as-keyword node)
      (when refines (name-as-keyword refines))
      (when (seq sees) (list* 'sees (map name-as-keyword sees)))
      (when (seq variables) (list* 'variables (prob->lisb variables)))
      (when (seq invariants) (list* 'invariants (prob->lisb invariants)))
      (when (seq variant) (list 'variant (prob->lisb variant)))
      (when init (list* 'init (-> init .getActions prob->lisb)))
      (when (seq events) (list* 'events (map prob->lisb events))))))

(defmethod prob->lisb EventBVariable [node]
  (-> node .getExpression .getAst ast->lisb))

(defmethod prob->lisb EventBInvariant [node]
  (-> node .getPredicate .getAst ast->lisb))

(defmethod prob->lisb Variant [node]
  (-> node .getExpression .getAst ast->lisb))

(defmethod prob->lisb Event$EventType [node]
  (keyword (str/lower-case (str node))))

(defn extract-inheritance [node]
  (let [inheritance (.getInheritance node)]
    (when (not= inheritance Event$Inheritance/NONE)
      ;; using event-refines and event-extends, because extends is already used for contexts
      (list (symbol (str/lower-case (str "event-" inheritance))) (keyword (.getName (.getParentEvent node)))))))

(defmethod prob->lisb Event [node]
    (let [status (prob->lisb (.getType node))]
      (with-optional 'event (name-as-keyword node)
      (extract-inheritance node)
      (when (not= status :ordinary) (list 'status status))
      (optional 'any (prob->lisb (.getParameters node)))
      (optional 'when (prob->lisb (.getGuards node)))
      (optional 'with (prob->lisb (.getWitnesses node)))
      (optional 'then (prob->lisb (.getActions node))))))

(defmethod prob->lisb EventParameter [node]
  (-> node .getExpression .getAst ast->lisb))

(defmethod prob->lisb EventBGuard [node]
  (-> node .getPredicate .getAst ast->lisb))

(defmethod prob->lisb Witness [node]
  (-> node .getPredicate .getAst ast->lisb))

(defmethod prob->lisb EventBAction [node]
  (-> node .getCode .getAst ast->lisb))

(defmethod prob->lisb EventB [node]
  (-> node .getAst ast->lisb))

;; Context

(defmethod prob->lisb Context [node]
  (let [extends (map name-as-keyword (.getExtends node))]
    (with-optional 'context (name-as-keyword node)
    (when (seq extends) (list* 'extends extends))
    (optional 'sets (prob->lisb (.getSets node)))
    (optional 'constants (prob->lisb (.getConstants node)))
    (optional 'axioms (prob->lisb (.getAxioms node))))))

(defmethod prob->lisb de.prob.model.representation.Set [node]
  (name-as-keyword node))

(defmethod prob->lisb EventBConstant [node]
  (name-as-keyword node))

(defmethod prob->lisb EventBAxiom [node]
  (-> node .getPredicate .getAst ast->lisb))

;; Misc

(defmethod prob->lisb ModelElementList [node]
  (map prob->lisb node))

(defmethod prob->lisb nil [node]
  nil)
