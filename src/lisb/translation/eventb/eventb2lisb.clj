(ns lisb.translation.eventb.eventb2lisb
  (:require [lisb.translation.ast2lisb]
            [lisb.prob.animator :refer [api injector]]
            [lisb.translation.lisb2ir :refer [b bop]])
  (:import com.google.inject.Guice
           com.google.inject.Stage
           de.prob.MainModule
           de.prob.scripting.EventBFactory
           de.prob.model.eventb.translate.ModelToXML
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
            Witness
            Variant
            Context)
           (de.be4.classicalb.core.parser.node
            ATypeofExpression
            )))


(defn name-as-keyword [node] (-> node .getName keyword))

(defn with-optional [& nodes]
  (remove nil? nodes))

(defn optional [sym lisb]
    (when (seq lisb) (conj lisb sym)))

(defmulti prob->eventb (fn [node] (class node)))

(defmethod prob->eventb EventBModel [node]
  (mapv prob->eventb (concat (.getContexts node) (.getMachines node))))

;; Machine

(defmethod prob->eventb EventBMachine [node]
  (let [refines (.getRefinesMachine node)
        variant (.getVariant node)]
    (with-optional (if refines 'refinement 'machine)
      (name-as-keyword node)
      (when refines (name-as-keyword refines))
      (optional 'sees (map name-as-keyword (.getSees node)))
      (optional 'variables (prob->eventb (.getVariables node)))
      (optional 'invariants (prob->eventb (.getInvariants node)))
      (when variant (list 'variant (prob->eventb variant)))
      (optional 'init
                (-> node
                    (.getEvent "INITIALISATION")
                    .getActions
                    prob->eventb))
      (optional 'events
                (map prob->eventb (remove #(= "INITIALISATION" (.getName %))
                                       (.getEvents node)))))))

(defmethod prob->eventb EventBVariable [node]
  (-> node .getExpression .getAst prob->eventb))

(defmethod prob->eventb EventBInvariant [node]
  (-> node .getPredicate .getAst prob->eventb))

(defmethod prob->eventb Variant [node]
  (-> node .getExpression .getAst prob->eventb))

(defmethod prob->eventb Event [node]
  (concat (with-optional (name-as-keyword node)
            (optional 'any (prob->eventb (.getParameters node)))
            (optional 'when (prob->eventb (.getGuards node)))
            (optional 'with (prob->eventb (.getWitnesses node))))
          (prob->eventb (.getActions node))))

(defmethod prob->eventb EventParameter [node]
  (-> node .getExpression .getAst prob->eventb))

(defmethod prob->eventb EventBGuard [node]
  (-> node .getPredicate .getAst prob->eventb))

(defmethod prob->eventb Witness [node]
  (-> node .getPredicate .getAst prob->eventb))

(defmethod prob->eventb EventBAction [node]
  (-> node .getCode .getAst prob->eventb))

(defmethod prob->eventb EventB [node]
  (-> node .getAst prob->eventb ))

;; Context

(defmethod prob->eventb Context [node]
  (with-optional 'context (name-as-keyword node)
    (optional 'extends (prob->eventb (.getExtends node)))
    (optional 'sets (prob->eventb (.getSets node)))
    (optional 'constants (prob->eventb (.getConstants node)))
    (optional 'axioms (prob->eventb (.getAxioms node)))))

(defmethod prob->eventb de.prob.model.representation.Set [node]
  (name-as-keyword node))

(defmethod prob->eventb EventBConstant [node]
  (name-as-keyword node))

(defmethod prob->eventb EventBAxiom [node]
  (-> node .getPredicate .getAst prob->eventb))

;; Predicates & Expression

(defmethod prob->eventb ATypeofExpression [node]
  (prob->eventb (.getExpression node)))

(defmethod prob->eventb :default [node]
  (lisb.translation.ast2lisb/ast->lisb node))

;; Misc

(defmethod prob->eventb ModelElementList [node]
  (map prob->eventb node))

(defmethod prob->eventb nil [node]
  nil)

;; Util

(defn model2rodin! [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn rodin->lisb [filename]
  (-> (.eventb_load api filename)
      .getModel
      prob->eventb))

(comment
  (def bev (rodin->lisb "/workspaces/lisb/resources/eventb/ausleihsystem/Ausleihsystem.bum"))
  (clojure.pprint/pprint bev) 
  )
