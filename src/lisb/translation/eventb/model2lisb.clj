(ns lisb.translation.eventb.model2lisb
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

(defmulti prob->lisb (fn [node] (class node)))

(defmethod prob->lisb EventBModel [node]
  (mapv prob->lisb (concat (.getContexts node) (.getMachines node))))

;; Machine

(defmethod prob->lisb EventBMachine [node]
  (let [refines (.getRefinesMachine node)
        variant (.getVariant node)]
    (with-optional (if refines 'refinement 'machine)
      (name-as-keyword node)
      (when refines (name-as-keyword refines))
      (optional 'sees (map name-as-keyword (.getSees node)))
      (optional 'variables (prob->lisb (.getVariables node)))
      (optional 'invariants (prob->lisb (.getInvariants node)))
      (when variant (list 'variant (prob->lisb variant)))
      (optional 'init
                (-> node
                    (.getEvent "INITIALISATION")
                    .getActions
                    prob->lisb))
      (optional 'events
                (map prob->lisb (remove #(= "INITIALISATION" (.getName %))
                                       (.getEvents node)))))))

(defmethod prob->lisb EventBVariable [node]
  (-> node .getExpression .getAst prob->lisb))

(defmethod prob->lisb EventBInvariant [node]
  (-> node .getPredicate .getAst prob->lisb))

(defmethod prob->lisb Variant [node]
  (-> node .getExpression .getAst prob->lisb))

(defmethod prob->lisb Event [node]
  (concat (with-optional (name-as-keyword node)
            (optional 'any (prob->lisb (.getParameters node)))
            (optional 'when (prob->lisb (.getGuards node)))
            (optional 'with (prob->lisb (.getWitnesses node))))
          (prob->lisb (.getActions node))))

(defmethod prob->lisb EventParameter [node]
  (-> node .getExpression .getAst prob->lisb))

(defmethod prob->lisb EventBGuard [node]
  (-> node .getPredicate .getAst prob->lisb))

(defmethod prob->lisb Witness [node]
  (-> node .getPredicate .getAst prob->lisb))

(defmethod prob->lisb EventBAction [node]
  (-> node .getCode .getAst prob->lisb))

(defmethod prob->lisb EventB [node]
  (-> node .getAst prob->lisb ))

;; Context

(defmethod prob->lisb Context [node]
  (with-optional 'context (name-as-keyword node)
    (optional 'extends (prob->lisb (.getExtends node)))
    (optional 'sets (prob->lisb (.getSets node)))
    (optional 'constants (prob->lisb (.getConstants node)))
    (optional 'axioms (prob->lisb (.getAxioms node)))))

(defmethod prob->lisb de.prob.model.representation.Set [node]
  (name-as-keyword node))

(defmethod prob->lisb EventBConstant [node]
  (name-as-keyword node))

(defmethod prob->lisb EventBAxiom [node]
  (-> node .getPredicate .getAst prob->lisb))

;; Predicates & Expression

(defmethod prob->lisb ATypeofExpression [node]
  (prob->lisb (.getExpression node)))

(defmethod prob->lisb :default [node]
  (lisb.translation.ast2lisb/ast->lisb node))

;; Misc

(defmethod prob->lisb ModelElementList [node]
  (map prob->lisb node))

(defmethod prob->lisb nil [node]
  nil)

;; Util

(defn model2rodin! [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn rodin->lisb [filename]
  (-> (.eventb_load api filename)
      .getModel
      prob->lisb))

(comment
  (def bev (rodin->lisb "/workspaces/lisb/resources/eventb/ausleihsystem/Ausleihsystem.bum"))
  (clojure.pprint/pprint bev) 
  )
