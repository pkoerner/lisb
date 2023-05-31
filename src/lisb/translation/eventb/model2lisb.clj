(ns lisb.translation.eventb.model2lisb
  (:require [lisb.translation.ast2lisb]
            [lisb.translation.util :refer [bfile->b b->ast ir->b b->lisb b-predicate->ir]]
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

(defmulti ast->lisb (fn [node] (class node)))

(defmethod ast->lisb EventBModel [node]
  (mapv ast->lisb (concat (.getContexts node) (.getMachines node))))

;; Machine

(comment
  (-> "/home/julius/Uni/BA_Info/bachelor-rodin/hello_world/hello_world.bcm"
      eventb->state-space!
      .getModel
      ast->lisb
      pprint
      ))

(defmethod ast->lisb EventBMachine [node]
  (let [refines (.getRefinesMachine node)
        variant (.getVariant node)]
    (with-optional (if refines 'refinement 'machine)
      (name-as-keyword node)
      (when refines (name-as-keyword refines))
      (optional 'sees (map name-as-keyword (.getSees node)))
      (optional 'variables (ast->lisb (.getVariables node)))
      (optional 'invariants (ast->lisb (.getInvariants node)))
      (when variant (list 'variant (ast->lisb variant)))
      (optional 'init
                (-> node
                    (.getEvent "INITIALISATION")
                    .getActions
                    ast->lisb))
      (optional 'events
                (map ast->lisb (remove #(= "INITIALISATION" (.getName %))
                                       (.getEvents node)))))))

(defmethod ast->lisb EventBVariable [node]
  (-> node .getExpression .getAst ast->lisb))

(defmethod ast->lisb EventBInvariant [node]
  (-> node .getPredicate .getAst ast->lisb))

(defmethod ast->lisb Variant [node]
  (-> node .getExpression .getAst ast->lisb))

(defmethod ast->lisb Event [node]
  (concat (with-optional (name-as-keyword node)
            (optional 'any (ast->lisb (.getParameters node)))
            (optional 'where (ast->lisb (.getGuards node)))
            (optional 'with (ast->lisb (.getWitnesses node))))
          (ast->lisb (.getActions node))))

(defmethod ast->lisb EventParameter [node]
  (-> node .getExpression .getAst ast->lisb))

(defmethod ast->lisb EventBGuard [node]
  (-> node .getPredicate .getAst ast->lisb))

(defmethod ast->lisb Witness [node]
  (-> node .getPredicate .getAst ast->lisb))

(defmethod ast->lisb EventBAction [node]
  (-> node .getCode .getAst ast->lisb))

(defmethod ast->lisb EventB [node]
  (-> node .getAst ast->lisb ))

;; Context

(defmethod ast->lisb Context [node]
  (with-optional 'context (name-as-keyword node)
    (optional 'extends (ast->lisb (.getExtends node)))
    (optional 'sets (ast->lisb (.getSets node)))
    (optional 'constants (ast->lisb (.getConstants node)))
    (optional 'axioms (ast->lisb (.getAxioms node)))))

(defmethod ast->lisb de.prob.model.representation.Set [node]
  (name-as-keyword node))

(defmethod ast->lisb EventBConstant [node]
  (name-as-keyword node))

(defmethod ast->lisb EventBAxiom [node]
  (-> node .getPredicate .getAst ast->lisb))

;; Predicates & Expression

(defmethod ast->lisb ATypeofExpression [node]
  (ast->lisb (.getExpression node)))

(defmethod ast->lisb :default [node]
  (lisb.translation.ast2lisb/ast->lisb node))

;; Misc

(defmethod ast->lisb ModelElementList [node]
  (map ast->lisb node))

(defmethod ast->lisb nil [node]
  nil)

;; Util

(defn model2rodin! [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn eventb->state-space! [filename]
  (.eventb_load api filename))

(comment
  (require '[clojure.pprint :refer [pprint]])

  (-> "/home/julius/Uni/BA_Info/bachelor-rodin/hello_world/hello_world.bcm"
      eventb->state-space!
      .getModel
      ast->lisb
      pprint
      )
  )
