(ns lisb.translation.eventb.eventb2ir
  (:require [lisb.translation.lisb2ir :as lisb2ir]
            [lisb.translation.util :refer [bfile->b b->ast ir->b b->lisb b-predicate->ir]]
            [lisb.prob.animator :refre [injector api]]
            [lisb.translation.lisb2ir :refer [b bop]])
  (:import com.google.inject.Guice
           com.google.inject.Stage
           de.prob.MainModule
           de.prob.scripting.EventBFactory
           de.prob.model.eventb.translate.ModelToXML
           (de.prob.model.eventb
            EventBMachine
            EventBInvariant
            EventBGuard
            Event
            Context)))

(def rodin-factory (.getInstance injector EventBFactory))

(defn rodin2model! [file]
  (.extract rodin-factory file))

(defn model2rodin! [model model-name path]
  (.writeToRodin (ModelToXML.) model model-name path))

(defn name-as-keyword [node] (-> node .getName keyword))

(defn extract-sees [node]
  (apply lisb2ir/bsees (map name-as-keyword (.getName (.getSees node)))))

(defn extract-variables [node]
  (apply lisb2ir/bvariables (map name-as-keyword (.getVariables node))))

(defn extract-invariants [node]
  (apply lisb2ir/binvariants (map ast->ir (.getInvariants node))))

(defn code->ir [node] (-> node .getCode b-predicate->ir))

(defn pred->ir [pred] (-> pred .getPredicate .getCode b-predicate->ir))

(defn extract-events [node]
  (lisb2ir/bop [] no))

(defmulti ast->ir (fn [node] (class node)))

(defmethod ast->ir EventBMachine [node]
  (lisb2ir/bmachine (keyword (.getName node))
                    (extract-sees node)
                    (extract-variables node)
                    (extract-invariants node)))

(defmethod ast->ir EventBInvariant [node]
  (assoc (-> node .getPredicate .getCode b-predicate->ir)
         :name (name-keyword node)))

(defmethod ast->ir EventBGuard [node]
  (assoc (-> node .getPredicate .getCode b-predicate->ir)
         :name (name-keyword node)))

(defmethod ast->ir EventBAction [node]
  (assoc (-> node .getCode b-predicate->ir)
         :name (name-keyword node)))

(defmethod ast->ir Event [node]
  {:tag         :event
   :name        (name-keyword node)
   :args        (map name-keyword (.getParameters node))
   ;:witnesses   (.getWittnesses node)
   :guards      (map ast->ir (.getGuards node))
   :actions     (.getActions node)})


(defn eventb->state-space! [filename]
  (.eventb_load api filename))

(defn eventbfile->b
  "Use this instead of slurping a machine file.
  Will load an .mch file with ProB and obtain an internal representation.
  Mainly used to circumvent definitions."
  [filename]
  (let [animator (eventb->state-space! filename)
        cmd (de.prob.animator.command.GetInternalRepresentationPrettyPrintCommand.)]
    (.execute animator cmd)
    (.getPrettyPrint cmd)))

(comment
  (require '[clojure.reflect :as r]
           '[clojure.pprint :refer [pprint]])
  (def extracted-model (rodin2model! "/home/julius/rodin-workspace/Ausleihsystem/Ausleihsystem.bcm"))
  (def main-component (.getMainComponent extracted-model))
  (ast->ir main-component)
  (->> main-component .getEvents first r/reflect :members (map :name))
  (->> main-component .getEvents second .getParameters (map name-keyword))
  (->> main-component .getEvents second ast->ir)
  (def model (.getModel extracted-model))
  (def rodin-file (model2rodin! model "MyAusleihsystem" "."))


  (spit "BevergeVendingMachine.mch"
        (eventbfile->b "/home/julius/rodin-workspace/BevergeVendingMachine.zip_expanded/BeverageVendingMachine/BeverageVendingMachine.bcm"))
  (spit "BevergeVendingMachine_cola.mch"
        (eventbfile->b "/home/julius/rodin-workspace/BevergeVendingMachine.zip_expanded/BeverageVendingMachine/BeverageVendingMachine_cola.bcm"))
  (b->lisb (slurp "BeverageVendingMachine.mch"))
  )
