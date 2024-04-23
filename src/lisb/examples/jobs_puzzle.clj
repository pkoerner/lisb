(ns lisb.examples.jobs-puzzle
  (:require [lisb.core :refer [eval-ir-formula]]
            [lisb.translation.lisb2ir :refer [defpred bmap-set]]))

(defpred jobs-pred [holds-job]
  ;; using strings in lack of constants
    (clojure.core/let [roberta "Roberta"
        thelma "Thelma"
        steve "Steve"
        pete "Pete"
        
        chef "chef"
        guard "guard"
        nurse "nurse"
        clerk "clerk"
        police "police"
        teacher "teacher"
        actor "actor"
        boxer "boxer"
        holds-job-fn (partial fn-call holds-job)]
    (and
      (= :people #{roberta, thelma, steve, pete})
      (= :jobs #{chef guard nurse clerk police teacher actor boxer})
      
      (= :female #{roberta, thelma})
      (= :male #{steve, pete})
  
      (member? holds-job (-->> :jobs :people))
  
      (for-all [:x]
              (member? :x :people)
              (= 2 (count (|> holds-job #{:x}))))
  
      (member? (holds-job-fn nurse) :male)
      (member? (holds-job-fn actor) :male)
  
      (member? :husband (>+> :female :male))

      (member? [(holds-job-fn chef) (holds-job-fn clerk)] :husband)

      (not= (holds-job-fn boxer) roberta)

      (= :qualified-jobs #{police, teacher, nurse})

      ;; (not (member? pete (bimage holds-job :qualified-jobs)))
      (not (member? pete (bmap-set holds-job-fn :qualified-jobs)))

      (= :golfers #{roberta, (holds-job-fn chef) (holds-job-fn police)})
      (= (size :golfers) 3))))

(defn jobs []
  (eval-ir-formula (jobs-pred :jobs)))
