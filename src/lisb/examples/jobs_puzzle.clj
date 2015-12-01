(ns lisb.examples.jobs-puzzle
  (:require [lisb.core :refer [eval]])
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]]))

(defpred jobs-pred [holds-job]
  ;; using strings in lack of constants
  (let [roberta (bstr "Roberta")
        thelma (bstr "Thelma")
        steve (bstr "Steve")
        pete (bstr "Pete")
        
        chef (bstr "chef")
        guard (bstr "guard")
        nurse (bstr "nurse")
        clerk (bstr "clerk")
        police (bstr "police")
        teacher (bstr "teacher")
        actor (bstr "actor")
        boxer (bstr "boxer")
        holds-job-fn (partial apply holds-job)]
    (and
      (= :people #{roberta, thelma, steve, pete})
      (= :jobs #{chef guard nurse clerk police teacher actor boxer})
      
      (= :female #{roberta, thelma})
      (= :male #{steve, pete})
  
      (member? holds-job (-->> :jobs :people))
  
      (forall [:x]
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
      (= (count :golfers) 3))))

(defn jobs []
  (eval (to-ast (jobs-pred :jobs))))
