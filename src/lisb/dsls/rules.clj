(ns lisb.dsls.rules
  (:require [lisb.translation.lisb2ir :refer :all]))

;; https://prob.hhu.de/w/index.php?title=Rules-DSL

(defn rule
  [namey rules-dependencies computation-dependencies activation errortypes & body]
  {:name namey
   :dependencies {:rules rules-dependencies
                  :computations computation-dependencies}
   :activation activation ;; some predicate
   :errortypes errortypes ;; positive number of error types?
   :body body})

(defn rule-forall [ids where expect error-type counterexample]
  {:tag :rule-forall
   :identifiers ids
   :where where ; conditions on identifiers
   :expect expect ; conditions that must be fulfilled for this rule
   :error-type error-type ; number in range of error-types
   :counterexample counterexample ; of type INTEGER <-> STRING ; STRING_FORMAT("errorMessage ~w", identifier from list)
   })

(defn rule-fail [ids whenn error-type counterexample]
  {:tag :rule-fail
   :identifiers ids
   :when whenn ; conditions on identifiers for a failing rule
   :error-type error-type ; number in range of error-types
   :counterexample counterexample ; of type INTEGER <-> STRING ; STRING_FORMAT("errorMessage ~w", identifier from list)
   })


(defn computation [namey rule-dependencies computation-dependencies activation replaces body]
  {:name namey
   :rule-dependencies rule-dependencies
   :computation-dependencies computation-dependencies
   :activation activation
   :replaces replaces ;; identifier of exactly one computation
   :body body })

(defn computation-body [variable-name typey dummy-value value]
  {:define variable-name
   :type typey
   :dummy-value dummy-value ;; value before computation happens
   :value value ;; variable value after execution
   })

(defn function [output namey params precondition postcondition body output-assignment]
  {:output output ; variable name
   :name namey ; function name
   :params params
   :precondition precondition ; predicate parameters must fulfill
   :postcondition postcondition ; output value(s) must satisfy postcondition
   :body body ; b statements
   :output-assignment output-assignment })

(defmulti body->substs (fn [body ctx] (:tag body)))
(defmethod body->substs :rule-forall [m rule]
  (bif-sub (bnot (bfor-all (get m :identifiers)
                           (get m :where)
                           (get m :expect)))
           (bsequential-sub
             (bassign (keyword (str (name (:name rule)) "_Counterexamples"))
                      (bunion (keyword (str (name (:name rule)) "_Counterexamples"))
                              (bmaplet (get m :error-type) (get m :counterexample))))
             (bassign (:name rule) "FAIL"
                    :$RESULT "FAIL"
                    :$COUNTEREXAMPLES (keyword (str (name (:name rule)) "_Counterexamples"))))))
(defmethod body->substs :rule-fail [m rule]
  (bif-sub (if (get m :identifiers)
             (bexists (get m :identifiers) (get m :when))
             (get m :when))
           (bsequential-sub
             (bassign (keyword (str (name (:name rule)) "_Counterexamples"))
                      (bunion (keyword (str (name (:name rule)) "_Counterexamples"))
                              (bmaplet (get m :error-type) (get m :counterexample))))
             (bassign (:name rule) "FAIL"
                    :$RESULT "FAIL"
                    :$COUNTEREXAMPLES (keyword (str (name (:name rule)) "_Counterexamples"))))))

(defn rule->machineparts [rule]
  {:variables [(:name rule) ;; string "SUCCESS", "NOT_CHECKED", "FAIL"
               (keyword (str (name (:name rule)) "_Counterexamples")) ;; {} ; mapping error code to string message?
               ]
   :operations (bop [:$RESULT :$COUNTEREXAMPLES] ; return values
                    (:name rule)
                    [] ; no params
                    (bselect (apply band (concat [(:activation rule)]
                                                 (map (partial b= "SUCCESS") (get-in rule [:dependencies :rules]))
                                                 (map (partial b= "EXECUTED") (get-in rule [:dependencies :computation-dependencies]))))
                             (bsequential-sub (apply bsequential-sub (mapcat body->substs (get rule :body)))
                                              (bif-sub (bnot= (:name rule) "FAIL")
                                                       (bassign (:name rule) "SUCCESS")))))})

(defn computation-body->mch-parts [comp-part]
  {:variables [(get comp-part :define)]
   :properties (bmember? (get comp-part :define) (get comp-part :type))
   :initialisation (bassign (get comp-part :define) (get comp-part :dummy-value))
   :assignments (bassign (get comp-part :define) (get comp-part :value))})

(defn merge-keys-with 
  ([merge-map m] m)
  ([merge-map m1 m2]
   (reduce (fn [m [k v]]
             (if (contains? m k)
               (update m k (get merge-map k) v)
               (assoc m k v)))
           m1
           m2)) 
  ([merge-map m1 m2 m3 & ms]
   (reduce (partial merge-keys-with merge-map) m1 (cons m2 (cons m3 ms)))))


(defn computation->machineparts [computation]
  (let [base (apply merge-keys-with 
                    {:variables concat
                     :properties band
                     :initialisation bparallel-sub
                     :assignments bsequential-sub }
                    (map computation-body->mch-parts (get computation :body)))]
    (-> base
        (dissoc :assignments)
        (assoc :operations [(bop (get computation :name)
                                 []
                                 (bselect (apply band (:activation computation)
                                                 (concat 
                                                   (map (partial b= "SUCCESS") (get computation :rule-dependencies))
                                                   (map (partial b= "EXECUTED") (get computation :computation-dependencies))))
                                          (get base :assignments)))]))))



(clojure.pprint/pprint
  (computation->machineparts (computation :COMP_Numbers
             []
             []
             true
             []
             [(computation-body :UpperBound
                               binteger-set
                               -1
                               100)
              (computation-body :EvenNumbers
                                (bpow binteger-set)
                                {} ;; not in original machine
                                (bcomprehension-set [:x] (band :x (binterval 0 :Upperbound)
                                                               (b= (bmod :x 2) 0))))
              (computation-body :NumberOfEvenNumbers
                                binteger-set
                                -1
                                (bcard :EvenNumbers))])))

;; more predicates:


;    SUCCEEDED_RULE(rule1): TRUE, if the check of rule1 succeeded
;    SUCCEEDED_RULE_ERROR_TYPE(rule1,1): TRUE, if the check of rule1 did not fail with error type 1
;    GET_RULE_COUNTEREXAMPLES(rule1): set of counterexamples of rule1
;    FAILED_RULE(rule1): TRUE, if the check of rule1 failed
;    FAILED_RULE_ERROR_TYPE(rule1,2): TRUE, if the check of rule1 failed with error type 2
;    FAILED_RULE_ALL_ERROR_TYPES(rule1): TRUE, if the check of rule1 failed with all possible error types for rule1
;    NOT_CHECKED_RULE(rule1): TRUE, if rule1 has not yet been checked
;    DISABLED_RULE(rule1): TRUE, if rule1 is disabled (i.e., the preconditions are not fulfilled)

; BODY allowed in RULE:
; FOR variable(s) IN set
; DO
;     operation(s)
; END
