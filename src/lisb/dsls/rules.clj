(ns lisb.dsls.rules)

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
  {:identifiers ids
   :where where ; conditions on identifiers
   :expect expect ; conditions that must be fulfilled for this rule
   :error-type ; number in range of error-types
   :counterexample counterexample ; of type INTEGER <-> STRING ; STRING_FORMAT("errorMessage ~w", identifier from list)
   })

(defn rule-fail [ids whenn error-type counterexample]
  {:identifiers ids
   :when whenn ; conditions on identifiers for a failing rule
   :error-type ; number in range of error-types
   :counterexample counterexample ; of type INTEGER <-> STRING ; STRING_FORMAT("errorMessage ~w", identifier from list)
   })


(defn computation [rule-dependencies computation-dependency activation replaces body]
  {:rule-dependencies rules-dependencies
   :computation-dependencies computation-dependencies
   :activation activation
   :replaces replaces ;; identifier of exactly one computation
   :body body })

(defn computation-body [define typey dummy-value value]
  {:define variable-name
   :type typey
   :dummy-value dummy-value ;; value before computation happens
   :value value ;; variable value after execution
   })

(defn function [output namey params precondition postcondition body output-assignment]
  {:output output ; variable name
   :name :namey ; function name
   :params params
   :precondition precondition ; predicate parameters must fulfill
   :postcondition postcondition ; output value(s) must satisfy postcondition
   :body body ; b statements
   :output-assignment output-assignment })


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
