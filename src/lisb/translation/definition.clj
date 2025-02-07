(ns lisb.translation.definition
  (:require [lisb.translation.lisb2ir :refer [bexpand]])
  (:use lisb.translation.util))

(defmacro op [bop example lisb-sig docstr & aliases]
  {:type :operator
   :operator `'~(first example)
   :b-operator bop
   :lisb-fn `'~(first (bexpand example))
   :arguments `(quote ~lisb-sig)
   :example `'~example
   :doc docstr
   :ir `(b ~example)})

(defmacro literal [example docstr]
  {:type :literal
   :lisb-literal example
   :b-literal (lisb->b example)
   :doc docstr})

(defmacro constant [const docstr]
  {:type :constant
   :lisb-constant `'~const
   :b-literal (lisb->b const)
   :lisb-var `'~(bexpand const)
   :ir `(b ~const)
   :doc docstr})


(defmulti print-doc (fn [m long?] (:type m)))
(defmethod print-doc :operator [{:keys [operator lisb-fn b-operator example arguments doc ir]} long?]
  (when long? (println "-------------------------"))
  (println operator "|" (str lisb-fn " | " ir) "| B:" b-operator)
  (when long? (println arguments " e.g." example))
  (when long? (println "Operator"))
  (when long? (println "  " doc)))
(defmethod print-doc :literal [{:keys [lisb-literal b-literal doc]} long?]
  (when long? (println "-------------------------"))
  (println lisb-literal "| B:" b-literal)
  (when long? (println "Literal"))
  (when long? (println "  " doc)))
(defmethod print-doc :constant [{:keys [lisb-constant b-literal lisb-var ir doc]} long?]
  (when long? (println "-------------------------"))
  (println lisb-constant "|" (str lisb-var " | " ir) "| B:" b-literal)
  (when long? (println "Constant"))
  (when long? (println "  " doc)))

;; NOTE: started work with https://pkoerner.github.io/lisb-doc/docs/numbers/
;; covered 1. -- 3. 

(def b-info [ 
(literal 42 "Number literal.")

(constant min-int
  "Smallest implementable integer number.
   Configurable using ProB's MININT preference.")
(constant max-int
  "Largest implementable integer number.
   Configurable using ProB's MAXINT preference.")

(constant int-set 
  "Set of all implementable integer numbers ranging from min-int to max-int.
   See also: integer-set, min-int, max-int.") 
(constant nat-set 
  "Set of all implementable natural numbers between min-int to max-int.
   See also: natural-set, min-int, max-int.") 
(constant nat1-set 
  "Set of all implementable natural numbers between min-int to max-int excluding zero.
   See also: natural1-set, min-int, max-int.") 
(constant integer-set 
  "Set of all integer numbers.") 
(constant natural-set 
  "Set of all natural numbers (including zero).
   See also: natural1-set.") 
(constant natural1-set 
  "Set of all natural numbers (excluding zero).
   See also: natural-set.") 

(op "+" (+ 1 2 3) [& nums] "Addition operator on numbers" )

])

(defn bpropos [search & {:as opts :keys [short]}]
  (let [search (clojure.string/lower-case search)]
    (doseq [res (filter (fn [x] (some #(clojure.string/includes?
                                         (clojure.string/lower-case 
                                           (str %))
                                         search)
                                      (vals x)))
                        b-info)]
      (print-doc res (not short)))))

(comment (bpropos "add" :short false))
