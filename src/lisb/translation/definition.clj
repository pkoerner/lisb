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
   :bexample (lisb->b example)
   :doc docstr
   :ir `(b ~example)})

(defmacro literal [example docstr]
  {:type :literal
   :lisb-literal `(quote ~example)
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
(defmethod print-doc :operator [{:keys [operator lisb-fn b-operator example bexample arguments doc ir]} long?]
  (when long? (println "-------------------------"))
  (println operator "|" (str lisb-fn " | " ir) "| B:" bexample #_b-operator)
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

; Missing:
;;    Predicates
;;    Set Operators
;;    Relations
;;    Functions
;;    Sequences
;;    Records
;;    Substitutions
;; Machine Structure


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

;; --
(op "interval" (interval 1 10) [lower upper] 
  "The set of integer numbers ranging from lower (inclusive) to upper (inclusive).
   See also: range")

(op "range" (range 1 10) [lower upper] 
  "The set of integer numbers ranging from lower (inclusive) to upper (exclusive).
   See also: interval")

;; --
(op "<" (< 1 2) [& nums] 
  "Comparison operator 'less than' on numbers.
   If called with more than two numbers, will expand to a < b & b < c & etc.
   See also: <=, >, >=.")

(op "<=" (<= 1 2) [& nums] 
  "Comparison operator 'less than or equal' on numbers.
   If called with more than two numbers, will expand to a <= b & b <= c & etc.
   See also: <, >, >=.")

(op ">" (> 1 2) [& nums] 
  "Comparison operator 'greater than' on numbers.
   If called with more than two numbers, will expand to a > b & b > c & etc.
   See also: <, <=, >=.")

(op ">=" (>= 1 2) [& nums] 
  "Comparison operator 'greater than or equal' on numbers.
   If called with more than two numbers, will expand to a >= b & b >= c & etc.
   See also: <, <=, >.")

;; --
(op "+" (+ 1 2 3) [& nums] 
  "Arithmetics operator. Addition on numbers." )

(op "-" (- 1 2 3) [& nums] 
  "Arithmetics operator. Subtraction on numbers." )

(op "*" (- 1 2 3) [& nums] 
  "Arithmetics operator. Multiplication on numbers." )

(op "/" (/ 100 5 2) [& nums] 
  "Arithmetics operator. Floored integer division on numbers." )

(op "**" (** 2 2 5) [& nums] 
  "Arithmetics operator. Power operation on numbers. Right-associative.")

(op "mod" (mod 5 2) [& nums] 
  "Arithmetics operator. Modulo operation on numbers.")

;; --
(op "min" (min #{1 2 3}) [set] 
  "Set operator. Returns minimum value of all values stored in the set.
   Has a well-definedness condition: set must be finite and not empty.
   See also: 2nd definition of min, max.")

(op "min" (min 1 2 3) [num & nums] 
  "Set operator. Returns minimum value of all values passed as arguments.
   See also: 1st definition of min, max.")

(op "max" (max #{1 2 3}) [set] 
  "Set operator. Returns maximum value of all values stored in the set.
   Has a well-definedness condition: set must be finite and not empty.
   See also: 2nd definition of max, min.")

(op "min" (max 1 2 3) [num & nums] 
  "Set operator. Returns maximum value of all values passed as arguments.
   See also: 1st definition of max, min.")

;; --
(op "predecessor" (predecessor 5) [num]
  "Number operator. Returns the predecessor of the passed number (i.e., num - 1). 
   See also: dec, successor.")

(op "dec" (dec 5) [num]
  "Number operator. Decrements the passed number. Same as predecessor.
   See also: predecessor, inc.")

(op "successor" (successor 5) [num] 
  "Number operator. Returns the successor of the passed number (i.e., num + 1). 
   See also: inc, predecessor.")

(op "inc" (inc 5) [num] 
  "Number operator. Increments the passed number. Same as successor.
   See also: successor, inc.")


;; --
(op "sigma" (sigma [:x] (member? :x #{1 2 3}) (* :x :x)) [ids pred expr]
  "Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the sum of the results.
   In the example, the values of x are constrained to #{1,2,3}, each value is squared
   and the sum of the squares 1,4,9 = 14 is calculated.
   See also: +, pi, union-pe.")

(op "pi" (pi [:x] (member? :x #{1 2 3}) (* :x :x)) [ids pred expr]
  "Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the product of the results.
   See also: +, sigma, union-pe")
 
;; --

(literal 3.14 "Real number literal.")

(op "real" (real 3) [num]
  "Conversion operator. Takes an integer number and casts it to a real number.
   See also: floor, ceil.")

(constant real-set 
  "Set of all real numbers.
   See also: float-set.") 

#_(constant float-set
  "Set of all (representable) floating point numbers.
   Currently not implemented.
   See also: real-set.")

(op "floor" (floor 3.14) [num]
  "Conversion operator. Takes a real number and calculates the highest integer number
   smaller or equal to it.
   See also: ceil, real.")

(op "ceil" (ceil 3.14) [num]
  "Conversion operator. Takes a real number and calculates the smallest integer number
   greater or equal to it.
   See also: floor, real.")


;; ----


(literal #{1 2 3}
  "Set literal. Note that, contrary to B, you cannot write duplicate values in the set literal.")

(literal #{[:x] | (< 0 :x 10)}
  "Set literal. Order does not matter.
   Important is that it contains the | symbol, a vector of identifiers and a predicate
   constraining the introduced identifiers.
   See also: comprehension-set")

(op "comprehension-set" (comprehension-set [:x] (< 0 :x 10)) [ids pred]
  "Comprehension set (or set comprehension).
   Returns the set of values that are fulfilled by the predicate.
   See also: Set literal.")

(op "comprehension-set" (comprehension-set [:x] (< 0 :x 10) (* :x :x)) [ids pred expr]
  "Event-B style comprehension set (or set comprehension).
   Constrains the identifiers via the predicate and applies the expression to all derived values.")

;; --
(op "pow" (pow #{1 2 3}) [set]
  "Set operator. Calculates the powerset of the given set.
   See also: pow1, fin.")

(op "pow1" (pow1 #{1 2 3}) [set]
  "Set operator. Calculates the powerset of the given set.
   The result does not contain the empty set.
   See also: pow, fin1.")

(op "fin" (fin #{1 2 3}) [set]
  "Set operator. Calculates all *finite* subsets of the powerset of the given set.
   See also: pow, fin1.")

(op "fin1" (fin1 #{1 2 3}) [set]
  "Set operator. Calculates all *finite* subsets of the powerset of the given set.
   The result does not contain the empty set.
   See also: pow1, fin.")

;; --
(op "card" (card #{1 2 3}) [set]
  "Set operator. Calculates the cardinality of the given set.
   Has a well-definedness condition: Set must be finite.")

;; --

(op "union" (union #{1 2} #{3 4}) [& sets] 
  "Set operator. Calculates the union of sets.
   See also: intersection, set-.")

(op "intersection" (intersection #{1 2} #{2 3}) [& sets] 
  "Set operator. Calculates the intersection of sets.
   See also: union, set-.")

(op "set-" (set- #{1 2} #{2 3}) [& sets] 
  "Set operator. Calculates the difference of two sets.
   See also: intersection, set-.")

;; --

(op "member?" (member? 1 #{1 2 3}) [elem set]
  "Set operator. Checks whether the element is included in the set.
   For the non-inclusion operator `/:`, use (not (member? ...)) instead. 
   See also: contains?.")

(op "contains?" (contains? #{1 2 3} 3 4 5) [set & elems]
  "Set operator. Checks the given set includes all listed elements.
   See also: member?.")

;; --

(op "subset?" (subset? #{1} #{1 2} #{1 2 3}) [& sets]
  "Set operator. Checks whether the sets are subsets of (or equal to)
   all the following sets.
   See also: strict-subset?, superset?")

(op "strict-subset?" (strict-subset? #{1 2} #{1 2} #{1 2 3}) [& sets]
  "Set operator. Checks whether the sets are strict subsets of all the following sets.
   See also: subset?, superset?")

(op "superset?" (superset? #{1 2 3} #{1 2} #{1}) [& sets]
  "Set operator. Checks whether the sets are supersets of (or equal to)
   all the following sets.
   See also: strict-subset? superset?")

(op "strict-superset?" (strict-superset? #{1 2 3} #{1 2} #{1}) [& sets]
  "Set operator. Checks whether the sets are strict supersets of all the following sets.
   See also: subset?")

;; --
(op "unite-sets" (unite-sets #{#{2 3} #{1 3}}) [set-of-set]
  "Set operator. Given a set of sets, calculate the union of all contained sets.
   See also: union-pe, intersect-sets")

(op "intersect-sets" (unite-sets #{#{2 3} #{1 3}}) [set-of-set]
  "Set operator. Given a set of sets, calculate the intersection of all contained sets.
   See also: intersection-pe, unite-sets")

(op "union-pe" (union-pe [:x] (subset? :x #{7 8 9}) #{(card :x)}) [ids pred expr]
  "Set operator. Generalized union of sets. \"*union* with *p*redicate and *e*xpression\"
   Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the set union of the results.
   See also: intersection-pe, union, unite-sets, sigma.")

(op "intersection-pe" (intersection-pe [:x] (subset? :x #{7 8 9}) #{(card :x)}) [ids pred expr]
  "Set operator. Generalized intersection of sets. \"*intersection* with *p*redicate and *e*xpression\"
   Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the set intersection of the results.
   See also: union-pe, intersection, intersect-sets, sigma.")

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


(comment (bpropos "add" :short false)
(bpropos "greater"))
