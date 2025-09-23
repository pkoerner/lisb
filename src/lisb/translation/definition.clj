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
   Has a well-definedness condition: Set must be finite.
   See also: size")

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

;; ----

(op "cartesian-product" (cartesian-product #{1 2} #{3 4}) [& sets]
  "Set operator. Calculates the cartesian product of all given sets.")

(op "cart-or-mult" (cart-or-mult #{1 2} #{3 4}) [& all-sets-or-all-nums]
  "DEPRECATED! Do not use this.
   This is an internal node which only exists because the parser does not include
   type checking information. It does not contain information whether it is 
   a cartesian product or a multiplication. Please use instead:
   *, cartesian-product.")
;; -----

(literal [1 -> 2]
  "Tuple literal. Can be chained, for example, as [1 -> 2 -> 3 -> etc].
   Must be a vector, must contain a left element, the arrow symbol `->` and a right element.
   Does not directly create an intermediate representation but a tuple object instead.
   Corresponding IR can be seen when the print is wrapped in
      `(binding [lisb.translation.types/*as-ir* true] ...)`.
   See also: maplet, |->.")

(op "maplet" (maplet 1 2 3) [& elements]
  "Tuple constructor. Creates the (nested) tuple from the passed elements.
   See also: Tuple literal, |->")

(op "|->" (|-> 1 2 3) [& elements]
  "Tuple constructor. Creates the (nested) tuple from the passed elements.
   Same as maplet.
   See also: Tuple literal, maplet")


;; --
(op "<->" (<-> #{1 2} #{2 3}) [& sets]
  "Returns the set of all relations between the given sets.
   See also: relation, <<->, <->>, <<->>")

(op "relation" (relation #{1 2} #{2 3}) [& sets]
  "Returns the set of all relations between the given sets.
   Same as <->.
   See also: <->, total-relation, surjective-relation, total-surjective-relation.")

(op "<<->" (<<-> #{1 2} #{2 3}) [& sets]
  "Returns the set of all total relations between the given sets.
   See also: total-relation, <->, <->>, <<->>")

(op "total-relation" (total-relation #{1 2} #{2 3}) [& sets]
  "Returns the set of all total relations between the given sets.
   Same as <<->.
   See also: <<->, relation, surjective-relation, total-surjective-relation.")

(op "<->>" (<->> #{1 2} #{2 3}) [& sets]
  "Returns the set of all surjective relations between the given sets.
   See also: surjective-relation, <->, <<->, <<->>")

(op "surjective-relation" (surjective-relation #{1 2} #{2 3}) [& sets]
  "Returns the set of all total relations between the given sets.
   Same as <->>.
   See also: <->>, relation, total-relation, total-surjective-relation.")

(op "<<->>" (<<->> #{1 2} #{2 3}) [& sets]
  "Returns the set of all total and surjective relations between the given sets.
   See also: total-surjective-relation, <->, <<->, <->>")

(op "total-surjective-relation" (total-surjective-relation #{1 2} #{2 3}) [& sets]
  "Returns the set of all total and surjective relations between the given sets.
   Same as <<->>.
   See also: <<->>, relation, total-relation, surjective-relation.")

;; ---

(op "dom" (dom #{[1 -> 2], [2 -> 3]}) [relation]
  "Returns the domain of a given relation.
   Note that the operation returns the left sides of the tuples actually contained 
   in the relation, not the domain of the type of the relation.
   See also: ran")

(op "ran" (ran #{[1 -> 2], [2 -> 3]}) [relation]
  "Returns the range of a given relation.
   Note that the operation returns the right sides of the tuples actually contained 
   in the relation, not the domain of the type of the relation.
   See also: dom")

;; --
(op "id" (id #{1 2 3}) [set]
  "Returns the identity relation over a given set, mapping each element to itself.")


;; --
(op "<|" (<| #{1 2 3} #{[0 -> 1] [1 -> 2]}) [set relation]
  "Restricts the domain of a relation to the specified set, i.e., it removes all tuples
   whose left-hand side is not contained in the set.
   See also: domain-restriction, <||, |>, |>>")

(op "domain-restriction" (domain-restriction #{1 2 3} #{[0 -> 1] [1 -> 2]}) [set relation]
  "Restricts the domain of a relation to the specified set, i.e., it removes all tuples
   whose left-hand side is not contained in the set.
   Same as <|.
   See also: <|, domain-subtraction, range-restriction")

(op "<||" (<| #{1 2 3} #{[0 -> 1] [1 -> 2]}) [set relation]
  "Subtracts the specified set from the domain of the relation, i.e., it removes all tuples
   whose left-hand side is contained in the set.
   See also: domain-subtraction, <|, |>, |>>")

(op "domain-subtraction" (domain-subtraction #{1 2 3} #{[0 -> 1] [1 -> 2]}) [set relation]
  "Subtracts the specified set from the domain of the relation, i.e., it removes all tuples
   whose left-hand side is contained in the set.
   Same as <||.
   See also: <|, domain-restriction, range-subtraction")

(op "|>" (|> #{[0 -> 1] [1 -> 2]} #{1 2 3}) [relation set]
  "Restricts the range of a relation to the specified set, i.e., it removes all tuples
   whose right-hand side is not contained in the set.
   See also: range-restriction, <|, <||, <<|, |>>")

(op "range-restriction" (range-restriction #{[0 -> 1] [1 -> 2]} #{1 2 3} ) [relation set]
  "Restricts the domain of a relation to the specified set, i.e., it removes all tuples
   whose left-hand side is not contained in the set.
   Same as |>.
   See also: |>, domain-restriction, range-subtraction")

(op "|>>" (|>> #{[0 -> 1] [1 -> 2]} #{1 2 3}) [relation set]
  "Subtracts the specified set from the range of the relation, i.e., it removes all tuples
   whose right-hand side is contained in the set.
   See also: range-restriction, <|, <||, <<|, |>>")

(op "range-subtraction" (range-restriction #{[0 -> 1] [1 -> 2]} #{1 2 3} ) [relation set]
  "Subtracts the specified set from the range of the relation, i.e., it removes all tuples
   whose right-hand side is contained in the set.
   Same as |>>.
   See also: |>>, domain-subtraction, range-restriction")

;; ---
(op "inverse" (inverse #{[1 -> 2]}) [relation]
  "Returns the inverse relation that swaps the left- and right-hand side of all contained tuples.")

(op "image" (image #{[1 -> 2] [2 -> 3] [3 -> 4]} #{1 2}) [relation set]
  "Returns the relational image, i.e., the set of all right-hand sides contained in the relation,
   whose left-hand side is contained in the passed set.")

(op "<+" (<+ #{[1 -> 2] [1 -> 3] [3 -> 4]} #{[1 -> 1] [2 -> 0]}) [& relations]
  "Relational override. Can be seen as a combination of domain subtraction of the domain of relation2
   from relation1, followed by a union with relation2.")

(op "override" (override #{[1 -> 2] [1 -> 3] [3 -> 4]} #{[1 -> 1] [2 -> 0]}) [& relations]
  "Relational override. Can be seen as a combination of domain subtraction of the domain of relation2
   from relation1, followed by a union with relation2.
   Same as <+.")

(op "><" (>< #{[3 -> 4] [1 -> 2] [2 -> 3]} #{[4 -> 0] [1 -> 0]}) [& relations]
  "Direct product of the relations. If relation1 and relation2 have tuples with the same left-hand side,
   the resulting relation will contain a tuple with this value as its left-hand side and
   both right-hand sides of relation1 and relation2 as its right-hand side.
   Could be written as: {x,y,z | (x,y) : relation1 & (x,z) : relation2} 
   See also: direct-product.")

(op "direct-product" (direct-product #{[3 -> 4] [1 -> 2] [2 -> 3]} #{[4 -> 0] [1 -> 0]}) [& relations]
  "Direct product of the relations. If relation1 and relation2 have tuples with the same left-hand side,
   the resulting relation will contain a tuple with this value as its left-hand side and
   both right-hand sides of relation1 and relation2 as its right-hand side.
   Could be written as: {x,y,z | (x,y) : relation1 & (x,z) : relation2}.
   Same as ><.
   See also: ><.")

(op "composition" (composition #{[1 -> 2] [3 -> 4]} #{[2 -> 6] [4 -> 4]}) [& relations]
  "The composition of two relations. If a right-hand side of relation1 is contained as a left-hand side
   in relation2, replace its with the corresponding right-hand side of relation2.
   Could be written as: {x,z | exists y . (x,y) : relation1 & (y,z) : relation2}.") 

(op "parallel-product" (parallel-product #{[3 -> 4] [1 -> 2]} #{[5 -> 6] [7 -> 8]}) [& relations]
  "Parallel product of two relations. Returns a set of nested tuples [lhs1 -> lhs2 -> [rhs1 -> rhs2]].
   Could be written as: {((x,v), (y,w)) | (x,y) : relation1 & (v,w) : relation2}.")

;; --
(op "prj1" (prj1 nat-set nat-set) [set-domain set-range]
  "Transformation function.
   Returns a projection function, properly typed according to the passed sets.
   If a tuple is passed to this function, will return the left-hand side.")

(op "prj2" (prj2 nat-set nat-set) [set-domain set-range]
  "Transformation function.
   Returns a projection function, properly typed according to the passed sets.
   If a tuple is passed to this function, will return the right-hand side.")

;; --
(op "closure" (closure #{[1 -> 2] [2 -> 3]}) [relation]
  "Transitive and reflexive closure.
   Transitive: If [elem1 -> elem2] and [elem2 -> elem3] is contained in the relation, [elem1 -> elem3]
               will be contained as well. Same with any element that is further 'reachable' from elem3.
   Reflexive: If elem is contained as a left-hand side, [elem -> elem] will be contained in the result.
   See also: closure1, iterate.")

(op "closure1" (closure1 #{[1 -> 2] [2 -> 3]}) [relation]
  "Transitive closure.
   Transitive: If [elem1 -> elem2] and [elem2 -> elem3] is contained in the relation, [elem1 -> elem3]
               will be contained as well. Same with any element that is further 'reachable' from elem3.
   Will only contain [elem -> elem] if elem is reachable from the same node via a non-empty 'path'.
   See also: closure, iterate.")

(op "iterate" (iterate #{[1 -> 2] [2 -> 3]} 2) [relation n]
  "Iteration of the relation.
   Will contain tuples [elem1 -> elem2] with elem2 'reachable' from elem1 within n steps.
   See also: closure, closure1.")

;; --
(op "fnc" (fnc #{[1 -> 2] [1 -> 3]}) [relation]
  "Transformation function. Will transform the relation to a function by collecting all right-hand sides
   of the same left-hand side as a set and create the corresponding mapping #{[lhs1 -> #{rhs1, rhs2}] etc}.
   Inverse function of rel.
   See also: rel.")

(op "rel" (rel #{(maplet 1 #{1 2})}) [function]
  "Transformation function. Will transform a function of the form elem -> set(elems) by 'splicing'
   the set into (multiple) relational mappings [elem -> elem1], [elem -> elem2], etc.
   Inverse function of fnc.
   See also: fnc.")

;; ---

(op "+->" (+-> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial functions from set1 to set2.
   See also: partial-function, -->, +->>, >+>, >+>>.")

(op "partial-function" (partial-function #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial functions from set1 to set2.
   Same as +->.
   See also: +->, total-function, partial-surjection, partial-injection, partial-bijection.")

(op "-->" (--> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total functions from set1 to set2.
   See also: total-function, +->, -->>, >->, >->>.")

(op "total-function" (total-function #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total functions from set1 to set2.
   Same as -->.
   See also: -->, partial-function, total-surjection, total-injection, total-bijection.")

(op "+->>" (+->> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial surjective functions from set1 to set2.
   See also: partial-surjection, -->, +->>, >+>, >+>>.")

(op "partial-surjection" (partial-surjection #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial surjective functions from set1 to set2.
   Same as +->>.
   See also: +->>, total-surjection, partial-function, partial-injection, partial-bijection.")

(op "-->>" (-->> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total surjective functions from set1 to set2.
   See also: total-surjection, -->, +->>, >->, >->>.")

(op "total-surjection" (total-surjection #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total surjective functions from set1 to set2.
   Same as -->>.
   See also: +->>, total-function, partial-surjection, total-injection, total-bijection.")


(op ">+>" (>+> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial injective functions from set1 to set2.
   See also: partial-injection, >->, +->, +->>, >+>>.")

(op "partial-injection" (partial-injection #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial injective functions from set1 to set2.
   Same as >+>.
   See also: +->>, total-injection, partial-function, partial-surjection, partial-bijection.")

(op ">->" (>-> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total injective functions from set1 to set2.
   See also: total-injection, -->, +->>, >->, >->>.")

(op "total-injection" (total-surjection #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total injective functions from set1 to set2.
   Same as >->.
   See also: >->, total-function, partial-injection, total-surjection, total-bijection.")


(op ">+>>" (>+>> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial bijective functions from set1 to set2.
   See also: partial-bijection, >->>, +->, +->>, >+>.")

(op "partial-bijection" (partial-bijection #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial bijective functions from set1 to set2.
   Same as >+>>.
   See also: +->>, total-bijection, partial-function, partial-surjection, partial-injection.")

(op ">->>" (>->> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total bijective functions from set1 to set2.
   See also: total-bijection, -->, >+>>, >->, -->>.")

(op "total-bijection" (total-bijection #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total bijective functions from set1 to set2.
   Same as >->>.
   See also: >->, total-function, partial-bijection, total-surjection, total-injection.")

;; ---

(op "lambda" (lambda [:x] (member? :x #{1 2}) (inc :x)) [ids pred expr]
  "Lambda expression. Creates a function mapping all identifer values constrained by the predicate
   to the given expression (likely depending on the identifier values).")

(op "fn-call" (fn-call #{[1 -> 2]} 1) [fn & elems]
  "Function call. Will apply the function to the specified element.
   Has a well-definedness condition. The element must be in the domain of the function.")
 
(op "sequence" (sequence 42 1337 97) [& elems]
  "Sequence constructor. Will create the set of tuples mapping 1 to the first element,
   2 to the second, etc.
   The is no sequence literal available in lisb.")

(op "seq" (seq #{1337 42}) [set]
  "Set of sequences mapping indices to values from the given set.
   Unlike seq1, will contain the empty sequence. 
   Will contain an infinite number of elements (as there is an infinite number of indices).
   See also: seq1, iseq, perm.")

(op "seq1" (seq1 #{1337 42}) [set]
  "Set of sequences mapping indices to values from the given set.
   Unlike seq, will not contain the empty sequence. 
   Will contain an infinite number of elements (as there is an infinite number of indices).
   See also: seq, iseq1, perm.")

(op "iseq" (iseq #{1337 42}) [set]
  "Set of injective sequences mapping indices to values from the given set.
   Thus, elements from the set will occur at most once on the right-hand side of the tuples.
   Unlike iseq1, will contain the empty sequence. 
   See also: seq, iseq1, perm.")

(op "iseq1" (iseq1 #{1337 42}) [set]
  "Set of injective sequences mapping indices to values from the given set.
   Thus, elements from the set will occur at most once on the right-hand side of the tuples.
   Unlike iseq, will not contain the empty sequence. 
   See also: seq1, iseq, perm.")

(op "perm" (perm #{1337 42}) [set]
  "Set of permutations (bijective sequences) mapping indices to values from the given set.
   Thus, elements from the set will occur exactly once on the right-hand side of the tuples.
   See also: seq, seq1, iseq, iseq1.")

;; --
(op "size" (size (sequence 1 2 3)) [sequence]
  "Returns the size of a given sequence. Must be a sequence, or results might be messed up.
   Must be a finite sequence.
   See also: card")

;; --
(op "concat" (concat (sequence 1 2 3) (sequence 4 5 6)) [sequence1 sequence2] 
  "Concatenates two sequences.
   See also: prepend, append, conc")

(op "prepend" (prepend 42 (sequence 1 2 3)) [elem sequence]
  "Adds the element as a first item to a sequence.
   See also: tail, append, conc, concat")

(op "append" (append (sequence 1 2 3) 42) [sequence elem]
  "Adds the element as a last item to a sequence.
   See also: front, prepend, conc, concat")

(op "conc" (conc (sequence (sequence 1 2) (sequence 3 4) (sequence 4 5 6))) [sequence] 
  "Concatenates a sequence of sequences.
   See also: prepend, append, concat")
;; --

(op "front" (front (sequence 1 2 3)) [sequence]
  "Removes the last element from the sequence.
   See also: last, append, tail, drop-last")

(op "drop-last" (drop-last (sequence 1 2 3)) [sequence]
  "Removes the last element from the sequence.
   Same as front.
   See also: last, append, tail, front")

(op "tail" (tail (sequence 1 2 3)) [sequence]
  "Removes the first element from the sequence.
   See also: first append, front, rest")

(op "rest" (rest (sequence 1 2 3)) [sequence]
  "Removes the first element from the sequence.
   Same as tail
   See also: first, append, front, tail")

(op "first" (first (sequence 1 2 3)) [sequence]
  "Returns the first element of a sequence.
   See also: last, prepend, front")

(op "last" (last (sequence 1 2 3)) [sequence]
  "Returns the last element of a sequence.
   See also: first, append, tail")

;; ----

(op "struct" (struct :foo nat-set :bar nat-set) [& id-type-pairs]
  "Generates the set (type) of all records with the given field names and types.
   See also: record.")

(op "record" (record :foo 42 :bar "a") [& id-value-pairs]
  "Record constructor. Generates a record from the given field names and values.
   See also: struct, record-get")

(op "record-get" ((record :foo 42 :bar "a") :foo) [record id]
  "Look up the value of a specific field in a record.
   See also: record")


])

;; ---



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
(bpropos "greater")
(bpropos "+") 
(bpropos "comprehension"))
