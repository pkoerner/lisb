(ns lisb.translation.definition
  (:require [lisb.translation.lisb2ir :refer [bexpand]])
  (:use lisb.translation.util))

(defmacro op [bop example lisb-sig docstr & {:keys [related inverse alias]}]
  {:type :operator
   :operator `'~(first example)
   :b-operator (name bop)
   :lisb-fn `'~(first (bexpand example))
   :arguments `(quote ~lisb-sig)
   :example `'~example
   :bexample (lisb->b example)
   :doc docstr
   :related (mapv name related)
   :inverse (when inverse (name inverse))
   :alias (when alias (name alias))
   :ir `(b ~example)})

(defmacro literal [example docstr & {:keys [related]}]
  {:type :literal
   :lisb-literal `(quote ~example)
   :b-literal (lisb->b example)
   :related (mapv name related)
   :doc docstr})

(defmacro constant [const docstr & {:keys [related]}]
  {:type :constant
   :lisb-constant `'~const
   :b-literal (lisb->b const)
   :related (mapv name related)
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
   Configurable using ProB's MININT preference."
   :related [max-int])
(constant max-int
  "Largest implementable integer number.
   Configurable using ProB's MAXINT preference."
   :related [min-int])

(constant int-set 
  "Set of all implementable integer numbers ranging from min-int to max-int." 
   :related [integer-set, min-int, max-int]) 

(constant nat-set 
  "Set of all implementable natural numbers between min-int to max-int."
   :related [natural-set, nat1-set, min-int, max-int]) 

(constant nat1-set 
  "Set of all implementable natural numbers between min-int to max-int excluding zero." 
   :related [nat-set natural1-set, min-int, max-int]) 

(constant integer-set 
  "Set of all integer numbers."
  :related [int-set])

(constant natural-set 
  "Set of all natural numbers (including zero)."
   :related [natural1-set nat-set]) 

(constant natural1-set 
  "Set of all natural numbers (excluding zero)." 
   :related [natural-set nat1-set])

;; --
(op "interval" (interval 1 10) [lower upper] 
  "The set of integer numbers ranging from lower (inclusive) to upper (inclusive)."
  :related [range])

(op "range" (range 1 10) [lower upper] 
  "The set of integer numbers ranging from lower (inclusive) to upper (exclusive)."
  :related [interval])

;; --
(op "<" (< 1 2) [& nums] 
  "Comparison operator 'less than' on numbers.
   If called with more than two numbers, will expand to a < b & b < c & etc."
   :related [<= > >= = not=])

(op "<=" (<= 1 2) [& nums] 
  "Comparison operator 'less than or equal' on numbers.
   If called with more than two numbers, will expand to a <= b & b <= c & etc."
   :related [< > >= = not=])

(op ">" (> 1 2) [& nums] 
  "Comparison operator 'greater than' on numbers.
   If called with more than two numbers, will expand to a > b & b > c & etc."
   :related [< <= >= = not=])

(op ">=" (>= 1 2) [& nums] 
  "Comparison operator 'greater than or equal' on numbers.
   If called with more than two numbers, will expand to a >= b & b >= c & etc."
  :related [< <= > = not=])

;; --
(op "+" (+ 1 2 3) [& nums] 
  "Arithmetics operator. Addition on numbers."
  :related [- * / ** mod]
  :inverse -)

(op "-" (- 1 2 3) [& nums] 
  "Arithmetics operator. Subtraction on numbers." 
  :related [+ * / ** mod]
  :inverse +)

(op "*" (- 1 2 3) [& nums] 
  "Arithmetics operator. Multiplication on numbers." 
  :related [+ - / ** mod]
  :inverse /)

(op "/" (/ 100 5 2) [& nums] 
  "Arithmetics operator. Floored integer division on numbers."
  :related [+ - * ** mod]
  :inverse *)

(op "**" (** 2 2 5) [& nums] 
  "Arithmetics operator. Power operation on numbers. Right-associative."
  :related [+ - * / mod])

(op "mod" (mod 5 2) [& nums] 
  "Arithmetics operator. Modulo operation on numbers."
  :related [+ - * / **]) 

;; --
(op "min" (min #{1 2 3}) [[set] [num & nums]] 
  "Set operator with two definitions:
   - If passed a single argument, returns minimum value of all values stored in the set.
     Has a well-definedness condition: set must be finite and not empty.
   - If passed more than one argument: Returns minimum value of all values passed as arguments."
   :related [max])

(op "max" (max #{1 2 3}) [[set] [num & nums]] 
  "Set operator with two definitions:
   - If passed a single argument, returns maximum value of all values stored in the set.
     Has a well-definedness condition: set must be finite and not empty.
   - If passed more than one argument: Returns maximum value of all values passed as arguments."
  :related [min])

;; --
(op "predecessor" (predecessor 5) [num]
  "Number operator. Returns the predecessor of the passed number (i.e., num - 1)."
  :inverse successor
  :alias dec)

(op "successor" (successor 5) [num] 
  "Number operator. Returns the successor of the passed number (i.e., num + 1)."
  :inverse predecessor
  :alias inc)


;; --
(op "sigma" (sigma [:x] (member? :x #{1 2 3}) (* :x :x)) [ids pred expr]
  "Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the sum of the results.
   In the example, the values of x are constrained to #{1,2,3}, each value is squared
   and the sum of the squares 1,4,9 = 14 is calculated."
   :related [+ pi union-pe])

(op "pi" (pi [:x] (member? :x #{1 2 3}) (* :x :x)) [ids pred expr]
  "Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the product of the results."
   :related [+ sigma union-pe]) 
 
;; --

(literal 3.14 "Real number literal.")

(op "real" (real 3) [num]
  "Conversion operator. Takes an integer number and casts it to a real number."
   :related [floor ceil])

(constant real-set 
  "Set of all real numbers."
   :related [float-set]) 

#_(constant float-set
  "Set of all (representable) floating point numbers.
   Currently not implemented."
  :related [real-set])

(op "floor" (floor 3.14) [num]
  "Conversion operator. Takes a real number and calculates the highest integer number
   smaller or equal to it."
  :related [ceil, real])

(op "ceil" (ceil 3.14) [num]
  "Conversion operator. Takes a real number and calculates the smallest integer number
   greater or equal to it."
   :related [floor, real])


;; ----


(literal #{1 2 3}
  "Set literal. Note that, contrary to B, you cannot write duplicate values in the set literal.")

(literal #{[:x] | (< 0 :x 10)}
  "Set literal. Order does not matter.
   Important is that it contains the | symbol, a vector of identifiers and a predicate
   constraining the introduced identifiers."
   :related [comprehension-set])

(op "comprehension-set" (comprehension-set [:x] (< 0 :x 10)) [[ids pred] [ids pred expr]]
  "Comprehension set (or set comprehension) with two definitions:
   - B style comprehension set:
     Returns the set of values that are fulfilled by the predicate. This can be written as a set literal.
   - Event-B style comprehension set (or set comprehension).
     Constrains the identifiers via the predicate and applies the expression to all derived values.")
   
;; --
(op "pow" (pow #{1 2 3}) [set]
  "Set operator. Calculates the powerset of the given set."
  :related [pow1 fin])

(op "pow1" (pow1 #{1 2 3}) [set]
  "Set operator. Calculates the powerset of the given set.
   The result does not contain the empty set."
  :related [pow1 fin1])

(op "fin" (fin #{1 2 3}) [set]
  "Set operator. Calculates all *finite* subsets of the powerset of the given set."
  :related [pow fin])

(op "fin1" (fin1 #{1 2 3}) [set]
  "Set operator. Calculates all *finite* subsets of the powerset of the given set.
   The result does not contain the empty set."
  :related [pow1 fin])

;; --
(op "card" (card #{1 2 3}) [set]
  "Set operator. Calculates the cardinality of the given set.
   Has a well-definedness condition: Set must be finite."
  :related [size])

;; --

(op "union" (union #{1 2} #{3 4}) [& sets] 
  "Set operator. Calculates the union of sets."
  :related [intersection set-])

(op "intersection" (intersection #{1 2} #{2 3}) [& sets] 
  "Set operator. Calculates the intersection of sets."
  :related [union set-])

(op "set-" (set- #{1 2} #{2 3}) [& sets] 
  "Set operator. Calculates the difference of two sets."
  :related [intersection set-])

;; --

(op "member?" (member? 1 #{1 2 3}) [elem set]
  "Set operator. Checks whether the element is included in the set.
   For the non-inclusion operator `/:`, use (not (member? ...)) instead."
  :related [contains?])

(op "contains?" (contains? #{1 2 3} 3 4 5) [set & elems]
  "Set operator. Checks the given set includes all listed elements."
  :related [member?])

;; --

(op "subset?" (subset? #{1} #{1 2} #{1 2 3}) [& sets]
  "Set operator. Checks whether the sets are subsets of (or equal to)
   all the following sets."
  :related [strict-subset? superset?])

(op "strict-subset?" (strict-subset? #{1 2} #{1 2} #{1 2 3}) [& sets]
  "Set operator. Checks whether the sets are strict subsets of all the following sets."
  :related [subset? superset?])
 

(op "superset?" (superset? #{1 2 3} #{1 2} #{1}) [& sets]
  "Set operator. Checks whether the sets are supersets of (or equal to)
   all the following sets."
  :related [strict-superset? subset?])

(op "strict-superset?" (strict-superset? #{1 2 3} #{1 2} #{1}) [& sets]
  "Set operator. Checks whether the sets are strict supersets of all the following sets."
  :related [superset? strict-subset?])

;; --
(op "unite-sets" (unite-sets #{#{2 3} #{1 3}}) [set-of-set]
  "Set operator. Given a set of sets, calculate the union of all contained sets."
  :related [union-pe intersect-sets])

(op "intersect-sets" (unite-sets #{#{2 3} #{1 3}}) [set-of-set]
  "Set operator. Given a set of sets, calculate the intersection of all contained sets."
  :related [intersection-pe unite-sets])

(op "union-pe" (union-pe [:x] (subset? :x #{7 8 9}) #{(card :x)}) [ids pred expr]
  "Set operator. Generalized union of sets. \"*union* with *p*redicate and *e*xpression\"
   Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the set union of the results."
   :related [intersection-pe union unite-sets sigma])

(op "intersection-pe" (intersection-pe [:x] (subset? :x #{7 8 9}) #{(card :x)}) [ids pred expr]
  "Set operator. Generalized intersection of sets. \"*intersection* with *p*redicate and *e*xpression\"
   Calculates all combinations of identifiers that fulfill the constraining predicate.
   Then, map the expression over all those values and return the set intersection of the results.
   See also: union-pe, intersection, intersect-sets, sigma."
   :related [intersection union-pe intersect-sets sigma])

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
      `(binding [lisb.translation.types/*as-ir* true] ...)`."
   :related [maplet])

(op "maplet" (maplet 1 2 3) [& elements]
  "Tuple constructor. Creates the (nested) tuple from the passed elements.
   Could be written as a tuple literal."
   :alias "|->")

;; --
(op "<->" (<-> #{1 2} #{2 3}) [& sets]
  "Returns the set of all relations between the given sets."
   :related [<<-> <->>, <<->>]
   :alias relation)

(op "<<->" (<<-> #{1 2} #{2 3}) [& sets]
  "Returns the set of all total relations between the given sets."
   :alias total-relation
   :related [<->, <->>, <<->>])

(op "<->>" (<->> #{1 2} #{2 3}) [& sets]
  "Returns the set of all surjective relations between the given sets."
   :alias surjective-relation
   :related [<->, <<->, <<->>])

(op "<<->>" (<<->> #{1 2} #{2 3}) [& sets]
  "Returns the set of all total and surjective relations between the given sets."
   :alias total-surjective-relation
   :related [<->, <<->, <->>])

;; ---

(op "dom" (dom #{[1 -> 2], [2 -> 3]}) [relation]
  "Returns the domain of a given relation.
   Note that the operation returns the left sides of the tuples actually contained 
   in the relation, not the domain of the type of the relation."
   :related [ran])

(op "ran" (ran #{[1 -> 2], [2 -> 3]}) [relation]
  "Returns the range of a given relation.
   Note that the operation returns the right sides of the tuples actually contained 
   in the relation, not the domain of the type of the relation."
   :related [dom])

;; --
(op "id" (id #{1 2 3}) [set]
  "Returns the identity relation over a given set, mapping each element to itself."
  :inverse dom)


;; --
(op "<|" (<| #{1 2 3} #{[0 -> 1] [1 -> 2]}) [set relation]
  "Restricts the domain of a relation to the specified set, i.e., it removes all tuples
   whose left-hand side is not contained in the set."
   :alias domain-restriction
   :related [<|| |> |>>])

(op "<||" (<| #{1 2 3} #{[0 -> 1] [1 -> 2]}) [set relation]
  "Subtracts the specified set from the domain of the relation, i.e., it removes all tuples
   whose left-hand side is contained in the set."
   :alias domain-subtraction
   :related [<|, |>, |>>])

(op "|>" (|> #{[0 -> 1] [1 -> 2]} #{1 2 3}) [relation set]
  "Restricts the range of a relation to the specified set, i.e., it removes all tuples
   whose right-hand side is not contained in the set."
   :alias range-restriction
   :related [<|, <||, |>>])

(op "|>>" (|>> #{[0 -> 1] [1 -> 2]} #{1 2 3}) [relation set]
  "Subtracts the specified set from the range of the relation, i.e., it removes all tuples
   whose right-hand side is contained in the set."
   :alias range-restriction
   :related [<| <|| |>])

;; ---
(op "inverse" (inverse #{[1 -> 2]}) [relation]
  "Returns the inverse relation that swaps the left- and right-hand side of all contained tuples."
  :inverse inverse)

(op "image" (image #{[1 -> 2] [2 -> 3] [3 -> 4]} #{1 2}) [relation set]
  "Returns the relational image, i.e., the set of all right-hand sides contained in the relation,
   whose left-hand side is contained in the passed set.")

(op "<+" (<+ #{[1 -> 2] [1 -> 3] [3 -> 4]} #{[1 -> 1] [2 -> 0]}) [& relations]
  "Relational override. Can be seen as a combination of domain subtraction of the domain of relation2
   from relation1, followed by a union with relation2."
  :alias override)

(op "><" (>< #{[3 -> 4] [1 -> 2] [2 -> 3]} #{[4 -> 0] [1 -> 0]}) [& relations]
  "Direct product of the relations. If relation1 and relation2 have tuples with the same left-hand side,
   the resulting relation will contain a tuple with this value as its left-hand side and
   both right-hand sides of relation1 and relation2 as its right-hand side.
   Could be written as: {x,y,z | (x,y) : relation1 & (x,z) : relation2}"
   :alias direct-product)

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
   If a tuple is passed to this function, will return the left-hand side."
   :related [prj2])

(op "prj2" (prj2 nat-set nat-set) [set-domain set-range]
  "Transformation function.
   Returns a projection function, properly typed according to the passed sets.
   If a tuple is passed to this function, will return the right-hand side."
   :related [prj1])

;; --
(op "closure" (closure #{[1 -> 2] [2 -> 3]}) [relation]
  "Transitive and reflexive closure.
   Transitive: If [elem1 -> elem2] and [elem2 -> elem3] is contained in the relation, [elem1 -> elem3]
               will be contained as well. Same with any element that is further 'reachable' from elem3.
   Reflexive: If elem is contained as a left-hand side, [elem -> elem] will be contained in the result."
   :related [closure1 iterate])

(op "closure1" (closure1 #{[1 -> 2] [2 -> 3]}) [relation]
  "Transitive closure.
   Transitive: If [elem1 -> elem2] and [elem2 -> elem3] is contained in the relation, [elem1 -> elem3]
               will be contained as well. Same with any element that is further 'reachable' from elem3.
   Will only contain [elem -> elem] if elem is reachable from the same node via a non-empty 'path'."
   :related [closure iterate])

(op "iterate" (iterate #{[1 -> 2] [2 -> 3]} 2) [relation n]
  "Iteration of the relation.
   Will contain tuples [elem1 -> elem2] with elem2 'reachable' from elem1 within n steps."
   :related [closure closure1])

;; --
(op "fnc" (fnc #{[1 -> 2] [1 -> 3]}) [relation]
  "Transformation function. Will transform the relation to a function by collecting all right-hand sides
   of the same left-hand side as a set and create the corresponding mapping #{[lhs1 -> #{rhs1, rhs2}] etc}."
   :inverse rel)

(op "rel" (rel #{(maplet 1 #{1 2})}) [function]
  "Transformation function. Will transform a function of the form elem -> set(elems) by 'splicing'
   the set into (multiple) relational mappings [elem -> elem1], [elem -> elem2], etc."
   :inverse fnc)

;; ---

(op "+->" (+-> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial functions from set1 to set2."
   :alias partial-function
   :related [--> +->> >+> >+>> fn-call])

(op "-->" (--> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total functions from set1 to set2."
   :alias total-function
   :related [+-> -->> >-> >->> fn-call])

(op "+->>" (+->> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial surjective functions from set1 to set2."
   :alias partial-surjection
   :related [--> +->> >*> >+>> fn-call])

(op "-->>" (-->> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total surjective functions from set1 to set2."
   :alias total-surjection
   :related [--> +->> >-> >->> fn-call])

(op ">+>" (>+> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial injective functions from set1 to set2."
   :alias partial-injection
   :related [>-> +-> +->> >+>> fn-call])

(op ">->" (>-> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total injective functions from set1 to set2.  "
   :alias total-injection
   :related [--> +->> >-> >->> fn-call])

(op ">+>>" (>+>> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all partial bijective functions from set1 to set2."
   :alias partial-bijection
   :related [>->> +-> +->> >+> fn-call])

(op ">->>" (>->> #{1 2 3} #{4 5 6}) [set1 set2]
  "Set of all total bijective functions from set1 to set2."
   :alias total-bijection
   :related [--> >+>> >-> -->> fn-call])

;; ---

(op "lambda" (lambda [:x] (member? :x #{1 2}) (inc :x)) [ids pred expr]
  "Lambda expression. Creates a function mapping all identifer values constrained by the predicate
   to the given expression (likely depending on the identifier values)."
   :related [fn-call])

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
   Will contain an infinite number of elements (as there is an infinite number of indices)."
   :related [seq1 iseq perm])

(op "seq1" (seq1 #{1337 42}) [set]
  "Set of sequences mapping indices to values from the given set.
   Unlike seq, will not contain the empty sequence. 
   Will contain an infinite number of elements (as there is an infinite number of indices)."
   :related [seq iseq1 perm])

(op "iseq" (iseq #{1337 42}) [set]
  "Set of injective sequences mapping indices to values from the given set.
   Thus, elements from the set will occur at most once on the right-hand side of the tuples.
   Unlike iseq1, will contain the empty sequence."
   :related [iseq1 seq perm])

(op "iseq1" (iseq1 #{1337 42}) [set]
  "Set of injective sequences mapping indices to values from the given set.
   Thus, elements from the set will occur at most once on the right-hand side of the tuples.
   Unlike iseq, will not contain the empty sequence. 
   See also: seq1, iseq, perm.")

(op "perm" (perm #{1337 42}) [set]
  "Set of permutations (bijective sequences) mapping indices to values from the given set.
   Thus, elements from the set will occur exactly once on the right-hand side of the tuples."
   :related [seq seq1 iseq iseq1])

;; --
(op "size" (size (sequence 1 2 3)) [sequence]
  "Returns the size of a given sequence. Must be a sequence, or results might be messed up.
   Must be finite and must be a sequence."
   :related [card])

;; --
(op "concat" (concat (sequence 1 2 3) (sequence 4 5 6)) [sequence1 sequence2] 
  "Concatenates two sequences."
   :related [prepend append conc front tail])

(op "prepend" (prepend 42 (sequence 1 2 3)) [elem sequence]
  "Adds the element as a first item to a sequence."
   :inverse tail
   :related [append conc concat])

(op "append" (append (sequence 1 2 3) 42) [sequence elem]
  "Adds the element as a last item to a sequence."
   :inverse front
   :related [prepend conc concat])

(op "conc" (conc (sequence (sequence 1 2) (sequence 3 4) (sequence 4 5 6))) [sequence] 
  "Concatenates a sequence of sequences."
   :related [prepend append concat])
;; --

(op "front" (front (sequence 1 2 3)) [sequence]
  "Removes the last element from the sequence.
   See also: last, append, tail, drop-last"
   :inverse append
   :alias drop-last
   :related [tail])


(op "tail" (tail (sequence 1 2 3)) [sequence]
  "Removes the first element from the sequence.
   See also: first append, front, rest"
   :alias rest
   :inverse prepend
   :related [front])

(op "first" (first (sequence 1 2 3)) [sequence]
  "Returns the first element of a sequence."
   :related [last, prepend, front])

(op "last" (last (sequence 1 2 3)) [sequence]
  "Returns the last element of a sequence."
   :related [first, append, tail])

;; ----

(op "struct" (struct :foo nat-set :bar nat-set) [& id-type-pairs]
  "Generates the set (type) of all records with the given field names and types."
   :related [record])

(op "record" (record :foo 42 :bar "a") [& id-value-pairs]
  "Record constructor. Generates a record from the given field names and values."
   :related [struct, record-get])

(op "record-get" (record-get (record :foo 42 :bar "a") :foo) [record id]
  "Look up the value of a specific field in a record."
   :related [record])


;; ---

(literal "foobar"
  "String literal.")

(op "=" (= 1 1) [& values]
  "Equality predicate. Tests if all passed values are the same."
  :related [not= pred->bool])

(op "not=" (= 1 2) [val1 val2]
  "Non-equality predicate. Tests if the first argument is not equal to the second."
   :related [= distinct?])

(op "distinct?" (distinct? 1 2 3) [& values]
  "Non-equality predicate. Tests if all values are different from each other."
   :related [not=])

(op "not" (not (= 1 2)) [predicate]
  "Negation predicate. Transforms a predicate-true into predicate-false and vice versa."
   :related [implication equivalence and or])

(op "and" (and (= 1 2) (= 3 4)) [predicate1 predicate2]
  "Logical conjunction predicate."
   :related [implication equivalence not or])

(op "or" (or (= 1 2) (= 3 4)) [predicate1 predicate2]
  "Logical disjunction predicate."
   :related [implication equivalence not and])

(op "implication" (implication (= 1 2) (= 3 4)) [predicate1 predicate2]
  "Implication predicate. Checks whether the left predicate implies the right predicate."
   :alias =>
   :related [equivalence not and or]) 

(op "equivalence" (equivalence (= 1 2) (= 3 4)) [predicate1 predicate2]
  "Equivalence predicate. Checks whether the left predicate is equivalent to the right predicate.
   See also: <=>, implication, not, and, or"
   :alias <=>
   :related [implication not and or])

(op "for-all" (for-all [:x] (=> (member? :x nat1-set) (< 0 :x))) [[ids implication] [ids premise conclusion]]
  "Universal quantification with two definitions:
   - If called with two arguments, second must be an implication.
   - If called with three argument, the second is to be expected the premise of the implication,
     and the third to be the conclusion."
  :related [exists])

(op "exists" (exists [:x :y] (< :x 10 :y)) [ids predicate]
  "Existential quantification."
  :related [for-all])

(literal "skip"
  "The empty substitution that does nothing.")

(op "assign" (assign :x 42 :y 1337) [& id-value-pairs]
  "Assignment substitution. Substitutes the specified variables
   with the given values."
   :related [becomes-element-of becomes-such <--])

(op "becomes-element-of" (becomes-element-of [:x :y] #{[1 -> 2]}) [ids set]
  "Assigning substitution. Substitutes the specified variables with any value
   (non-deterministically) drawn from the set.
   If multiple identifiers are given, the set be of the corresponding tuple type."
   :related [assign becomes-such <--])

(op "becomes-such" (becomes-such [:x :y] (= :x (inc :y))) [ids predicate]
  "Assigning substitution. Substitutes the specified variables (non-deterministically)
   with any value(s) that fulfill the given predicate."
   :related [assign becomes-element-of <--])

(op "<--" (<-- [:x :y] (op-call :foo :a 42)) [ids operation-call]
  "Assigning substitution. Substitutes the specified variables (non-deterministically)
   with any value(s) that are returned by the specified operation call.
   See also: assign, becomes-element-of, becomes-such"
   :related [assign becomes-element-of <--] )

(op "parallel-sub" (parallel-sub (assign :x 42) (assign :y 43) (assign :z 1337)) [& substitutions]
  "Composed substitutions. Executes all given substitutions simultaneously."
   :alias ||
   :related [sequential-sub])

(op "sequential-sub" (sequential-sub (assign :x 42) (assign :y 43) (assign :z 1337)) [& substitutions]
  "Composed substitutions. Executes all given substitutions, one after the other,
   from left to right."
   :related [parallel-sub])

(op "any" (any [:x :y] (< min-int :x :y max-int) (assign :a :x :b :y)) [ids predicate & substitutions]
  "Guarded substitution. (Non-deterministically) chooses values for the identifiers that fulfill
   the predicate, which can be used in the following substitutions."
   :related [choice])

(op "let" (let-sub [:x 42 :y 1337] (assign :a :x :y 1337)) [id-value-pairs & substitutions]
  "Let-binding for (fresh) identifiers which can be used in the following substitutions."
   :related [var])

(op "var" (var-sub [:x] (assign :x 42) (assign :a :x)) [ids & substitutions]
  "Introduces a local variable that can be assigned arbitrarily."
   :related [let])

(op "pre" (pre (= :x 42) (assign :y 42)) [predicate & substitutions]
  "Substitutions with precondition. The caller of the substitution is responsible
   to ensure that the precondition holds."
   :related [assert])

(op "assert" (assert (= :x 42) (assign :y 42)) [predicate & substitutions]
  "Substitutions with assertion."
   :related [pre])

(op "choice" (choice (assign :x 42) (assign :y 42)) [& substitutions]
  "Non-deterministic choice between different substitutions (behaviours)."
  :related [any])

(op "if-sub" (if-sub (= :x 0) (assign :x 100) (assign :x (dec :x))) [predicate substitution-then substitution-else?]
  "Conditional substitution that only executes its then-branch, if the predicate is fulfilled.
   Executes the else-branch if specified."
   :related [cond select case])

(op "cond" (cond (= :x 0) (assign :x 1) (= :y 0) (assign :y 1)) [& predicate-substitution-pairs else-substitution?]
  "Similar to if, but allows to specifiy multiple cases that are tested one after each other.
   Will expand to if / else-if / else-if ... / else."
   :related [if-sub select case])

(op "select" (select (= :x 0) (assign :x 1) (= :y 0) (assign :y 1) (assign :v "default")) [& predicate-substitution-pairs default-substitution?]
  "Non-deterministic guarded substitutions. Can choose from any substitution whose matching predicate 
   evaluates to true. If a default substitutiuon is given and all predicates evaluate to false,
   the default substitution is executed."
   :related [if-sub cond case])

(op "case" (case :x 0 (assign :x 1) 1 (assign :y 1) (assign :v "default")) [variable & constant-substitution-pairs default-substitution?]
  "Tests the value of a given variable to be one of the list of specified constant literals.
   Variable expression and constants must be a simple type (integer, boolean, deferred or enumerated type).
   If no value is matched, optional default-case is applied."
   :related [if-sub cond select])

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
(bpropos "greater")
(bpropos "+") 
(bpropos "comprehension"))
