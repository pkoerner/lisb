(ns lisb.lisb2ast-test
  (:require [clojure.test :refer :all]
            [lisb.representation :refer :all]
            [lisb.examples.simple :as simple]
            [lisb.translation :refer [node-repr->ast node-repr->predicate-ast node-repr->expression-ast node-repr->substitution-ast node-repr->machine-clause-ast]]))

(import de.be4.classicalb.core.parser.visualisation.ASTPrinter)
(def printer (ASTPrinter.))
(defn print-ast [b-ast]
  (.apply b-ast printer))

(import de.be4.classicalb.core.parser.util.PrettyPrinter)
(defn get-machine-from-ast [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (.getPrettyPrint pprinter)))

#_(deftest test2
    (testing "test2"
      (is (= (bfor-all #{:x} (b= 1 1) (b= 2 2)) (macroexpand-1 `(b lift))))))

(deftest test2
  (testing "test2"
    (is ( = "" (eval `(b simple/lift))))))

(defn normalize-string [string]
  (clojure.string/replace string #"[ \n\r\t]" ""))

(deftest examples-simple-test
  (testing "examples-simple"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/" b)))) (normalize-string (get-machine-from-ast (node-repr->ast lisb))))
                  "Lift.mch" simple/lift
                  "ACounter.mch" simple/a-counter
                  ;"GCD.mch" simple/gcd
                  "KnightsKnaves.mch" simple/knights-knaves
                  "Bakery0.mch" simple/bakery0
                  "Bakery1.mch" simple/bakery1
                  )))

(deftest machine-test
  (testing "machine"
    (is (= "MACHINE Empty\nEND"
           (get-machine-from-ast (node-repr->ast
                                   (b (machine
                                        (machine-variant)
                                        (machine-header :Empty ())))))))))

(deftest machine-clauses-test
  (testing "machine-clauses"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->machine-clause-ast lisb)))
                  "1=1" (b (constraints (= 1 1))))))

(deftest substitutions-test
  (testing "substitutions"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->substitution-ast lisb)))
                  "skip" (b skip)
                  "x := E" (b (assign :x :E))
                  ;"f(x) := E"
                  "x::S" (b (becomes-element-of #{:x} :S))
                  "x :(x>0) " (b (becomes-such #{:x} (> :x 0)))
                  "x<--OP(y)" (b (operation-call #{:x} :OP #{:y}))
                  "skip || skip" (b (parallel-substitution skip skip))
                  "skip || skip || skip" (b (parallel-substitution skip skip skip))
                  "skip ; skip" (b (sequence-substitution skip skip))
                  "skip ; skip ; skip" (b (sequence-substitution skip skip skip))
                  "ANY x WHERE x>0 THEN skip END " (b (any #{:x} (> :x 0) skip))
                  "LET x BE x=1 IN skip END " (b (let-sub #{:x} (= :x 1) skip))
                  "VAR x IN skip END " (b (bvar #{:x} skip))
                  "PRE 1=2 THEN skip END " (b (pre (= 1 2) skip))
                  "ASSERT 1=2 THEN skip END " (b (assert (= 1 2) skip))
                  "CHOICE skip OR skip END " (b (choice skip skip))
                  "IF 1=2 THEN skip END " (b (if-sub (= 1 2) skip))
                  "IF 1=2 THEN skip ELSE skip END " (b (if-sub (= 1 2) skip skip))
                  "IF 1=2 THEN skip ELSIF 1=3 THEN skip END " (b (cond (= 1 2) skip (= 1 3) skip))
                  "IF 1=2 THEN skip ELSIF 1=3 THEN skip ELSE skip END " (b (cond (= 1 2) skip (= 1 3) skip skip))
                  "SELECT 1=2 THEN skip END " (b (select (= 1 2) skip))
                  "SELECT 1=2 THEN skip ELSE x := 1 END " (b (select (= 1 2) skip (assign :x 1)))
                  "SELECT 1=1 THEN skip WHEN 2=2 THEN skip END " (b (select (= 1 1) skip (= 2 2) skip))
                  "SELECT 1=1 THEN skip WHEN 2=2 THEN skip ELSE skip END " (b (select (= 1 1) skip (= 2 2) skip skip))
                  ;"CASE E OF EITHER m THEN G OR n THEN H END END"
                  ;"CASE E OF EITHER m THEN G OR n THEN H ELSE I END END"
                  )))

(deftest if-test
  (testing "if"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                  "IF 1=1 THEN 2 ELSE 3 END" (b (if-expr (= 1 1) 2 3)))
    (are [b lisb] (= b (get-machine-from-ast (node-repr->predicate-ast lisb)))
                  "(1=1 => 2=2) & (not(1=1) => 3=3)" (b (and (=> (= 1 1) (= 2 2)) (=> (not (= 1 1)) (= 3 3)))))))

#_(deftest let-test
  (testing "let"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "LET x, y BE x=1 & y=2 IN 3 END" '(let-expr #{:x :y} (and (= :x 1) (= :y 2)) 3) )
    (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                  "LET x, y BE x=1 & y=2 IN 0=0 END" '(let-pred #{:x :y} (and (= :x 1) (= :y 2)) (= 0 0)) )))


(deftest strings-test
  (testing "strings"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                  "\"astring\"" (b "astring")
                  "STRING" (b string-set)
                  "size(\"s\")" (b (count-seq "s"))
                  "\"s\"~" (b (reverse "s"))
                  "\"s\"^\"t\"" (b (concat "s" "t"))
                  ;"conc(['''s''', '''t'''])" '(conc (sequence "s" "t"))
                  )))

(deftest sequences-test
  (testing "sequences"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                  ;"<>" '(sequence)
                  ;"[]" '(sequence)
                  ;"[E]" '(sequence :E)
                  ;"[E,F]" '(sequence :E :F)
                  "seq(S)" (b (seq :S))
                  "seq1(S)" (b (seq1 :S))
                  "iseq(S)" (b (iseq :S))
                  "iseq1(S)" (b (iseq1 :S))
                  "perm(S)" (b (perm :S))
                  "size(S)" (b (count-seq :S))
                  "s^t" (b (concat :s :t))
                  "E->s" (b (-> :E :s))
                  "s<-E" (b (<- :s :E))
                  "S~" (b (reverse :S))
                  "first(S)" (b (first :S))
                  "last(S)" (b (last :S))
                  "front(S)" (b (front :S))
                  "tail(S)" (b (tail :S))
                  "conc(S)" (b (conc :S))
                  "s/|\\n" (b (restrict-front :s :n))
                  "s\\|/n" (b (restrict-tail :s :n)))))

(deftest function-test
  (testing "functions"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                  "S+->T" (b (+-> :S :T))
                  "S-->T" (b (--> :S :T))
                  "S+->>T" (b (+->> :S :T))
                  "S-->>T" (b (-->> :S :T))
                  "S>+>T" (b (>+> :S :T))
                  "S>->T" (b (>-> :S :T))
                  "S>->>T" (b (>->> :S :T))
                  "%x.(1=1|1)" (b (lambda #{:x} (= 1 1) 1))
                  "f(E)" (b (call :f :E))
                  ;"f(E,F)" '(:f :E :F)
                  )))

(deftest relation-test
  (testing "relations"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                  "S<->T" (b (<-> :S :T))
                  "S<<->T" (b (total-relation :S :T))
                  "S<->>T" (b (surjective-relation :S :T))
                  "S<<->>T" (b (total-surjective-relation :S :T))
                  "(E,F)" (b (couple :E :F))
                  "dom(r)" (b (dom :r))
                  "ran(r)" (b (ran :r))
                  "id(S)" (b (identity :S))
                  "S<|r" (b (<| :S :r))
                  "S<<|r" (b (<<| :S :r))
                  "r|>S" (b (|> :r :S))
                  "r|>>S" (b (|>> :r :S))
                  "r~" (b (inverse :r))
                  "r[S]" (b (image :r :S))
                  "r1<+r2" (b (<+ :r1 :r2))
                  "r1><r2" (b (>< :r1 :r2))
                  "(r1;r2)" (b (comp :r1 :r2))
                  "(r1||r2)" (b (|| :r1 :r2))
                  "prj1(S,T)" (b (prj1 :S :T))
                  "prj2(S,T)" (b (prj2 :S :T))
                  "closure1(r)" (b (closure1 :r))
                  "closure(r)" (b (closure :r))
                  "iterate(r,n)" (b (iterate :r :n))
                  "fnc(r)" (b (fnc :r))
                  "rel(r)" (b (rel :r)))))

(deftest numbers-test
  (testing "numbers"
    (testing "expressions"
      (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                    "1" (b 1)
                    "-1" (b -1)
                    "-x" (b (- :x))
                    "INTEGER" (b integer-set)
                    "NATURAL" (b natural-set)
                    "NATURAL1" (b natural1-set)
                    "INT" (b int-set)
                    "NAT" (b nat-set)
                    "NAT1" (b nat1-set)
                    ;"1..2" '(range 1 3)
                    "MININT" (b min-int)
                    "MAXINT" (b max-int)
                    "max(NAT)" (b (max nat-set))
                    "min(NAT)" (b (min nat-set))
                    "PI(z).(z:NAT|1)" (b (pi #{:z} (member :z nat-set) 1))
                    "SIGMA(z).(z:NAT|1)" (b (sigma #{:z} (member :z nat-set) 1))
                    )
      (testing "arithmetic"
        (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                      "1+2" (b (+ 1 2))
                      "1+2+3" (b (+ 1 2 3))
                      "1-2" (b (- 1 2))
                      "1-2-3" (b (- 1 2 3))
                      "1*2" (b (* 1 2))
                      "1*2*3" (b (* 1 2 3))
                      "1/2" (b (/ 1 2))
                      "1/2/3" (b (/ 1 2 3))
                      "1**2" (b (** 1 2))
                      "1 mod 2" (b (mod 1 2))
                      "1 mod 2 mod 3" (b (mod 1 2 3))
                      "succ(1)" (b (inc 1))
                      "pred(1)" (b (dec 1))
                      )))
    (testing "predicates"
      (are [b lisb] (= b (get-machine-from-ast (node-repr->predicate-ast lisb)))
                    "1>2" (b (> 1 2))
                    "1<2" (b (< 1 2))
                    "1>=2" (b (>= 1 2))
                    "1<=2" (b (<= 1 2))))))

(deftest sets-test
  (testing "sets"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                  "{}" (b #{})
                  "{E}" (b #{:E})
                  "{F,E}" (b #{:E :F})
                  "{x|x:NAT}" (b (comp-set #{:x} (member :x nat-set)))
                  "POW({})" (b (pow #{}))
                  "POW1({})" (b (pow1 #{}))
                  "FIN({})" (b (fin #{}))
                  "FIN1({})" (b (fin1 #{}))
                  "card({})" (b (count #{}))
                  "{E}*{F}" (b (* #{:E} #{:F}))
                  "{E}*{F}*{G}" (b (* #{:E} #{:F} #{:G}))
                  "{E}\\/{F}" (b (union #{:E} #{:F}))
                  "{E}\\/{F}\\/{G}" (b (union #{:E} #{:F} #{:G}))
                  "{E}/\\{F}" (b (intersection #{:E} #{:F}))
                  "{E}/\\{F}/\\{G}" (b (intersection #{:E} #{:F} #{:G}))
                  "{E}-{F}" (b (- #{:E} #{:F}))
                  "{E}-{F}-{G}" (b (- #{:E} #{:F} #{:G}))
                  "union({{}})" (b (unite-sets #{#{}}))
                  "inter({{}})" (b (intersect-sets #{#{}}))
                  "UNION(z).(z:NAT|1)" (b (union-pe #{:z} (member :z nat-set) 1))
                  "INTER(z).(z:NAT|1)" (b (intersection-pe #{:z} (member :z nat-set) 1)))
    (are [b lisb] (= b (get-machine-from-ast (node-repr->predicate-ast lisb)))
                  "1:{}" (b (member 1 #{}))
                  "1/:{}" (b (not-member 1 #{}))
                  "{E}<:{G}" (b (subset #{:E} #{:G}))
                  "{E}/<:{G}" (b (not-subset #{:E} #{:G}))
                  "{E}<<:{G}" (b (subset-strict #{:E} #{:G}))
                  "{E}/<<:{G}" (b (not-subset-strict #{:E} #{:G})))))

(deftest booleans-test
  (testing "booleans"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->expression-ast lisb)))
                  "TRUE" (b true)
                  "FALSE" (b false)
                  "BOOL" (b bool-set)
                  "bool(TRUE=TRUE)" (b (pred->bool (= true true))))))

(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->predicate-ast lisb)))
                  "TRUE=FALSE" (b (= true false))
                  "TRUE/=FALSE" (b (not= true false)))))

(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [b lisb] (= b (get-machine-from-ast (node-repr->predicate-ast lisb)))
                  "1=1 & 2=2" (b (and (= 1 1) (= 2 2)))
                  "1=1 & 2=2 & 3=3" (b (and (= 1 1) (= 2 2) (= 3 3)))
                  "1=1 or 2=2" (b (or (= 1 1) (= 2 2)))
                  "1=1 or 2=2 or 3=3" (b (or (= 1 1) (= 2 2) (= 3 3)))
                  "1=1 => 2=2" (b (=> (= 1 1) (= 2 2)))
                  "1=1 => 2=2 => 3=3" (b (=> (= 1 1) (= 2 2) (= 3 3)))
                  "1=1 <=> 2=2" (b (<=> (= 1 1) (= 2 2)))
                  "1=1 <=> 2=2 <=> 3=3" (b (<=> (= 1 1) (= 2 2) (= 3 3)))
                  "not(1=1)" (b (not (= 1 1)))
                  "not(1=1)" (b (not (= 1 1)))
                  "!x.(1=1 => 2=2)" (b (for-all #{:x} (= 1 1) (= 2 2)))
                  "!x.(x:NAT => 0<x)" (b (for-all #{:x} (member :x nat-set) (< 0 :x)))
                  "#x.(1=1 & 2=2)" (b (exists [:x] (and (= 1 1) (= 2 2)))))))

(def func '(:f :E :F))

(defmacro transform-function-calls1 [lisb]
  `(if (keyword? (first ~lisb))
    (conj ~lisb 'call)
    ~lisb))

(defmacro transform-function-calls [lisb]
  (if (keyword? (first lisb))
    (conj lisb (partial bcall))
    2))

(defmacro transform-function-calls2 [lisb]
  (if (list? lisb)
    (reduce
      (fn [acc param]
        (concat acc (transform-function-calls2 param)))
      (let [f (first lisb)]
        (if (keyword? f)
          (list bcall f)
          (list f)))
      (rest lisb))
    lisb))

(defn tf [lisb]
  (println lisb)
  (if (seqable? lisb)
    (let [params (map tf (rest lisb))]
      (conj params (first lisb)))
    lisb))

(defmacro transform-function-calls3 [lisb]
  (b (tf lisb)))

#_(deftest test1
  (testing "test1"
    (is (= "(call :f :E :F)" (transform-function-calls3 (or (= 1 1) (:f :E :F)))))
    #_(is (= "(call :f :E :F)" (apply (partial transform-function-calls) func)))
    #_(is (= "f(E,F)" (lisb->node-repr  '(:f :E :F))))))
