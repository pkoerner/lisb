(ns lisb.ast2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.ast2lisb :refer :all]
            [lisb.examples.simple :as simple])
  (:import (de.be4.classicalb.core.parser BParser)))

(import de.be4.classicalb.core.parser.visualisation.ASTPrinter)
(def printer (ASTPrinter.))
(defn print-ast [b-ast]
  (.apply b-ast printer))

(deftest examples-simple-test
  (testing "examples-simple"
    (are [lisb b] (= lisb (b->lisb (slurp (clojure.java.io/resource (str "machines/" b)))))
                  simple/lift "Lift.mch"
                  simple/a-counter "ACounter.mch"
                  simple/gcd "GCD.mch"
                  simple/knights-knaves "KnightsKnaves.mch"
                  simple/bakery0 "Bakery0.mch"
                  simple/bakery1 "Bakery1.mch")))


;;; parse units

(deftest machine-test
  (testing "machine"
    (is (=
          '(machine
            (machine-variant)
            (machine-header :Empty ()))
          (b->lisb "MACHINE Empty\nEND")))))

;;; machine clauses

#_(deftest constraint-test
  (testing "constraint"
    (is (=
          (b->lisb (slurp (clojure.java.io/resource "machines/Constraint.mch")))))))

(deftest machine-clauses-test
  (testing "machine-clauses"
    (are [lisb b] (= lisb
                     (b->lisb (slurp (clojure.java.io/resource (str "machines/" b)))))
                  '(machine
                     (machine-variant)
                     (machine-header :Set ())
                     (sets (deferred-set :S) (enumerated-set :T #{:E :F}))) "Set.mch"
                  '(machine
                    (machine-variant)
                    (machine-header :Constant ())
                    (constants :con)
                    (properties (= :con 1))) "Constant.mch"
                  '(machine
                    (machine-variant)
                    (machine-header :Variable ())
                    (variables :nat)
                    (invariants (member :nat nat-set))
                    (init (assign :nat 0))) "Variable.mch")))


(deftest substitutions-test
  (testing "substitutions"
    (are [lisb b] (= lisb (b-substitution->lisb b))
                  'skip "skip"
                  '(assign :x :E) "x := E"
                  ; "f(x) := E"
                  '(becomes-element-of (:x) :S) "x :: S"
                  '(becomes-such (:x) (> :x 0))  "x : (x>0)"
                  '(operation-call (:x) :OP (:y)) "x <-- OP(y)"
                  '(parallel-substitution skip skip) "skip||skip"
                  '(parallel-substitution skip skip skip) "skip||skip||skip"
                  '(sequence-substitution skip skip) "skip;skip"
                  '(sequence-substitution skip skip skip) "skip;skip;skip"
                  '(any (:x) (> :x 0) skip) "ANY x WHERE (x>0) THEN skip END"
                  '(let-sub (:x) (= :x 1) skip) "LET x BE x=1 IN skip END"
                  '(var (:x) skip) "VAR x IN skip END"
                  '(pre (= 1 2) skip) "PRE 1=2 THEN skip END"
                  '(assert (= 1 2) skip) "ASSERT 1=2 THEN skip END"
                  '(choice skip skip) "CHOICE skip OR skip END"
                  '(if-sub (= 1 2) skip) "IF 1=2 THEN skip END"
                  '(if-sub (= 1 2) skip skip) "IF 1=2 THEN skip ELSE skip END"
                  '(cond-sub (= 1 2) skip (= 1 3) skip) "IF 1=2 THEN skip ELSIF 1=3 THEN skip END"
                  '(cond-sub (= 1 2) skip (= 1 3) skip :else skip) "IF 1=2 THEN skip ELSIF 1=3 THEN skip ELSE skip END"
                  '(select (= 1 2) skip) "SELECT 1=2 THEN skip END"
                  '(select (= 1 2) skip :else (assign :x 1)) "SELECT 1=2 THEN skip ELSE x:= 1 END"
                  ;"SELECT 1=1 THEN skip WHEN 2=2 THEN skip END"
                  ;"SELECT 1=1 THEN G WHEN Q THEN H ELSE I END"
                  ;"CASE E OF EITHER m THEN G OR n THEN H END END"
                  ;"CASE E OF EITHER m THEN G OR n THEN H ELSE I END END"
                  )))


(deftest if-test
  (testing "if"
    (are [lisb b] (= lisb (b-expression->lisb b))
                             '(if-expr (= 1 1) 2 3) "IF 1=1 THEN 2 ELSE 3 END")
    (are [lisb b] (= lisb (b-predicate->lisb b))
                  '(and (=> (= 1 1) (= 2 2)) (=> (not (= 1 1)) (= 3 3))) "IF 1=1 THEN 2=2 ELSE 3=3 END")))

(deftest let-test
  (testing "let"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  '(let-expr (and (= :x 1) (= :y 2)) 3) "LET x, y BE x=1 & y=2 IN 3 END")
    (are [lisb b] (= lisb (b-predicate->lisb b))
                  '(let-pred (and (= :x 1) (= :y 2)) (= 0 0)) "LET x, y BE x=1 & y=2 IN 0=0 END")))


#_(deftest tree-test
    (testing "trees"
      (is (= (b-formula->lisb "tree(NAT)")))
      (is (= (b-formula->lisb "btree(NAT)")))
      (is (= (b-formula->lisb "top(t)")))
      (is (= (b-formula->lisb "const(E,s)")))
      (is (= (b-formula->lisb "rank(t,n)")))
      (is (= (b-formula->lisb "father(t,n)")))
      (is (= (b-formula->lisb "son(t,n,i)")))
      (is (= (b-formula->lisb "sons(t)")))
      (is (= (b-formula->lisb "subtree(t,n)")))
      (is (= (b-formula->lisb "arity(t,n)")))
      (is (= (b-formula->lisb "bin(E)")))
      (is (= (b-formula->lisb "bin(tl,E,tr)")))
      (is (= (b-formula->lisb "left(t)")))
      (is (= (b-formula->lisb "right(t)")))
      (is (= (b-formula->lisb "sizet(t)")))
      (is (= (b-formula->lisb "prefix(t)")))
      (is (= (b-formula->lisb "postfix(t)")))))


#_(deftest reals-test
    (testing "reals"
      (is (= (b-formula->lisb "REAL")))
      (is (= (b-formula->lisb "FLOAT")))
      (is (= (b-formula->lisb "1.2")))
      (is (= (b-formula->lisb "real(1)")))
      (is (= (b-formula->lisb "floor(1.2)")))
      (is (= (b-formula->lisb "ceiling(1.2)")))))


(deftest strings-test
  (testing "strings"
    (are [lisb b] (= lisb (b-formula->lisb b))
                  "astring" "\"astring\""
                  "astring" "'''astring'''"
                  '(string-set) "STRING"
                  '(count-seq "s") "size('''s''')"
                  '(reverse "s") "rev('''s''')"
                  '(concat "s" "t") "'''s'''^'''t'''"
                  '(conc (sequence "s" "t")) "conc(['''s''', '''t'''])")))


#_(deftest records-test
  (testing "records"
    #_(is (= (bstruct [:n (bnat-set)]) (b-expression->lisb "struct(n:NAT")))
    #_(is (= (b-expression->lisb "struct(n:NAT,b:BOOL")))
    #_(is (= (b-expression->lisb "rec(n:1")))
    #_(is (= (b-expression->lisb "rec(n:1,b:TRUE")))
    #_(is (= (b-expression->lisb "E'n")))))


(deftest sequences-test
  (testing "sequences"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  '(sequence) "<>"
                  '(sequence) "[]"
                  '(sequence :E) "[E]"
                  '(sequence :E :F) "[E,F]"
                  '(seq :S) "seq(S)"
                  '(seq1 :S) "seq1(S)"
                  '(iseq :S) "iseq(S)"
                  '(iseq1 :S) "iseq1(S)"
                  '(perm :S) "perm(S)"
                  '(count-seq :S) "size(S)"
                  '(concat :s :t) "s^t"
                  '(-> :E :s) "E->s"
                  '(<- :s :E) "s<-E"
                  '(reverse :S) "rev(S)"
                  '(first :S) "first(S)"
                  '(last :S) "last(S)"
                  '(front :S) "front(S)"
                  '(tail :S) "tail(S)"
                  '(conc :S) "conc(S)"
                  '(restrict-front :s :n) "s/|\\n"
                  '(restrict-tail :s :n) "s\\|/n")))


(deftest function-test
  (testing "functions"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  '(+-> :S :T) "S+->T"
                  '(--> :S :T) "S-->T"
                  '(+->> :S :T) "S+->>T"
                  '(-->> :S :T) "S-->>T"
                  '(>+> :S :T)  "S>+>T"
                  '(>-> :S :T) "S>->T"
                  '(>->> :S :T) "S>->>T"
                  '(lambda (:x) (= 1 1) 1) "%x.(1=1|1)"
                  '(:f :E) "f(E)"
                  '(:f :E :F) "f(E,F)")))


(deftest relation-test
  (testing "relations"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  '(<-> :S :T) "S<->T"
                  '(total-relation :S :T) "S<<->T"
                  '(surjective-relation :S :T) "S<->>T"
                  '(total-surjective-relation :S :T) "S<<->>T"
                  '(couple :E :F) "E|->F"
                  '(dom :r) "dom(r)"
                  '(ran :r) "ran(r)"
                  '(identity :S) "id(S)"
                  '(<| :S :r) "S<|r"
                  '(<<| :S :r) "S<<|r"
                  '(|> :r :S)"r|>S"
                  '(|>> :r :S) "r|>>S"
                  '(inverse :r) "r~"
                  '(image :r :S) "r[S]"
                  '(<+ :r1 :r2) "r1<+r2"
                  '(>< :r1 :r2) "r1><r2"
                  '(comp :r1 :r2) "(r1;r2)"
                  '(|| :r1 :r2) "(r1||r2)"
                  '(prj1 :S :T) "prj1(S,T)"
                  '(prj2 :S :T) "prj2(S,T)"
                  '(closure1 :r) "closure1(r)"
                  '(closure :r) "closure(r)"
                  '(iterate :r :n) "iterate(r,n)"
                  '(fnc :r) "fnc(r)"
                  '(rel :r) "rel(r)")))


(deftest number-test
  (testing "numbers"
    (testing "expressions"
      (are [lisb b] (= lisb (b-expression->lisb b))
                    1 "1"
                    -1 "-1"
                    '(- :x) "-x"
                    15 "0xF"
                    'integer-set "INTEGER"
                    'natural-set "NATURAL"
                    'natural1-set "NATURAL1"
                    'int-set "INT"
                    'nat-set "NAT"
                    'nat1-set "NAT1"
                    '(range 1 3) "1..2"
                    'min-int "MININT"
                    'max-int "MAXINT"
                    '(max nat-set) "max(NAT)"
                    '(min nat-set) "min(NAT)"
                    '(pi (:z) (member :z nat-set) 1) "PI(z).(z:NAT|1)"
                    '(sigma (:z) (member :z nat-set) 1) "SIGMA(z).(z:NAT|1)"))
    (testing "arithmetic"
      (are [lisb b] (= lisb (b-expression->lisb b))
                    '(+ 1 2) "1+2"
                    '(+ 1 2 3) "1+2+3"
                    '(- 1 2) "1-2"
                    '(- 1 2 3) "1-2-3"
                    '(* 1 2) "1*2"
                    '(* 1 2 3) "1*2*3"
                    '(/ 1 2) "1/2"
                    '(/ 1 2 3) "1/2/3"
                    '(** 1 2) "1**2"
                    '(mod 1 2) "1 mod 2"
                    '(mod 1 2 3) "1 mod 2 mod 3"
                    '(inc 1) "succ(1)"
                    '(dec 1) "pred(1)"))
    (testing "predicates"
      (are [lisb b] (= lisb (b-predicate->lisb b))
                    '(> 1 2) "1>2"
                    '(< 1 2) "1<2"
                    '(>= 1 2) "1>=2"
                    '(<= 1 2) "1<=2"))))


(deftest sets-test
  (testing "sets"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  #{} "{}"
                  #{:E} "{E}"
                  #{:E :F} "{E, F}"
                  '(comp-set (:x) (member :x nat-set)) "{x|x:NAT}"
                  '(pow #{}) "POW({})"
                  '(pow1 #{}) "POW1({})"
                  '(fin #{}) "FIN({})"
                  '(fin1 #{}) "FIN1({})"
                  '(count #{}) "card({})"
                  '(* #{:E} #{:F}) "{E}*{F}"
                  '(* #{:E} #{:F} #{:G})"{E}*{F}*{G}"
                  '(union #{:E} #{:F})"{E}\\/{F}"
                  '(union #{:E} #{:F} #{:G})"{E}\\/{F}\\/{G}"
                  '(intersection #{:E} #{:F})"{E}/\\{F}"
                  '(intersection #{:E} #{:F} #{:G})"{E}/\\{F}/\\{G}"
                  '(- #{:E} #{:F}) "{E}-{F}"
                  '(- #{:E} #{:F} #{:G}) "{E}-{F}-{G}"
                  '(unite-sets #{#{}}) "union({{}})"
                  '(intersect-sets #{#{}}) "inter({{}})"
                  '(union-pe (:z) (member :z nat-set) 1) "UNION(z).(z:NAT|1)"
                  '(intersection-pe (:z) (member :z nat-set) 1) "INTER(z).(z:NAT|1)")
    (are [lisb b] (= lisb (b-predicate->lisb b))
                  '(member 1 #{}) "1:{}"
                  '(not-member 1 #{}) "1/:{}"
                  '(subset #{:E} #{:G}) "{E}<:{G}"
                  '(not-subset #{:E} #{:G}) "{E}/<:{G}"
                  '(subset-strict #{:E} #{:G}) "{E}<<:{G}"
                  '(not-subset-strict #{:E} #{:G}) "{E}/<<:{G}")))


(deftest booleans-test
  (testing "booleans"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  true "TRUE"
                  false "FALSE"
                  'bool-set "BOOL"
                  '(pred->bool (= true true)) "bool(TRUE=TRUE)")))


(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [lisb b] (= lisb (b-predicate->lisb b))
                  '(= true false) "TRUE = FALSE"
                  '(not= true false) "TRUE /= FALSE")))


(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [lisb b] (= lisb (b-predicate->lisb b))
                  '(and (= 1 1) (= 2 2)) "1=1 & 2=2"
                  '(and (= 1 1) (= 2 2) (= 3 3)) "1=1 & 2=2 & 3=3"
                  '(or (= 1 1) (= 2 2)) "1=1 or 2=2"
                  '(or (= 1 1) (= 2 2) (= 3 3)) "1=1 or 2=2 or 3=3"
                  '(=> (= 1 1) (= 2 2)) "1=1 => 2=2"
                  '(=> (= 1 1) (= 2 2) (= 3 3)) "1=1 => 2=2 => 3=3"
                  '(<=> (= 1 1) (= 2 2)) "1=1 <=> 2=2"
                  '(<=> (= 1 1) (= 2 2) (= 3 3)) "1=1 <=> 2=2 <=> 3=3"
                  '(not (= 1 1)) "not(1=1)"
                  '(not (= 1 1)) "not(1=1)"
                  '(for-all (:x) (member :x nat-set) (< 0 :x)) "!(x).(x:NAT => 0<x)"
                  '(exists (:x) (and (= 1 1) (= 2 2))) "#(x).(1=1 & 2=2)")))

