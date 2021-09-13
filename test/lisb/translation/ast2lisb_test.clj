(ns lisb.translation.ast2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.translation.ast2lisb :refer :all]
            [lisb.examples.simple :as simple]
            [lisb.translation.lisb2ir :refer [b]]
            [lisb.translation.b2ast :refer :all]))

(defn b->lisb [b] (ast->lisb (b->ast b)))
(defn b-predicate->lisb [b-predicate] (ast->lisb (b-predicate->ast b-predicate)))
(defn b-expression->lisb [b-predicate] (ast->lisb (b-expression->ast b-predicate)))
(defn b-formula->lisb [b-predicate] (ast->lisb (b-formula->ast b-predicate)))
(defn b-substitution->lisb [b-predicate] (ast->lisb (b-substitution->ast b-predicate)))

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
             (machine-header :Empty []))
          (b->lisb "MACHINE Empty\nEND")))))

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
                     (machine-header :Set [])
                     (sets (deferred-set :S) (enumerated-set :T :e1 :e2))) "Set.mch"
                  '(machine
                     (machine-variant)
                     (machine-header :Constant [])
                     (constants :con)
                     (properties (= :con 1))) "Constant.mch"
                  ; TODO
                  #_(machine
                    (machine-variant)
                    (machine-header :Definitions ()))       ;"Definitions.mch"
                  '(machine
                     (machine-variant)
                     (machine-header :Variable [])
                     (variables :nat)
                     (invariant (contains? nat-set :nat))
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
                  '(any [:x] (> :x 0) skip) "ANY x WHERE (x>0) THEN skip END"
                  '(let-sub (:x) (= :x 1) skip) "LET x BE x=1 IN skip END"
                  '(var-sub [:x] skip) "VAR x IN skip END"
                  '(pre (= 1 2) skip) "PRE 1=2 THEN skip END"
                  '(assert (= 1 2) skip) "ASSERT 1=2 THEN skip END"
                  '(choice skip skip) "CHOICE skip OR skip END"
                  '(if-sub (= 1 2) skip) "IF 1=2 THEN skip END"
                  '(if-sub (= 1 2) skip skip) "IF 1=2 THEN skip ELSE skip END"
                  '(cond (= 1 2) skip (= 1 3) skip) "IF 1=2 THEN skip ELSIF 1=3 THEN skip END"
                  '(cond (= 1 2) skip (= 1 3) skip skip) "IF 1=2 THEN skip ELSIF 1=3 THEN skip ELSE skip END"
                  '(select (= 1 2) skip) "SELECT 1=2 THEN skip END"
                  '(select (= 1 2) skip (assign :x 1)) "SELECT 1=2 THEN skip ELSE x:= 1 END"
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
                  '(let-expr [:x :y] (and (= :x 1) (= :y 2)) 3) "LET x, y BE x=1 & y=2 IN 3 END")
    (are [lisb b] (= lisb (b-predicate->lisb b))
                  '(let-pred [:x :y] (and (= :x 1) (= :y 2)) (= 0 0)) "LET x, y BE x=1 & y=2 IN 0=0 END")))


(deftest strings-test
  (testing "strings"
    (are [lisb b] (= lisb (b-formula->lisb b))
                  "astring" "\"astring\""
                  "astring" "'''astring'''"
                  'string-set "STRING"
                  '(count-seq "s") "size('''s''')"
                  '(reverse "s") "rev('''s''')"
                  '(concat "s" "t") "'''s'''^'''t'''"
                  '(conc (sequence "s" "t")) "conc(['''s''', '''t'''])")))


(deftest records-test
  (testing "records"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  '(struct :n nat-set) "struct(n:NAT)"
                  '(struct :n nat-set, :b bool-set) "struct(n:NAT,b:BOOL)"
                  '(record :n 1) "rec(n:1)"
                  '(record :n 1, :b true) "rec(n:1,b:TRUE)"
                  '(rec-get :E :n) "E'n")))


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
                  '(cons :s :E) "E->s"
                  '(cons :s :E :F) "F->(E->s)"
                  '(cons :s :E :F :G) "G->(F->(E->s))"
                  '(append :s :E) "s<-E"
                  '(reverse :S) "rev(S)"
                  '(first :S) "first(S)"
                  '(last :S) "last(S)"
                  '(drop-last :S) "front(S)"
                  '(rest :S) "tail(S)"
                  '(conc :S) "conc(S)"
                  '(take :n :s) "s/|\\n"
                  '(drop :n :s) "s\\|/n")))


(deftest function-test
  (testing "functions"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  '(+-> :S :T) "S+->T"
                  '(--> :S :T) "S-->T"
                  '(+->> :S :T) "S+->>T"
                  '(-->> :S :T) "S-->>T"
                  '(>+> :S :T) "S>+>T"
                  '(>-> :S :T) "S>->T"
                  '(>->> :S :T) "S>->>T"
                  '(lambda [:x] (= 1 1) 1) "%x.(1=1|1)"
                  '(call :f :E) "f(E)"
                  '(call :f :E :F) "f(E,F)")))


(deftest relation-test
  (testing "relations"
    (are [lisb b] (= lisb (b-expression->lisb b))
                  '(<-> :S :T) "S<->T"
                  '(<-> :S :T :U) "S<->T<->U"
                  '(total-relation :S :T) "S<<->T"
                  '(total-relation :S :T :U) "S<<->T<<->U"
                  '(surjective-relation :S :T) "S<->>T"
                  '(surjective-relation :S :T :U) "S<->>T<->>U"
                  '(total-surjective-relation :S :T) "S<<->>T"
                  '(total-surjective-relation :S :T :U) "S<<->>T<<->>U"
                  '[:E :F] "E|->F"
                  '[:E :F :G] "E|->F|->G"
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
                    '(interval 1 2) "1..2"
                    'min-int "MININT"
                    'max-int "MAXINT"
                    '(max nat-set) "max(NAT)"
                    '(min nat-set) "min(NAT)"
                    '(pi [:z] (contains? nat-set :z) 1) "PI(z).(z:NAT|1)"
                    '(sigma [:z] (contains? nat-set :z) 1) "SIGMA(z).(z:NAT|1)"))
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
                    '(<= 1 2) "1<=2"
                    ))))


(deftest sets-test
  (testing "sets"
    (are [lisb b-expr] (= lisb (b-expression->lisb b-expr))
                  #{} "{}"
                  #{:E} "{E}"
                  #{:E :F} "{E, F}"
                  '(comp-set [:x] (contains? nat-set :x)) "{x|x:NAT}"
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
                  '(difference #{:E} #{:F}) "{E}\\{F}"
                  '(difference #{:E} #{:F} #{:G}) "{E}\\{F}\\{G}"
                  '(unite-sets #{#{}}) "union({{}})"
                  '(intersect-sets #{#{}}) "inter({{}})"
                  '(union-pe [:z] (contains? nat-set :z) 1) "UNION(z).(z:NAT|1)"
                  '(intersection-pe [:z] (contains? nat-set :z) 1) "INTER(z).(z:NAT|1)")
    (are [lisb b-pred] (= lisb (b-predicate->lisb b-pred))
                  '(contains? #{} 1) "1:{}"
                  '(not (contains? #{} 1)) "1/:{}"
                  '(subset? #{:E} #{:G}) "{E}<:{G}"
                  '(not (subset? #{:E} #{:G})) "{E}/<:{G}"
                  '(subset-strict? #{:E} #{:G}) "{E}<<:{G}"
                  '(not (subset-strict? #{:E} #{:G})) "{E}/<<:{G}")))


(deftest booleans-test
  (testing "booleans"
    (are [lisb b-expr] (= lisb (b-expression->lisb b-expr))
                  true "TRUE"
                  false "FALSE"
                  'bool-set "BOOL"
                  '(pred->bool (= true true)) "bool(TRUE=TRUE)")))


(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [lisb b-pred] (= lisb (b-predicate->lisb b-pred))
                  '(= true false) "TRUE = FALSE"
                  '(not= true false) "TRUE /= FALSE")))


(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [lisb b-pred] (= lisb (b-predicate->lisb b-pred))
                       '(and (= 1 1) (= 2 2)) "1=1 & 2=2"
                       '(and (= 1 1) (= 2 2) (= 3 3)) "1=1 & 2=2 & 3=3"
                       '(or (= 1 1) (= 2 2)) "1=1 or 2=2"
                       '(or (= 1 1) (= 2 2) (= 3 3)) "1=1 or 2=2 or 3=3"
                       '(=> (= 1 1) (= 2 2)) "1=1 => 2=2"
                       '(=> (= 1 1) (= 2 2) (= 3 3)) "1=1 => 2=2 => 3=3"
                       '(<=> (= 1 1) (= 2 2)) "1=1 <=> 2=2"
                       '(<=> (= 1 1) (= 2 2) (= 3 3)) "1=1 <=> 2=2 <=> 3=3"
                       '(not (= 1 1)) "not(1=1)"
                       '(for-all [:x] (=> (contains? nat-set :x) (< 0 :x))) "!(x).(x:NAT => 0<x)"
                       '(exists [:x] (and (= 1 1) (= 2 2))) "#(x).(1=1 & 2=2)")))
