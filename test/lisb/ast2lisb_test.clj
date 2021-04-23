(ns lisb.ast2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.ast2lisb :refer :all]
            [lisb.representation :refer :all])
  (:import (de.be4.classicalb.core.parser BParser)))

(import de.be4.classicalb.core.parser.visualisation.ASTPrinter)
(def printer (ASTPrinter.))
(defn print-ast [b-ast]
  (.apply b-ast printer))

;;; parse units

(deftest machine-test
  (testing "machine"
    (is
      (=
        (bmachine
          (bmachine-variant)
          (bmachine-header :Empty ()))
        (b->lisb "MACHINE Empty\nEND")))))

;;; machine clauses

#_(deftest constraint-test
  (testing "constraint"
    (is (=
          (b->lisb (slurp (clojure.java.io/resource "machines/Constraint.mch")))))))

(deftest machine-clause-sets-test
  (testing "machine-clause-sets"
    (is (= (bmachine
             (bmachine-variant)
             (bmachine-header :Set ())
             (bsets (bdeferred-set :S) (benumerated-set :T #{:E :F})))
           (b->lisb (slurp (clojure.java.io/resource "machines/Set.mch")))))))

(deftest machine-clause-constants-test
  (testing "machine-clause-constants"
    (is (= (bmachine
             (bmachine-variant)
             (bmachine-header :Constant ())
             (bconstants :con)
             (bproperties (b= :con 1)))
           (b->lisb (slurp (clojure.java.io/resource "machines/Constant.mch")))))))

(deftest machine-clause-variables-test
  (testing "machine-clause-variables"
    (is (= (bmachine
             (bmachine-variant)
             (bmachine-header :Variable ())
             (bvariables :nat)
             (binvariants (bmember :nat (bnat-set)))
             (binit (bblock (bassign '(:nat) '(0)))))
           (b->lisb (slurp (clojure.java.io/resource "machines/Variable.mch")))))))


(deftest substitutions-test
  (testing "substitutions"
    (is (= (bskip) (b-substitution->lisb "skip")))
    (is (= (bassign '(:x) '(:E)) (b-substitution->lisb "x := E")))
    #_(is (= "" (b-substitution->lisb "f(x) := E")))
    (is (= (bbecomes-element-of '(:x) :S) (b-substitution->lisb "x :: S")))
    (is (= (bbecomes-such '(:x) (b> :x 0)) (b-substitution->lisb "x : (x>0)")))
    (is (= (boperation-call '(:x) :OP '(:y)) (b-substitution->lisb "x <-- OP(y)")))
    (is (= (bparallel-substitution (bskip) (bskip)) (b-substitution->lisb "skip||skip")))
    (is (= (bparallel-substitution (bskip) (bskip) (bskip)) (b-substitution->lisb "skip||skip||skip")))
    (is (= (bsequence-substitution (bskip) (bskip)) (b-substitution->lisb "skip;skip")))
    (is (= (bsequence-substitution (bskip) (bskip) (bskip)) (b-substitution->lisb "skip;skip;skip")))
    (is (= (bany '(:x) (b> :x 0) (bskip)) (b-substitution->lisb "ANY x WHERE (x>0) THEN skip END")))
    (is (= (blet-sub '(:x) (b= :x 1) (bskip)) (b-substitution->lisb "LET x BE x=1 IN skip END")))
    (is (= (bvar '(:x) (bskip)) (b-substitution->lisb "VAR x IN skip END")))
    (is (= (bprecondition (b= 1 2) (bskip)) (b-substitution->lisb "PRE 1=2 THEN skip END")))
    (is (= (bassert (b= 1 2) (bskip)) (b-substitution->lisb "ASSERT 1=2 THEN skip END")))
    (is (= (bchoice (bskip) (bskip)) (b-substitution->lisb "CHOICE skip OR skip END")))
    (is (= (bif-sub (b= 1 2) (bskip)) (b-substitution->lisb "IF 1=2 THEN skip END")))
    (is (= (bif-sub (b= 1 2) (bskip) (bskip)) (b-substitution->lisb "IF 1=2 THEN skip ELSE skip END")))
    (is (= (bif-sub (b= 1 2) (bskip) (bif-sub (b= 1 3) (bskip))) (b-substitution->lisb "IF 1=2 THEN skip ELSIF 1=3 THEN skip END")))
    (is (= (bif-sub (b= 1 2) (bskip) (bif-sub (b= 1 3) (bskip) (bskip))) (b-substitution->lisb "IF 1=2 THEN skip ELSIF 1=3 THEN skip ELSE skip END")))
    #_(is (= (b-substitution->lisb "SELECT 1=1 THEN skip WHEN 2=2 THEN skip END")))
    #_(is (= (b-substitution->lisb "SELECT 1=1 THEN G WHEN Q THEN H ELSE I END")))
    #_(is (= (b-substitution->lisb "CASE E OF EITHER m THEN G OR n THEN H END END")))
    #_(is (= (b-substitution->lisb "CASE E OF EITHER m THEN G OR n THEN H ELSE I END END")))))


(deftest if-test
  (testing "if"
    (is (= (bif-expr (b= 1 1) 2 3) (b-expression->lisb "IF 1=1 THEN 2 ELSE 3 END")))
    (is (= (band (b=> (b= 1 1) (b= 2 2)) (b=> (bnot (b= 1 1)) (b= 3 3))) (b-predicate->lisb "IF 1=1 THEN 2=2 ELSE 3=3 END")))))

(deftest let-test
  (testing "let"
    (is (= (blet-expr [:x 1 :y 2] 3) (b-expression->lisb "LET x, y BE x=1 & y=2 IN 3 END")))
    (is (= (blet-pred [:x 1 :y 2] (b= 0 0)) (b-predicate->lisb "LET x, y BE x=1 & y=2 IN 0=0 END")))))


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
    (is (= "astring" (b-formula->lisb "\"astring\"")))
    (is (= "astring" (b-formula->lisb "'''astring'''")))
    (is (= (bstring-set) (b-formula->lisb "STRING")))
    (is (= (bcount "s") (b-formula->lisb "size('''s''')")))
    (is (= (breverse "s") (b-formula->lisb "rev('''s''')")))
    (is (= (bconcat "s" "t") (b-formula->lisb "'''s'''^'''t'''")))
    (is (= (bconc (bsequence "s" "t")) (b-formula->lisb "conc(['''s''', '''t'''])")))))


#_(deftest records-test
  (testing "records"
    #_(is (= (bstruct [:n (bnat-set)]) (b-expression->lisb "struct(n:NAT")))
    #_(is (= (b-expression->lisb "struct(n:NAT,b:BOOL")))
    #_(is (= (b-expression->lisb "rec(n:1")))
    #_(is (= (b-expression->lisb "rec(n:1,b:TRUE")))
    #_(is (= (b-expression->lisb "E'n")))))


(deftest sequences-test
  (testing "sequences"
    (testing "empty sequence"
      (is (= (bsequence) (b-expression->lisb "<>")))
      (is (= (bsequence) (b-expression->lisb "[]"))))
    (is (= (bsequence :E) (b-expression->lisb "[E]")))
    (is (= (bsequence :E :F) (b-expression->lisb "[E,F]")))
    (is (= (bseq :S) (b-expression->lisb "seq(S)")))
    (is (= (bseq1 :S) (b-expression->lisb "seq1(S)")))
    (is (= (biseq :S) (b-expression->lisb "iseq(S)")))
    (is (= (biseq1 :S) (b-expression->lisb "iseq1(S)")))
    (is (= (bperm :S) (b-expression->lisb "perm(S)")))
    (is (= (bcount :S) (b-expression->lisb "size(S)")))
    (is (= (bconcat :s :t) (b-expression->lisb "s^t")))
    (is (= (b-> :E :s) (b-expression->lisb "E->s")))
    (is (= (b<- :s :E) (b-expression->lisb "s<-E")))
    (is (= (breverse :S) (b-expression->lisb "rev(S)")))
    (is (= (bfirst :S) (b-expression->lisb "first(S)")))
    (is (= (blast :S) (b-expression->lisb "last(S)")))
    (is (= (bfront :S) (b-expression->lisb "front(S)")))
    (is (= (btail :S) (b-expression->lisb "tail(S)")))
    (is (= (bconc :S) (b-expression->lisb "conc(S)")))
    (is (= (brestrict-front :s :n) (b-expression->lisb "s/|\\n")))
    (is (= (brestrict-tail :s :n) (b-expression->lisb "s\\|/n")))))


(deftest function-test
  (testing "functions"
    (is (= (b+-> :S :T) (b-expression->lisb "S+->T")))
    (is (= (b--> :S :T) (b-expression->lisb "S-->T")))
    (is (= (b+->> :S :T) (b-expression->lisb "S+->>T")))
    (is (= (b-->> :S :T) (b-expression->lisb "S-->>T")))
    (is (= (b>+> :S :T) (b-expression->lisb "S>+>T")))
    (is (= (b>-> :S :T) (b-expression->lisb "S>->T")))
    (is (= (b>->> :S :T) (b-expression->lisb "S>->>T")))
    (is (= (blambda '(:x) (b= 1 1) 1) (b-expression->lisb "%x.(1=1|1)")))
    (is (= (bcall :f :E) (b-expression->lisb "f(E)")))
    (is (= (bcall :f :E :F) (b-expression->lisb "f(E,F)")))))


(deftest relation-test
  (testing "relations"
    (is (= (b<-> :S :T) (b-expression->lisb "S<->T")))
    (is (= (btotal-relation :S :T) (b-expression->lisb "S<<->T")))
    (is (= (bsurjective-relation :S :T) (b-expression->lisb "S<->>T")))
    (is (= (btotal-surjective-relation :S :T) (b-expression->lisb "S<<->>T")))
    (is (= (bcouple :E :F) (b-expression->lisb "E|->F")))
    (is (= (bdom :r) (b-expression->lisb "dom(r)")))
    (is (= (bran :r) (b-expression->lisb "ran(r)")))
    (is (= (bid :S) (b-expression->lisb "id(S)")))
    (is (= (b<| :S :r) (b-expression->lisb "S<|r")))
    (is (= (b<<| :S :r) (b-expression->lisb "S<<|r")))
    (is (= (b|> :r :S) (b-expression->lisb "r|>S")))
    (is (= (b|>> :r :S) (b-expression->lisb "r|>>S")))
    (is (= (binverse :r) (b-expression->lisb "r~")))
    (is (= (bimage :r :S) (b-expression->lisb "r[S]")))
    (is (= (b<+ :r1 :r2) (b-expression->lisb "r1<+r2")))
    (is (= (b>< :r1 :r2) (b-expression->lisb "r1><r2")))
    (is (= (bcomp :r1 :r2) (b-expression->lisb "(r1;r2)")))
    (is (= (b|| :r1 :r2) (b-expression->lisb "(r1||r2)")))
    (is (= (bprj1 :S :T) (b-expression->lisb "prj1(S,T)")))
    (is (= (bprj2 :S :T) (b-expression->lisb "prj2(S,T)")))
    (is (= (bclosure1 :r) (b-expression->lisb "closure1(r)")))
    (is (= (bclosure :r) (b-expression->lisb "closure(r)")))
    (is (= (biterate :r :n) (b-expression->lisb "iterate(r,n)")))
    (is (= (bfnc :r) (b-expression->lisb "fnc(r)")))
    (is (= (brel :r) (b-expression->lisb "rel(r)")))))


(deftest number-test
  (testing "numbers"
    (is (= 1 (b-expression->lisb "1")))
    (is (= -1 (b-expression->lisb "-1")))
    (is (= (b- :x) (b-expression->lisb "-x")))
    (is (= 15 (b-expression->lisb "0xF")))
    (is (= (binteger-set) (b-expression->lisb "INTEGER")))
    (is (= (bnatural-set) (b-expression->lisb "NATURAL")))
    (is (= (bnatural1-set) (b-expression->lisb "NATURAL1")))
    (is (= (bint-set) (b-expression->lisb "INT")))
    (is (= (bnat-set) (b-expression->lisb "NAT")))
    (is (= (bnat1-set) (b-expression->lisb "NAT1")))
    (is (= (binterval 1 2) (b-expression->lisb "1..2")))
    (is (= (bmin-int) (b-expression->lisb "MININT")))
    (is (= (bmax-int) (b-expression->lisb "MAXINT")))
    (is (= (bmax (bnat-set)) (b-expression->lisb "max(NAT)")))
    (is (= (bmin (bnat-set)) (b-expression->lisb "min(NAT)")))
    (is (= (bpi '(:z) (bmember :z (bnat-set)) 1) (b-formula->lisb "PI(z).(z:NAT|1)")))
    (is (= (bsigma '(:z) (bmember :z (bnat-set)) 1)(b-formula->lisb "SIGMA(z).(z:NAT|1)"))))
  (testing "number predicates"
    (is (= (b> 1 2) (b-predicate->lisb "1>2")))
    (is (= (b< 1 2) (b-predicate->lisb "1<2")))
    (is (= (b>= 1 2) (b-predicate->lisb "1>=2")))
    (is (= (b<= 1 2) (b-predicate->lisb "1<=2"))))
  (testing "number-arithmetic"
    (is (= (b+ 1 2) (b-expression->lisb "1+2")))
    (is (= (b+ 1 2 3) (b-expression->lisb "1+2+3")))
    (is (= (b- 1 2) (b-expression->lisb "1-2")))
    (is (= (b- 1 2 3) (b-expression->lisb "1-2-3")))
    (is (= (b* 1 2) (b-expression->lisb "1*2")))
    (is (= (b* 1 2 3) (b-expression->lisb "1*2*3")))
    (is (= (bdiv 1 2) (b-expression->lisb "1/2")))
    (is (= (bdiv 1 2 3) (b-expression->lisb "1/2/3")))
    (is (= (b** 1 2) (b-expression->lisb "1**2")))
    (is (= (bmod 1 2) (b-expression->lisb "1 mod 2")))
    (is (= (bmod 1 2 3) (b-expression->lisb "1 mod 2 mod 3")))
    (is (= (binc 1) (b-expression->lisb "succ(1)")))
    (is (= (bdec 1) (b-expression->lisb "pred(1)")))))


(deftest sets-test
  (testing "sets"
    (is (= #{} (b-expression->lisb "{}")))
    (is (= #{:E} (b-expression->lisb "{E}")))
    (is (= #{:E :F} (b-expression->lisb "{E, F}")))
    (is (= (bcomp-set '(:x) (bmember :x (bnat-set))) (b-expression->lisb "{x|x:NAT}")))
    (is (= (bpow #{}) (b-expression->lisb "POW({})")))
    (is (= (bpow1 #{}) (b-expression->lisb "POW1({})")))
    (is (= (bfin #{}) (b-expression->lisb "FIN({})")))
    (is (= (bfin1 #{}) (b-expression->lisb "FIN1({})")))
    (is (= (bcount #{}) (b-expression->lisb "card({})")))
    (is (= (b* #{:E} #{:F}) (b-expression->lisb "{E}*{F}")))
    (is (= (b* #{:E} #{:F} #{:G}) (b-expression->lisb "{E}*{F}*{G}")))
    (is (= (bunion #{:E} #{:F}) (b-expression->lisb "{E}\\/{F}")))
    (is (= (bunion #{:E} #{:F} #{:G}) (b-expression->lisb "{E}\\/{F}\\/{G}")))
    (is (= (bintersection #{:E} #{:F}) (b-expression->lisb "{E}/\\{F}")))
    (is (= (bintersection #{:E} #{:F} #{:G}) (b-expression->lisb "{E}/\\{F}/\\{G}")))
    (is (= (b- #{:E} #{:F}) (b-expression->lisb "{E}-{F}")))
    (is (= (b- #{:E} #{:F} #{:G}) (b-expression->lisb "{E}-{F}-{G}")))
    (is (= (bmember 1 #{}) (b-predicate->lisb "1:{}")))
    (is (= (bnot-member 1 #{}) (b-predicate->lisb "1/:{}")))
    (is (= (bsubset #{:E} #{:G}) (b-predicate->lisb "{E}<:{G}")))
    (is (= (bnot-subset #{:E} #{:G}) (b-predicate->lisb "{E}/<:{G}")))
    (is (= (bsubset-strict #{:E} #{:G}) (b-predicate->lisb "{E}<<:{G}")))
    (is (= (bnot-subset-strict #{:E} #{:G}) (b-predicate->lisb "{E}/<<:{G}")))
    (is (= (bunite-sets #{#{}}) (b-expression->lisb "union({{}})")))
    (is (= (bintersect-sets #{#{}}) (b-expression->lisb "inter({{}})")))
    (is (= (bunion-pe '(:z) (bmember :z (bnat-set)) 1) (b-expression->lisb "UNION(z).(z:NAT|1)")))
    (is (= (bintersection-pe '(:z) (bmember :z (bnat-set)) 1) (b-expression->lisb "INTER(z).(z:NAT|1)")))))


(deftest booleans-test
  (testing "booleans"
    (is (= true (b-expression->lisb "TRUE")))
    (is (= false (b-expression->lisb "FALSE")))
    (is (= (bbool-set) (b-expression->lisb "BOOL")))
    (is (= (bpred->bool (b= true true)) (b-expression->lisb "bool(TRUE=TRUE)")))))


(deftest equality-predicates-test
  (testing "equality-predicates"
    (is (= (b= true false) (b-predicate->lisb "TRUE = FALSE")))
    (is (= (bnot= true false) (b-predicate->lisb "TRUE /= FALSE")))))


(deftest logical-predicates-test
  (testing "equality-predicates"
    (is (= (band (b= 1 1) (b= 2 2)) (b-predicate->lisb "1=1 & 2=2")))
    (is (= (band (b= 1 1) (b= 2 2) (b= 3 3)) (b-predicate->lisb "1=1 & 2=2 & 3=3")))
    (is (= (bor (b= 1 1) (b= 2 2)) (b-predicate->lisb "1=1 or 2=2")))
    (is (= (bor (b= 1 1) (b= 2 2) (b= 3 3)) (b-predicate->lisb "1=1 or 2=2 or 3=3")))
    (is (= (b=> (b= 1 1) (b= 2 2)) (b-predicate->lisb "1=1 => 2=2")))
    (is (= (b=> (b= 1 1) (b= 2 2) (b= 3 3)) (b-predicate->lisb "1=1 => 2=2 => 3=3")))
    (is (= (b<=> (b= 1 1) (b= 2 2)) (b-predicate->lisb "1=1 <=> 2=2")))
    (is (= (b<=> (b= 1 1) (b= 2 2) (b= 3 3)) (b-predicate->lisb "1=1 <=> 2=2 <=> 3=3")))
    (is (= (bfor-all '(:x) (bmember :x (bnat-set)) (b< 0 :x)) (b-predicate->lisb "!(x).(x:NAT => 0<x)")))
    (is (= (bexists '(:x) (band (b= 1 1) (b= 2 2))) (b-predicate->lisb "#(x).(1=1 & 2=2)")))))

