(ns lisb.ast2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.ast2lisb :refer :all]
            [lisb.representation :refer :all]))

(import de.be4.classicalb.core.parser.visualisation.ASTPrinter)
(def printer (ASTPrinter.))
(defn print-ast [b-ast]
  (.apply b-ast printer))

(deftest machine-test
  (testing "machine"
    (is
      (=
        (bmachine
          (bmachine-variant)
          (bmachine-header :Empty ()))
        (b->lisb "MACHINE Empty\nEND")))))

#_(deftest constraint-test
  (testing "constraint"
    (is (=
          (bmachinestr->lisb (slurp (clojure.java.io/resource "machines/Constraint.mch")))))))

(deftest constant-test
  (testing "constant"
    (is (= (bmachine
             (bmachine-variant)
             (bmachine-header :Constant ())
             (bconstants :con)
             (bproperties (b= :con 1)))
           (b->lisb (slurp (clojure.java.io/resource "machines/Constant.mch")))))))

(deftest variable-test
  (testing "variable"
    (is (= (bmachine
             (bmachine-variant)
             (bmachine-header :Variable ())
             (bvariables :nat)
             (binvariants (bmember :nat (bnat-set)))
             (binit (bblock (bassign '(:nat) '(0)))))
           (b->lisb (slurp (clojure.java.io/resource "machines/Variable.mch")))))))


(deftest logical-predicates-test
  (testing "equality-predicates"
    (is (= (band (b= 1 2) (b= true false)) (b-predicate->lisb "1=2 & TRUE=FALSE")))
    (is (= (bor (b= 1 2) (b= true false)) (b-predicate->lisb "1=2 or TRUE=FALSE")))
    (is (= (b=> (b= 1 2) (b= true false)) (b-predicate->lisb "1=2 => TRUE=FALSE")))
    (is (= (b<=> (b= 1 2) (b= true false)) (b-predicate->lisb "1=2 <=> TRUE=FALSE")))
    (is (= (bforall '(:x) (b=> (b= 1 2) (b= true false))) (b-predicate->lisb "!(x).(1=2 => TRUE=FALSE)")))
    (is (= (bexists '(:x) (band (b= 1 2) (b= true false))) (b-predicate->lisb "#(x).(1=2 & TRUE = FALSE)")))))

(deftest equality-predicates-test
  (testing "equality-predicates"
    (is (= (b= 1 2) (b-predicate->lisb "1 = 2")))
    (is (= (bnot= 1 2) (b-predicate->lisb "1 /= 2")))))

(deftest booleans-test
  (testing "booleans"
    (is (= true (b-expression->lisb "TRUE")))
    (is (= false (b-expression->lisb "FALSE")))
    (is (= (bbool-set) (b-expression->lisb "BOOL")))
    (is (= (bpred->bool (b= 1 2)) (b-expression->lisb "bool(1=2)")))))

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
    (is (= (b* #{} #{}) (b-expression->lisb "{}*{}")))
    (is (= (bunion #{} #{}) (b-expression->lisb "{}\\/{}")))
    (is (= (bintersection #{} #{}) (b-expression->lisb "{}/\\{}")))
    (is (= (b- #{} #{}) (b-expression->lisb "{}-{}")))
    (is (= (bmember 1 #{}) (b-predicate->lisb "1:{}")))
    (is (= (bnot-member 1 #{}) (b-predicate->lisb "1/:{}")))
    (is (= (bsubset #{} #{}) (b-predicate->lisb "{}<:{}")))
    (is (= (bnot-subset #{} #{}) (b-predicate->lisb "{}/<:{}")))
    (is (= (bsubset-strict #{} #{}) (b-predicate->lisb "{}<<:{}")))
    (is (= (bnot-subset-strict #{} #{}) (b-predicate->lisb "{}/<<:{}")))
    #_(is (= (bunion #{}) (b-expression->lisb "union({})")))
    #_(is (= (binter #{}) (b-expression->lisb "inter({})")))
    #_(is (= (b-expression->lisb "UNION(z).(P|E)")))
    #_(is (= (b-expression->lisb "INTER(z).(P|E)")))))

(deftest number-test
  (testing "numbers"
    (is (= (binteger-set) (b-expression->lisb "INTEGER")))
    (is (= (bnatural-set) (b-expression->lisb "NATURAL")))
    (is (= (bnatural1-set) (b-expression->lisb "NATURAL1")))
    (is (= (bint-set) (b-expression->lisb "INT")))
    (is (= (bnat-set) (b-expression->lisb "NAT")))
    (is (= (bnat1-set) (b-expression->lisb "NAT1")))
    (is (= (binterval 1 2) (b-expression->lisb "1..2")))
    (is (= (bmax (bnat-set)) (b-expression->lisb "max(NAT)")))
    (is (= (bmin (bnat-set)) (b-expression->lisb "min(NAT)")))
    #_(is (= (b-formula->lisb "PI(z).(P|E)")))
    #_(is (= (b-formula->lisb "SIGMA(z).(P|E)")))
    #_(is (= (bsucc 1) (b-formula->lisb "succ(1)")))
    #_(is (= (bpred 1) (b-formula->lisb "pred(1)")))
    #_(is (= 15 (b-formula->lisb "0xF")))))

(deftest number-arithmetic-test
  (testing "number-arithmetic"
    (is (= (b+ 1 2) (b-expression->lisb "1+2")))
    (is (= (b- 1 2) (b-expression->lisb "1-2")))
    (is (= (b* 1 2) (b-expression->lisb "1*2")))
    (is (= (bdiv 1 2) (b-expression->lisb "1/2")))
    (is (= (b** 1 2) (b-expression->lisb "1**2")))
    (is (= (bmod 1 2) (b-expression->lisb "1 mod 2")))))

(deftest number-predicate-test
  (testing "number predicates"
    (is (= (b> 1 2) (b-predicate->lisb "1>2")))
    (is (= (b< 1 2) (b-predicate->lisb "1<2")))
    (is (= (b>= 1 2) (b-predicate->lisb "1>=2")))
    (is (= (b<= 1 2) (b-predicate->lisb "1<=2")))))

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

(deftest function-test
  (testing "functions"
    (is (= (b+-> :S :T) (b-expression->lisb "S+->T")))
    (is (= (b--> :S :T) (b-expression->lisb "S-->T")))
    (is (= (b+->> :S :T) (b-expression->lisb "S+->>T")))
    (is (= (b-->> :S :T) (b-expression->lisb "S-->>T")))
    (is (= (b>+> :S :T) (b-expression->lisb "S>+>T")))
    (is (= (b>-> :S :T) (b-expression->lisb "S>->T")))
    (is (= (b>->> :S :T) (b-expression->lisb "S>->>T")))
    #_(is (= (b-expression->lisb "%x.(P|E)")))
    (is (= (bapply :f :E) (b-expression->lisb "f(E)")))
    #_(is (= (b-expression->lisb "f(E1,...,En)")))))

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
    (is (=  (b-expression->lisb "size(S)")))                ; TODO
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

#_(deftest records-test
  (testing "records"
    (is (= (b-formula->lisb "struct(ID:S,...,ID:S")))
    (is (= (b-formula->lisb "rec(ID:E,...,ID:E")))
    (is (= (b-formula->lisb "E'ID")))))

#_(deftest strings-test
  (testing "strings"
    (is (= (b-formula->lisb "\"astring\"")))
    (is (= (b-formula->lisb "'''astring'''")))
    (is (= (b-formula->lisb "STRING")))
    (is (= (b-formula->lisb "size('''s''')")))
    (is (= (b-formula->lisb "rev('''s''')")))
    (is (= (b-formula->lisb "'''s'''^'''t'''")))
    (is (= (b-formula->lisb "conc('''s''', '''t''')")))))

#_(deftest reals-test
  (testing "reals"
    (is (= (b-formula->lisb "REAL")))
    (is (= (b-formula->lisb "FLOAT")))
    (is (= (b-formula->lisb "1.2")))
    (is (= (b-formula->lisb "real(1)")))
    (is (= (b-formula->lisb "floor(1.2)")))
    (is (= (b-formula->lisb "ceiling(1.2)")))))

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

#_(deftest if-then-else-test
  (testing "if-then-else"
    (is (= (b-formula->lisb "IF P THEN E1 ELSE E2 END")))))

#_(deftest let-test
  (testing "let"
    (is (= (b-formula->lisb "LET x BE x=1 In x+2 END")))))

#_(deftest statement-test
  (testing "substitutions"
    (is (= (b-formula->lisb "skip")))
    (is (= (b-formula->lisb "x := E")))
    (is (= (b-formula->lisb "f(x) := E")))
    (is (= (b-formula->lisb "x :: S")))
    (is (= (b-formula->lisb "x : (P)")))
    (is (= (b-formula->lisb "x <-- OP(x)")))
    (is (= (b-formula->lisb "G||H")))
    (is (= (b-formula->lisb "G;H")))
    (is (= (b-formula->lisb "ANY x,... WHERE P THEN G END")))
    (is (= (b-formula->lisb "LET x,... BE x=E & ... In G END")))))

#_(deftest equality-predicates-test-2
  (testing "equality-predicates"
    (is (= (print-ast (parse-b-machine (slurp (clojure.java.io/resource "machines/EqualityPredicates.mch"))))))))

#_(deftest equality-predicates-test-3
  (testing "equality-predicates"
    (is (= (b= 1 2)
           (print-ast (parse-b-formula "1 = 2"))))))

#_(deftest machine-clauses-test
  (testing "machine-clauses"
    (is (= (println (type '(1 2 3)))
           (print-ast (slurp (clojure.java.io/resource "machines/NumberPredicates.mch")))))))
