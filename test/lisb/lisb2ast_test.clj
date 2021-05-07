(ns lisb.lisb2ast-test
  (:require [clojure.test :refer :all]
            [lisb.lisb2ast :refer :all]
            [lisb.representation :refer :all]               ;[lisb->node-repr, lisb->node-repr1]
            [lisb.examples.simple :as simple]))

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
    (is (= (bfor-all #{:x} (b= 1 1) (b= 2 2)) (macroexpand-1 `(lisb->node-repr1 lift))))))


(defn normalize-string [string]
  (clojure.string/replace string #"[ \n\r\t]" ""))

(deftest examples-simple-test
  (testing "examples-simple"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/" b)))) (normalize-string (get-machine-from-ast (lisb->ast lisb))))
                  "Lift.mch" simple/lift
                  ;"ACounter.mch" simple/a-counter
                  ;"GCD.mch" simple/gcd
                  "KnightsKnaves.mch" simple/knights-knaves
                  "Bakery0.mch" simple/bakery0
                  "Bakery1.mch" simple/bakery1
                  )))

(deftest machine-test
  (testing "machine"
    (is (= "MACHINE Empty\nEND"
           (get-machine-from-ast (lisb->ast
                                   '(machine
                                      (machine-variant)
                                      (machine-header :Empty ()))))))))

(deftest machine-clauses-test
  (testing "machine-clauses"
    (are [b lisb] (= b (get-machine-from-ast (lisb-machine-clause->ast lisb)))
                  "1=1" '(constraints (= 1 1)))))

(deftest substitutions-test
  (testing "substitutions"
    (are [b lisb] (= b (get-machine-from-ast (lisb-substitution->ast lisb)))
                  "skip" 'skip
                  "x := E" '(assign :x :E)
                  ;"f(x) := E"
                  "x::S" '(becomes-element-of #{:x} :S)
                  "x :(x>0) " '(becomes-such #{:x} (> :x 0))
                  "x<--OP(y)" '(operation-call #{:x} :OP #{:y})
                  "skip || skip" '(parallel-substitution skip skip)
                  "skip || skip || skip" '(parallel-substitution skip skip skip)
                  "skip ; skip" '(sequence-substitution skip skip)
                  "skip ; skip ; skip" '(sequence-substitution skip skip skip)
                  "ANY x WHERE x>0 THEN skip END " '(any #{:x} (> :x 0) skip)
                  "LET x BE x=1 IN skip END " '(let-sub #{:x} (= :x 1) skip)
                  "VAR x IN skip END " '(bvar #{:x} skip)
                  "PRE 1=2 THEN skip END " '(pre (= 1 2) skip)
                  "ASSERT 1=2 THEN skip END " '(assert (= 1 2) skip)
                  "CHOICE skip OR skip END " '(choice skip skip)
                  "IF 1=2 THEN skip END " '(if-sub (= 1 2) skip)
                  "IF 1=2 THEN skip ELSE skip END " '(if-sub (= 1 2) skip skip)
                  "IF 1=2 THEN skip ELSIF 1=3 THEN skip END " '(cond (= 1 2) skip (= 1 3) skip)
                  "IF 1=2 THEN skip ELSIF 1=3 THEN skip ELSE skip END " '(cond (= 1 2) skip (= 1 3) skip skip)
                  "SELECT 1=2 THEN skip END " '(select (= 1 2) skip)
                  "SELECT 1=2 THEN skip ELSE x := 1 END " '(select (= 1 2) skip (assign :x 1))
                  "SELECT 1=1 THEN skip WHEN 2=2 THEN skip END " '(select (= 1 1) skip (= 2 2) skip)
                  "SELECT 1=1 THEN skip WHEN 2=2 THEN skip ELSE skip END " '(select (= 1 1) skip (= 2 2) skip skip)
                  ;"CASE E OF EITHER m THEN G OR n THEN H END END"
                  ;"CASE E OF EITHER m THEN G OR n THEN H ELSE I END END"
                  )))

(deftest if-test
  (testing "if"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "IF 1=1 THEN 2 ELSE 3 END" '(if-expr (= 1 1) 2 3))
    (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                  "(1=1 => 2=2) & (not(1=1) => 3=3)" '(and (=> (= 1 1) (= 2 2)) (=> (not (= 1 1)) (= 3 3))))))

#_(deftest let-test
  (testing "let"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "LET x, y BE x=1 & y=2 IN 3 END" '(let-expr #{:x :y} (and (= :x 1) (= :y 2)) 3) )
    (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                  "LET x, y BE x=1 & y=2 IN 0=0 END" '(let-pred #{:x :y} (and (= :x 1) (= :y 2)) (= 0 0)) )))


(deftest strings-test
  (testing "strings"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "\"astring\"" "astring"
                  "STRING" 'string-set
                  "card(\"s\")" '(count "s")
                  "\"s\"~" '(reverse "s")
                  "\"s\"^\"t\"" '(concat "s" "t")
                  ;"conc(['''s''', '''t'''])" '(conc (sequence "s" "t"))
                  )))

(deftest sequences-test
  (testing "sequences"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  ;"<>" '(sequence)
                  ;"[]" '(sequence)
                  ;"[E]" '(sequence :E)
                  ;"[E,F]" '(sequence :E :F)
                  "seq(S)" '(seq :S)
                  "seq1(S)" '(seq1 :S)
                  "iseq(S)" '(iseq :S)
                  "iseq1(S)" '(iseq1 :S)
                  "perm(S)" '(perm :S)
                  "size(S)" '(count-seq :S)
                  "s^t" '(concat :s :t)
                  "E->s" '(-> :E :s)
                  "s<-E" '(<- :s :E)
                  "S~" '(reverse :S)
                  "first(S)" '(first :S)
                  "last(S)" '(last :S)
                  "front(S)" '(front :S)
                  "tail(S)" '(tail :S)
                  "conc(S)" '(conc :S)
                  "s/|\\n" '(restrict-front :s :n)
                  "s\\|/n" '(restrict-tail :s :n) )))

(deftest function-test
  (testing "functions"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "S+->T" '(+-> :S :T)
                  "S-->T" '(--> :S :T)
                  "S+->>T" '(+->> :S :T)
                  "S-->>T" '(-->> :S :T)
                  "S>+>T" '(>+> :S :T)
                  "S>->T" '(>-> :S :T)
                  "S>->>T" '(>->> :S :T)
                  "%x.(1=1|1)" '(lambda #{:x} (= 1 1) 1)
                  ;"f(E)" '(:f :E)
                  ;"f(E,F)" '(:f :E :F)
                  )))

(deftest relation-test
  (testing "relations"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "S<->T" '(<-> :S :T)
                  "S<<->T" '(total-relation :S :T)
                  "S<->>T" '(surjective-relation :S :T)
                  "S<<->>T" '(total-surjective-relation :S :T)
                  ;"E|->F" '(couple :E :F)
                  "dom(r)" '(dom :r)
                  "ran(r)" '(ran :r)
                  "id(S)" '(identity :S)
                  "S<|r" '(<| :S :r)
                  "S<<|r" '(<<| :S :r)
                  "r|>S" '(|> :r :S)
                  "r|>>S" '(|>> :r :S)
                  "r~" '(inverse :r)
                  "r[S]" '(image :r :S)
                  "r1<+r2" '(<+ :r1 :r2)
                  "r1><r2" '(>< :r1 :r2)
                  "(r1;r2)"'(comp :r1 :r2)
                  "(r1||r2)" '(|| :r1 :r2)
                  "prj1(S,T)" '(prj1 :S :T)
                  "prj2(S,T)" '(prj2 :S :T)
                  "closure1(r)" '(closure1 :r)
                  "closure(r)" '(closure :r)
                  "iterate(r,n)" '(iterate :r :n)
                  "fnc(r)" '(fnc :r)
                  "rel(r)" '(rel :r))))

(deftest numbers-test
  (testing "numbers"
    (testing "expressions"
      (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                    "1" 1
                    "-1" -1
                    "-x" '(- :x)
                    "INTEGER" 'integer-set
                    "NATURAL" 'natural-set
                    "NATURAL1" 'natural1-set
                    "INT" 'int-set
                    "NAT" 'nat-set
                    "NAT1" 'nat1-set
                    ;"1..2" '(range 1 3)
                    "MININT" 'min-int
                    "MAXINT" 'max-int
                    "max(NAT)" '(max nat-set)
                    "min(NAT)" '(min nat-set)
                    "PI(z).(z:NAT|1)" '(pi #{:z} (member :z nat-set) 1)
                    "SIGMA(z).(z:NAT|1)" '(sigma #{:z} (member :z nat-set) 1)
                    ))
    (testing "arithmetic"
      (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                    "1+2" '(+ 1 2)
                    "1+2+3" '(+ 1 2 3)
                    "1-2" '(- 1 2)
                    "1-2-3" '(- 1 2 3)
                    "1*2" '(* 1 2)
                    "1*2*3" '(* 1 2 3)
                    "1/2" '(/ 1 2)
                    "1/2/3" '(/ 1 2 3)
                    "1**2" '(** 1 2)
                    "1 mod 2" '(mod 1 2)
                    "1 mod 2 mod 3" '(mod 1 2 3)
                    ;"succ(1)" '(inc 1)
                    ;"pred(1)" '(dec 1)
                    ))
    (testing "predicates"
      (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                    "1>2" '(> 1 2)
                    "1<2" '(< 1 2)
                    "1>=2" '(>= 1 2)
                    "1<=2" '(<= 1 2) ))))

(deftest sets-test
  (testing "sets"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "{}" #{}
                  "{E}" #{:E}
                  ;"{E, F}"  #{:E :F}
                  "{x|x:NAT}" '(comp-set #{:x} (member :x nat-set))
                  "POW({})" '(pow #{})
                  "POW1({})" '(pow1 #{})
                  "FIN({})" '(fin #{})
                  "FIN1({})" '(fin1 #{})
                  "card({})" '(count #{})
                  "{E}*{F}" '(* #{:E} #{:F})
                  "{E}*{F}*{G}" '(* #{:E} #{:F} #{:G})
                  "{E}\\/{F}"  '(union #{:E} #{:F})
                  "{E}\\/{F}\\/{G}"  '(union #{:E} #{:F} #{:G})
                  "{E}/\\{F}" '(intersection #{:E} #{:F})
                  "{E}/\\{F}/\\{G}" '(intersection #{:E} #{:F} #{:G})
                  "{E}-{F}" '(- #{:E} #{:F})
                  "{E}-{F}-{G}" '(- #{:E} #{:F} #{:G})
                  "union({{}})" '(unite-sets #{#{}})
                  "inter({{}})" '(intersect-sets #{#{}})
                  "UNION(z).(z:NAT|1)" '(union-pe #{:z} (member :z nat-set) 1)
                  "INTER(z).(z:NAT|1)" '(intersection-pe #{:z} (member :z nat-set) 1))
    (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                  "1:{}" '(member 1 #{})
                  "1/:{}" '(not-member 1 #{})
                  "{E}<:{G}" '(subset #{:E} #{:G})
                  "{E}/<:{G}" '(not-subset #{:E} #{:G})
                  "{E}<<:{G}" '(subset-strict #{:E} #{:G})
                  "{E}/<<:{G}" '(not-subset-strict #{:E} #{:G}))))

(deftest booleans-test
  (testing "booleans"
    (are [b lisb] (= b (get-machine-from-ast (lisb-expression->ast lisb)))
                  "TRUE" true
                  "FALSE" false
                  "BOOL" 'bool-set
                  "bool(TRUE=TRUE)" '(pred->bool (= true true)))))

(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                  "TRUE=FALSE" '(= true false)
                  "TRUE/=FALSE" '(not= true false))))

(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [b lisb] (= b (get-machine-from-ast (lisb-predicate->ast lisb)))
                  "1=1 & 2=2" '(and (= 1 1) (= 2 2))
                  "1=1 & 2=2 & 3=3" '(and (= 1 1) (= 2 2) (= 3 3))
                  "1=1 or 2=2" '(or (= 1 1) (= 2 2))
                  "1=1 or 2=2 or 3=3" '(or (= 1 1) (= 2 2) (= 3 3))
                  "1=1 => 2=2" '(=> (= 1 1) (= 2 2))
                  "1=1 => 2=2 => 3=3" '(=> (= 1 1) (= 2 2) (= 3 3))
                  "1=1 <=> 2=2" '(<=> (= 1 1) (= 2 2))
                  "1=1 <=> 2=2 <=> 3=3" '(<=> (= 1 1) (= 2 2) (= 3 3))
                  "not(1=1)" '(not (= 1 1))
                  "not(1=1)" '(not (= 1 1))
                  "!x.(1=1 => 2=2)" '(for-all #{:x} (= 1 1) (= 2 2))
                  ;"!x.(x:NAT => 0<x)" '(for-all #{:x} (member :x nat-set) (< 0 :x))
                  ;"#x.(1=1 & 2=2)"'(exists [:x] (and (= 1 1) (= 2 2)))
                  )))

(def func '(:f :E :F))

(defmacro transform-function-calls1 [lisb]
  `(if (keyword? (first ~lisb))
    (conj ~lisb 'call)
    ~lisb))

(defmacro transform-function-calls [lisb]
  (if (keyword? (first lisb))
    1
    2))

(deftest test1
  (testing "test1"
    #_(is (= "(call :f :E :F)" (apply (partial transform-function-calls) func)))
    #_(is (= "f(E,F)" (lisb->node-repr  '(:f :E :F))))))

#_(defn wtf [lisb]
  (eval (concat '(let [+ *]) (list lisb))))

#_(defn wtf2 [lisb]
  (eval (conj (list lisb) '[+ *] 'let)))
