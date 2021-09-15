(ns lisb.translation.ir2ast-test
  (:require [clojure.test :refer :all]
            [lisb.translation.lisb2ir :refer :all]
            [lisb.examples.simple :as simple]
            [lisb.examples.function-returns :as function-returns]
            [lisb.translation.ir2ast :refer [ir->ast ir->predicate-ast ir->expression-ast ir->substitution-ast ir->machine-clause-ast]]
            [lisb.translation.ast2b :refer [ast->b]]))

(defn normalize-string [string]
  (clojure.string/replace string #"[ \n\r\t]" ""))

(deftest function-returns-test
  (testing "function-returns"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/" b)))) (normalize-string (ast->b (ir->ast lisb))))
                  "FunctionReturns.mch" function-returns/function-returns)))

(deftest examples-simple-test
  (testing "examples-simple"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/" b)))) (normalize-string (ast->b (ir->ast (eval `(b ~lisb))))))
                  "Lift.mch" simple/lift
                  "ACounter.mch" simple/a-counter
                  "GCD.mch" simple/gcd
                  "KnightsKnaves.mch" simple/knights-knaves
                  "Bakery0.mch" simple/bakery0
                  "Bakery1.mch" simple/bakery1)))


(deftest machine-test
  (testing "machine"
    (is (= "MACHINE Empty\nEND"
           (ast->b (ir->ast
                                   (b (machine
                                        (machine-variant)
                                        (machine-header :Empty ())))))))))


(deftest machine-clauses-test
  (testing "machine-clauses"
    (are [b lisb] (= b (ast->b (ir->machine-clause-ast lisb)))
                  "CONSTRAINTS 1=1\n" (b (constraints (= 1 1)))
                  "SETS S; T={e1,e2}\n" (b (sets (deferred-set :S) (enumerated-set :T :e1 :e2)))
                  "CONSTANTS x, y\n" (b (constants :x :y))
                  "PROPERTIES\nTRUE=TRUE\n" (b (properties (= true true)))
                  ; definitions
                  "VARIABLES x, y\n" (b (variables :x :y))
                  "INVARIANT TRUE=TRUE\n" (b (invariant (= true true)))
                  "ASSERTIONS\nTRUE=TRUE; FALSE=FALSE\n" (b (assertions (= true true) (= false false)))
                  "INITIALISATION x,y := 0,0\n" (b (init (assign :x 0 :y 0)))
                  "OPERATIONS\ninc = x := x+1;\ndec = x := x-1\n" (b (operations (operation () :inc () (assign :x (+ :x 1))) (operation () :dec () (assign :x (- :x 1))))))))


(deftest substitutions-test
  (testing "substitutions"
    (are [b lisb] (= b (ast->b (ir->substitution-ast lisb)))
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
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "IF 1=1 THEN 2 ELSE 3 END" (b (if-expr (= 1 1) 2 3)))
    (are [b lisb] (= b (ast->b (ir->predicate-ast lisb)))
                  "(1=1 => 2=2) & (not(1=1) => 3=3)" (b (and (=> (= 1 1) (= 2 2)) (=> (not (= 1 1)) (= 3 3)))))))


(deftest let-test
  (testing "let"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "LET y,x BE x=1 & y=2 IN 3 END" (b (let-expr #{:x :y} (and (= :x 1) (= :y 2)) 3)))
    (are [b lisb] (= b (ast->b (ir->predicate-ast lisb)))
                  "LET y,x BE x=1 & y=2 IN 0=0 END" (b (let-pred #{:x :y} (and (= :x 1) (= :y 2)) (= 0 0))))))


(deftest strings-test
  (testing "strings"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "\"astring\"" (b "astring")
                  "STRING" (b string-set)
                  "size(\"s\")" (b (count-seq "s"))
                  "rev(\"s\")" (b (reverse "s"))
                  "\"s\"^\"t\"" (b (concat "s" "t"))
                  "conc([\"s\",\"t\"])" (b (conc (sequence "s" "t"))))))


(deftest struct-test
  (testing "structs"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "struct(n:NAT)" (b (struct :n nat-set))
                  "struct(n:NAT,b:BOOL)" (b (struct :n nat-set, :b bool-set))
                  "rec(n:1)" (b (record :n 1))
                  "rec(n:1,b:TRUE)" (b (record :n 1, :b true))
                  "R'n" (b (rec-get :R :n)))))


(deftest sequences-test
  (testing "sequences"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "[]" (b (sequence))
                  "[E]" (b (sequence :E))
                  "[E,F]" (b (sequence :E :F))
                  "seq(S)" (b (seq :S))
                  "seq1(S)" (b (seq1 :S))
                  "iseq(S)" (b (iseq :S))
                  "iseq1(S)" (b (iseq1 :S))
                  "perm(S)" (b (perm :S))
                  "size(S)" (b (count-seq :S))
                  "s^t" (b (concat :s :t))
                  "s^t^u" (b (concat :s :t :u))
                  "s^t^u^v" (b (concat :s :t :u :v))
                  "E->s" (b (cons :s :E))
                  "F->(E->s)" (b (cons :s :E :F))
                  "G->(F->(E->s))" (b (cons :s :E :F :G))
                  "s<-E" (b (append :s :E))
                  "s<-E<-F" (b (append :s :E :F))
                  "s<-E<-F<-G" (b (append :s :E :F :G))
                  "rev(S)" (b (reverse :S))
                  "first(S)" (b (first :S))
                  "last(S)" (b (last :S))
                  "front(S)" (b (drop-last :S))
                  "tail(S)" (b (rest :S))
                  "conc(S)" (b (conc :S))
                  "s/|\\n" (b (take :n :s))
                  "s\\|/n" (b (drop :n :s)))))


(deftest function-test
  (testing "functions"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "S+->T" (b (+-> :S :T))
                  "S+->T+->U" (b (+-> :S :T :U))
                  "S+->T+->U+->V" (b (+-> :S :T :U :V))
                  "S-->T" (b (--> :S :T))
                  "S-->T-->U" (b (--> :S :T :U))
                  "S-->T-->U-->V" (b (--> :S :T :U :V))
                  "S+->>T" (b (+->> :S :T))
                  "S+->>T+->>U" (b (+->> :S :T :U))
                  "S+->>T+->>U+->>V" (b (+->> :S :T :U :V))
                  "S-->>T" (b (-->> :S :T))
                  "S-->>T-->>U" (b (-->> :S :T :U))
                  "S-->>T-->>U-->>V" (b (-->> :S :T :U :V))
                  "S>+>T" (b (>+> :S :T))
                  "S>+>T>+>U" (b (>+> :S :T :U))
                  "S>+>T>+>U>+>V" (b (>+> :S :T :U :V))
                  "S>->T" (b (>-> :S :T))
                  "S>->T>->U" (b (>-> :S :T :U))
                  "S>->T>->U>->V" (b (>-> :S :T :U :V))
                  "S>->>T" (b (>->> :S :T))
                  "S>->>T>->>U" (b (>->> :S :T :U))
                  "S>->>T>->>U>->>V" (b (>->> :S :T :U :V))
                  "%x.(1=1|1)" (b (lambda #{:x} (= 1 1) 1))
                  "f(E)" (b (call :f :E)))))


(deftest relation-test
  (testing "relations"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "S<->T" (b (<-> :S :T))
                  "S<->T<->U" (b (<-> :S :T :U))
                  "S<->T<->U<->V" (b (<-> :S :T :U :V))
                  "S<<->T" (b (total-relation :S :T))
                  "S<<->T<<->U" (b (total-relation :S :T :U))
                  "S<<->T<<->U<<->V" (b (total-relation :S :T :U :V))
                  "S<->>T" (b (surjective-relation :S :T))
                  "S<->>T<->>U" (b (surjective-relation :S :T :U))
                  "S<->>T<->>U<->>V" (b (surjective-relation :S :T :U :V))
                  "S<<->>T" (b (total-surjective-relation :S :T))
                  "S<<->>T<<->>U" (b (total-surjective-relation :S :T :U))
                  "S<<->>T<<->>U<<->>V" (b (total-surjective-relation :S :T :U :V))
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
                  "r1<+r2<+r3" (b (<+ :r1 :r2 :r3))
                  "r1<+r2<+r3<+r4" (b (<+ :r1 :r2 :r3 :r4))
                  "r1><r2" (b (>< :r1 :r2))
                  "r1><r2><r3" (b (>< :r1 :r2 :r3))
                  "r1><r2><r3><r4" (b (>< :r1 :r2 :r3 :r4))
                  "(r1;r2)" (b (comp :r1 :r2))
                  "((r1;r2);r3)" (b (comp :r1 :r2 :r3))
                  "(((r1;r2);r3);r4)" (b (comp :r1 :r2 :r3 :r4))
                  "(r1||r2)" (b (|| :r1 :r2))
                  "((r1||r2)||r3)" (b (|| :r1 :r2 :r3))
                  "(((r1||r2)||r3)||r4)" (b (|| :r1 :r2 :r3 :r4))
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
      (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                    "1" (b 1)
                    "-1" (b -1)
                    "-x" (b (- :x))
                    "INTEGER" (b integer-set)
                    "NATURAL" (b natural-set)
                    "NATURAL1" (b natural1-set)
                    "INT" (b int-set)
                    "NAT" (b nat-set)
                    "NAT1" (b nat1-set)
                    "1..2" (b (range 1 3))
                    "MININT" (b min-int)
                    "MAXINT" (b max-int)
                    "max(NAT)" (b (max nat-set))
                    "max({1,3,2})" (b (max 1 2 3))
                    "min(NAT)" (b (min nat-set))
                    "min({1,3,2})" (b (min 1 2 3))
                    "PI(z).(z:NAT|1)" (b (pi #{:z} (contains? nat-set  :z) 1))
                    "SIGMA(z).(z:NAT|1)" (b (sigma #{:z} (contains? nat-set :z) 1)))
      (testing "arithmetic"
        (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                      "1+2" (b (+ 1 2))
                      "1+2+3" (b (+ 1 2 3))
                      "1-2" (b (- 1 2))
                      "1-2-3" (b (- 1 2 3))
                      "1*2" (b (* 1 2))
                      "1*2*3" (b (* 1 2 3))
                      "1*2*3*4" (b (* 1 2 3 4))
                      "1/2" (b (/ 1 2))
                      "1/2/3" (b (/ 1 2 3))
                      "1/2/3/4" (b (/ 1 2 3 4))
                      "1**2" (b (** 1 2))
                      "1**2**3" (b (** 1 2 3))
                      "1**2**3**4" (b (** 1 2 3 4))
                      "1 mod 2" (b (mod 1 2))
                      "1 mod 2 mod 3" (b (mod 1 2 3))
                      "1 mod 2 mod 3 mod 4" (b (mod 1 2 3 4))
                      "succ(1)" (b (inc 1))
                      "pred(1)" (b (dec 1)))))
    (testing "predicates"
      (are [b lisb] (= b (ast->b (ir->predicate-ast lisb)))
                    "1>2" (b (> 1 2))
                    "1>2 & 2>3" (b (> 1 2 3))
                    "1>2 & 2>3 & 3>4" (b (> 1 2 3 4))
                    "1<2" (b (< 1 2))
                    "1<2 & 2<3" (b (< 1 2 3))
                    "1<2 & 2<3 & 3<4" (b (< 1 2 3 4))
                    "1>=2" (b (>= 1 2))
                    "1>=2 & 2>=3" (b (>= 1 2 3))
                    "1>=2 & 2>=3 & 3>=4" (b (>= 1 2 3 4))
                    "1<=2" (b (<= 1 2))
                    "1<=2 & 2<=3" (b (<= 1 2 3))
                    "1<=2 & 2<=3 & 3<=4" (b (<= 1 2 3 4))))))


(deftest sets-test
  (testing "sets"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "{}" (b #{})
                  "{E}" (b #{:E})
                  "{F,E}" (b #{:E :F})
                  "{x|x:NAT}" (b (comp-set #{:x} (contains? nat-set :x)))
                  "POW({})" (b (pow #{}))
                  "POW1({})" (b (pow1 #{}))
                  "FIN({})" (b (fin #{}))
                  "FIN1({})" (b (fin1 #{}))
                  "card({})" (b (count #{}))
                  "{E}*{F}" (b (* #{:E} #{:F}))
                  "{E}*{F}*{G}" (b (* #{:E} #{:F} #{:G}))
                  "{E}*{F}*{G}*{H}" (b (* #{:E} #{:F} #{:G} #{:H}))
                  "{E}\\/{F}" (b (union #{:E} #{:F}))
                  "{E}\\/{F}\\/{G}" (b (union #{:E} #{:F} #{:G}))
                  "{E}\\/{F}\\/{G}\\/{H}" (b (union #{:E} #{:F} #{:G} #{:H}))
                  "{E}/\\{F}" (b (intersection #{:E} #{:F}))
                  "{E}/\\{F}/\\{G}" (b (intersection #{:E} #{:F} #{:G}))
                  "{E}/\\{F}/\\{G}/\\{H}" (b (intersection #{:E} #{:F} #{:G} #{:H}))
                  "{E}\\{F}" (b (difference #{:E} #{:F}))
                  "{E}\\{F}\\{G}" (b (difference #{:E} #{:F} #{:G}))
                  "{E}\\{F}\\{G}\\{H}" (b (difference #{:E} #{:F} #{:G} #{:H}))
                  "union({{}})" (b (unite-sets #{#{}}))
                  "union({{E}})" (b (unite-sets #{#{:E}}))
                  "union({{E},{F}})" (b (unite-sets #{#{:E} #{:F}}))
                  "inter({{}})" (b (intersect-sets #{#{}}))
                  "inter({{E}})" (b (intersect-sets #{#{:E}}))
                  "inter({{E},{F}})" (b (intersect-sets #{#{:E} #{:F}}))
                  "UNION(z).(z:NAT|1)" (b (union-pe #{:z} (contains? nat-set :z) 1))
                  "INTER(z).(z:NAT|1)" (b (intersection-pe #{:z} (contains? nat-set :z) 1)))
    (are [b lisb] (= b (ast->b (ir->predicate-ast lisb)))
                  "1:{}" (b (contains? #{} 1))
                  "1/:{}" (b (not (contains? #{} 1)))
                  "{E}<:{G}" (b (subset? #{:E} #{:G}))
                  "{E}/<:{G}" (b (not (subset? #{:E} #{:G})))
                  "{E}<<:{G}" (b (subset-strict? #{:E} #{:G}))
                  "{E}/<<:{G}" (b (not (subset-strict? #{:E} #{:G})))
                  "{G}<:{E}" (b (superset? #{:E} #{:G}))
                  "{G}/<:{E}" (b (not (superset? #{:E} #{:G})))
                  "{G}<<:{E}" (b (superset-strict? #{:E} #{:G}))
                  "{G}/<<:{E}" (b (not (superset-strict? #{:E} #{:G}))))))


(deftest booleans-test
  (testing "booleans"
    (are [b lisb] (= b (ast->b (ir->expression-ast lisb)))
                  "TRUE" (b true)
                  "FALSE" (b false)
                  "BOOL" (b bool-set)
                  "bool(TRUE=TRUE)" (b (pred->bool (= true true))))))


(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [b lisb] (= b (ast->b (ir->predicate-ast lisb)))
                  "TRUE=FALSE" (b (= true false))
                  "TRUE/=FALSE" (b (not= true false))
                  "1/=1 & 1/=2 & 1/=3 & 2/=3" (b (distinct? 1 2 1 3)))))


(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [b lisb] (= b (ast->b (ir->predicate-ast lisb)))
                  "1=1 & 2=2" (b (and (= 1 1) (= 2 2)))
                  "1=1 & 2=2 & 3=3" (b (and (= 1 1) (= 2 2) (= 3 3)))
                  "1=1 & 2=2 & 3=3 & 4=4" (b (and (= 1 1) (= 2 2) (= 3 3) (= 4 4)))
                  "1=1 or 2=2" (b (or (= 1 1) (= 2 2)))
                  "1=1 or 2=2 or 3=3" (b (or (= 1 1) (= 2 2) (= 3 3)))
                  "1=1 or 2=2 or 3=3 or 4=4" (b (or (= 1 1) (= 2 2) (= 3 3) (= 4 4)))
                  "1=1 => 2=2" (b (=> (= 1 1) (= 2 2)))
                  "1=1 => 2=2 => 3=3" (b (=> (= 1 1) (= 2 2) (= 3 3)))
                  "1=1 => 2=2 => 3=3 => 4=4" (b (=> (= 1 1) (= 2 2) (= 3 3) (= 4 4)))
                  "1=1 <=> 2=2" (b (<=> (= 1 1) (= 2 2)))
                  "1=1 <=> 2=2 <=> 3=3" (b (<=> (= 1 1) (= 2 2) (= 3 3)))
                  "1=1 <=> 2=2 <=> 3=3 <=> 4=4" (b (<=> (= 1 1) (= 2 2) (= 3 3) (= 4 4)))
                  "not(1=1)" (b (not (= 1 1)))
                  "!x.(x:NAT => 0<x)" (b (for-all [:x] (=> (contains? nat-set :x) (< 0 :x))))
                  "#x.(x:NAT & 0=x)" (b (exists [:x] (and (contains? nat-set :x) (= 0 :x)))))))
