(ns lisb.translation.ir2ast-test
  (:require [clojure.test :refer :all]
            [lisb.translation.util :refer :all]
            [lisb.translation.types :refer :all]
            [lisb.examples.simple :as simple]
            [lisb.examples.marriages :as marriages]
            [lisb.examples.function-returns :as function-returns]
            [lisb.examples.sebastian :as sebastian]
            [clojure.java.io]
            [clojure.string])
  (:require [clojure.spec.alpha :as s]))

(s/check-asserts true)

(defn normalize-string [string]
  (clojure.string/replace string #"[ \n\r\t]" ""))

(deftest function-returns-test
  (testing "function-returns"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/b/" b)))) (normalize-string (ast->b (ir->ast lisb))))
                  "FunctionReturns.mch" function-returns/function-returns)))

(deftest examples-sebastian-test
  (testing "examples-sebastian"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/b/sebastian/" b)))) (normalize-string (ast->b (ir->ast lisb))))
                  "GenericTimersMC.mch" sebastian/generic-timer-mc
                  "TrafficLight2.mch" sebastian/traffic-light2
                  "TrafficLightTime_Ref.mch" sebastian/traffic-light-time-ref
                  )))

(deftest examples-marriages-test
  (testing "examples-marriages"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/b/marriages/" b)))) (normalize-string (ast->b (ir->ast lisb))))
                  "Life.mch" marriages/life
                  "Marriage.mch" marriages/marriage
                  "Registrar.mch" marriages/registrar
                  )))

(deftest examples-simple-test
  (testing "examples-simple"
    (are [b lisb] (= (normalize-string (slurp (clojure.java.io/resource (str "machines/b/simple/" b)))) (normalize-string (ast->b (ir->ast lisb))))
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
                     (b (machine :Empty))))))))

(deftest definition-test
  (testing "definitions"
    (are [b ir] (= b (ir->b ir))
                "DEFINITIONS\nSET_PREF_MAX_OPERATIONS == 10" (b (definitions (expression-definition :SET_PREF_MAX_OPERATIONS [] 10)))
                "DEFINITIONS\nCHOOSE(X) == \"a member of X\";\nEXTERNAL_FUNCTION_CHOOSE(T) == POW(T)-->T" (b (definitions (expression-definition :CHOOSE [:X] "a member of X") (expression-definition :EXTERNAL_FUNCTION_CHOOSE [:T] (--> (pow :T) :T))))
                "DEFINITIONS\nPRED == x<y" (b (definitions (predicate-definition :PRED [] (< :x :y))))
                "DEFINITIONS\nSUBST == skip" (b (definitions (substitution-definition :SUBST [] skip)))
                "DEFINITIONS\nBTRUE == 1=1;\nBFALSE == 1/=1;\nEXPR == 1+1;\nSUBST == a_mch := {}" (b (definitions (predicate-definition :BTRUE [] (= 1 1)) (predicate-definition :BFALSE [] (not= 1 1)) (expression-definition :EXPR [] (+ 1 1)) (substitution-definition :SUBST [] (assign :a_mch #{})))))))

(deftest machine-clauses-test
  (testing "machine-clauses"
    (testing "machine-inclusions"
      (are [b ir] (= b (ast->b (ir->ast ir)))
                  "USES Lift" (b (uses :Lift))
                  "INCLUDES Lift" (b (includes :Lift))
                  "INCLUDES Lift(FLOORS)" (b (includes [:Lift :FLOORS]))
                  "SEES Lift" (b (sees :Lift))
                  "EXTENDS Lift" (b (extends :Lift))
                  "EXTENDS Lift(FLOORS)" (b (extends [:Lift :FLOORS]))
                  "PROMOTES inc" (b (promotes :inc))
                  ))
    (testing "machine-sections"
      (are [b ir] (= b (ast->b (ir->ast ir)))
                  "CONSTRAINTS 1=1" (b (constraints (= 1 1)))
                  "CONSTRAINTS 1=1 & 2=2 & 3=3" (b (constraints (= 1 1) (= 2 2) (= 3 3)))
                  "SETS S; T = {e1, e2}" (b (sets (deferred-set :S) (enumerated-set :T :e1 :e2)))
                  "SETS S; T = {e1, e2}" (b (sets :S :T #{:e1 :e2}))
                  "CONSTANTS x, y" (b (constants :x :y))
                  "PROPERTIES 1=1" (b (properties (= 1 1)))
                  "PROPERTIES 1=1 & 2=2 & 3=3" (b (properties (= 1 1) (= 2 2) (= 3 3)))
                  ; definitions
                  "FREETYPES List = Nil, Cons(INTEGER*List); Option = Some(INTEGER), None" (b (freetypes (freetype :List [] (constructor :Nil) (constructor :Cons (cart-or-mult integer-set :List))) (freetype :Option [] (constructor :Some integer-set) (constructor :None))))
                  "VARIABLES x, y" (b (variables :x :y))
                  "INVARIANT 1=1" (b (invariants (= 1 1)))
                  "INVARIANT 1=1 & 2=2 & 3=3" (b (invariants (= 1 1) (= 2 2) (= 3 3)))
                  "ASSERTIONS TRUE=TRUE; FALSE=FALSE" (b (assertions (= true true) (= false false)))
                  "INITIALISATION x := 0" (b (init (assign :x 0)))
                  "INITIALISATION x := 0 ; y := 0 ; z := 0" (b (init (assign :x 0) (assign :y 0) (assign :z 0)))
                  "OPERATIONS\ninc = x := x+1;\na <-- dec(x) = x := x-1" (b (operations
                                                                                (:inc [] (assign :x (+ :x 1)))
                                                                                (<-- [:a] (:dec [:x] (assign :x (- :x 1))))))))))


(deftest substitutions-test
  (testing "substitutions"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                "skip" (b skip)
                "x := E" (b (assign :x :E))
                "x,y := E,F" (b (assign :x :E :y :F))
                "f(x) := E" (b (assign (fn-call :f :x) :E))
                "x :: S" (b (becomes-element-of [:x] :S))
                "x : (x>0)" (b (becomes-such [:x] (> :x 0)))
                "op(x)" (b (op-call :op :x))
                "a <-- op(x)" (b (<-- [:a] (op-call :op :x)))
                "skip || skip" (b (parallel-sub skip skip))
                "skip || skip || skip" (b (parallel-sub skip skip skip))
                "skip ; skip" (b (sequential-sub skip skip))
                "skip ; skip ; skip" (b (sequential-sub skip skip skip))
                "ANY x WHERE x>0 THEN skip END" (b (any [:x] (> :x 0) skip))
                "ANY x WHERE x>0 THEN skip ; skip END" (b (any [:x] (> :x 0) skip skip))
                "LET x BE x=1 IN skip END" (b (let-sub [:x 1] skip))
                "LET x BE x=1 IN skip ; skip END" (b (let-sub [:x 1] skip skip))
                "VAR x IN skip END" (b (bvar #{:x} skip))
                "VAR x IN skip ; skip END" (b (bvar #{:x} skip skip))
                "PRE 1=2 THEN skip END" (b (pre (= 1 2) skip))
                "PRE 1=2 THEN skip ; skip END" (b (pre (= 1 2) skip skip))
                "ASSERT 1=2 THEN skip END" (b (assert (= 1 2) skip))
                "ASSERT 1=2 THEN skip ; skip END" (b (assert (= 1 2) skip skip))
                "CHOICE skip OR skip END" (b (choice skip skip))
                "IF 1=2 THEN skip END" (b (if-sub (= 1 2) skip))
                "IF 1=2 THEN skip ELSE skip END" (b (if-sub (= 1 2) skip skip))
                "IF 1=2 THEN skip ELSIF 1=3 THEN skip END" (b (cond (= 1 2) skip (= 1 3) skip))
                "IF 1=2 THEN skip ELSIF 1=3 THEN skip ELSE skip END" (b (cond (= 1 2) skip (= 1 3) skip skip))
                "SELECT 1=2 THEN skip END" (b (select (= 1 2) skip))
                "SELECT 1=2 THEN skip ELSE x := 1 END" (b (select (= 1 2) skip (assign :x 1)))
                "SELECT 1=1 THEN skip WHEN 2=2 THEN skip END" (b (select (= 1 1) skip (= 2 2) skip))
                "SELECT 1=1 THEN skip WHEN 2=2 THEN skip ELSE skip END" (b (select (= 1 1) skip (= 2 2) skip skip))
                "CASE 1+1 OF EITHER 1 THEN skip OR 2 THEN skip END END" (b (case (+ 1 1) 1 skip 2 skip))
                "CASE 1+1 OF EITHER 1 THEN skip OR 2 THEN skip ELSE skip END END" (b (case (+ 1 1) 1 skip 2 skip skip))
                "CASE 1+1 OF EITHER 1 THEN skip OR 2 THEN skip ELSE skip END END" (b (case (+ 1 1) 1 skip 2 skip skip))
                )))


(deftest if-test
  (testing "if"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                  "IF 1=1 THEN 2 ELSE 3 END" (b (if (= 1 1) 2 3)))
    (are [b ir] (= b (ast->b (ir->ast ir)))
                  "(1=1 => 2=2) & (not(1=1) => 3=3)" (b (and (=> (= 1 1) (= 2 2)) (=> (not (= 1 1)) (= 3 3)))))))


(deftest let-test
  (testing "let"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                  "LET x,y BE x=1 & y=2 IN 3 END" (b (let [:x 1 :y 2] 3)))
    (are [b ir] (= b (ast->b (ir->ast ir)))
                  "LET x,y BE x=1 & y=2 IN 0=0 END" (b (let [:x 1 :y 2] (= 0 0))))))


(deftest strings-test
  (testing "strings"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                "\"astring\"" (b "astring")
                "STRING" (b string-set)
                "size(\"s\")" (b (size "s"))
                "rev(\"s\")" (b (reverse "s"))
                "\"s\"^\"t\"" (b (concat "s" "t"))
                "conc([\"s\", \"t\"])" (b (conc (sequence "s" "t"))))))


(deftest struct-test
  (testing "structs"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                  "struct(n: NAT)" (b (struct :n nat-set))
                  "struct(n: NAT, b: BOOL)" (b (struct :n nat-set, :b bool-set))
                  "rec(n: 1)" (b (record :n 1))
                  "rec(n: 1, b: TRUE)" (b (record :n 1, :b true))
                  "R'n" (b (record-get :R :n)))))


(deftest sequences-test
  (testing "sequences"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                "[]" (b (sequence))
                "[E]" (b (sequence :E))
                "[E, F]" (b (sequence :E :F))
                "seq(S)" (b (seq :S))
                "seq1(S)" (b (seq1 :S))
                "iseq(S)" (b (iseq :S))
                "iseq1(S)" (b (iseq1 :S))
                "perm(S)" (b (perm :S))
                "size(S)" (b (size :S))
                "s^t" (b (concat :s :t))
                "s^t^u" (b (concat :s :t :u))
                "s^t^u^v" (b (concat :s :t :u :v))
                "E->s" (b (-> :E :s))
                "s<-E" (b (<- :s :E))
                "s<-E<-F" (b (<- :s :E :F))
                "rev(S)" (b (reverse :S))
                "first(S)" (b (first :S))
                "last(S)" (b (last :S))
                "front(S)" (b (front :S))
                "tail(S)" (b (tail :S))
                "conc(S)" (b (conc :S))
                "s/|\\n" (b (take :n :s))
                "s\\|/n" (b (drop :n :s)))))


(deftest function-test
  (testing "functions"
    (are [b ir] (= b (ast->b (ir->ast ir)))
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
                  "%(x).(1=1|1)" (b (lambda [:x] (= 1 1) 1))
                  "f(E)" (b (fn-call :f :E)))))


(deftest relation-test
  (testing "relations"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                "S<->T" (b (<-> :S :T))
                "S<->T<->U" (b (<-> :S :T :U))
                "S<->T<->U<->V" (b (<-> :S :T :U :V))
                "S<<->T" (b (<<-> :S :T))
                "S<<->T<<->U" (b (<<-> :S :T :U))
                "S<<->T<<->U<<->V" (b (<<-> :S :T :U :V))
                "S<->>T" (b (<->> :S :T))
                "S<->>T<->>U" (b (<->> :S :T :U))
                "S<->>T<->>U<->>V" (b (<->> :S :T :U :V))
                "S<<->>T" (b (<<->> :S :T))
                "S<<->>T<<->>U" (b (<<->> :S :T :U))
                "S<<->>T<<->>U<<->>V" (b (<<->> :S :T :U :V))
                "(E, F)" (b (maplet :E :F))
                "(E, F, G)" (b (maplet :E :F :G))
                "dom(r)" (b (dom :r))
                "ran(r)" (b (ran :r))
                "id(S)" (b (id :S))
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
                "(r1;r2)" (b (composition :r1 :r2))
                "((r1;r2);r3)" (b (composition :r1 :r2 :r3))
                "(((r1;r2);r3);r4)" (b (composition :r1 :r2 :r3 :r4))
                "(r1||r2)" (b (parallel-product :r1 :r2))
                "((r1||r2)||r3)" (b (parallel-product :r1 :r2 :r3))
                "(((r1||r2)||r3)||r4)" (b (parallel-product :r1 :r2 :r3 :r4))
                "prj1(S, T)" (b (prj1 :S :T))
                "prj2(S, T)" (b (prj2 :S :T))
                "closure1(r)" (b (closure1 :r))
                "closure(r)" (b (closure :r))
                "iterate(r, n)" (b (iterate :r :n))
                "fnc(r)" (b (fnc :r))
                "rel(r)" (b (rel :r))
                )))


(deftest numbers-test
  (testing "numbers"
    (testing "expressions"
      (are [b ir] (= b (ast->b (ir->ast ir)))
                    "1" (b 1)
                    "-1" (b -1)
                    "-x" (b (- :x))
                    "INTEGER" (b integer-set)
                    "NATURAL" (b natural-set)
                    "NATURAL1" (b natural1-set)
                    "INT" (b int-set)
                    "NAT" (b nat-set)
                    "NAT1" (b nat1-set)
                    "1..pred(3)" (b (range 1 3))
                    "MININT" (b min-int)
                    "MAXINT" (b max-int)
                    "max(NAT)" (b (max nat-set))
                    "max({1, 3, 2})" (b (max 1 2 3))
                    "min(NAT)" (b (min nat-set))
                    "min({1, 3, 2})" (b (min 1 2 3))
                    "PI(z).(z:NAT|1)" (b (pi [:z] (contains? nat-set  :z) 1))
                    "SIGMA(z).(z:NAT|1)" (b (sigma [:z] (contains? nat-set :z) 1)))
      (testing "arithmetic"
        (are [b ir] (= b (ast->b (ir->ast ir)))
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
      (are [b ir] (= b (ast->b (ir->ast ir)))
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
    (are [b ir] (= b (ast->b (ir->ast ir)))
                "{}" (b #{})
                "{E}" (b #{:E})
                "{F, E}" (b #{:E :F})
                "{x|x:NAT}" (b (comprehension-set :x (contains? nat-set :x)))
                "{x,y|x:NAT}" (b (comprehension-set [:x :y] (contains? nat-set :x)))
                "{x|x:NAT}" (b #{:x | (in :x nat-set)})
                "{x,y|x:NAT}" (b #{[:x :y] | (in :x nat-set)})
                "POW({})" (b (pow #{}))
                "POW1({})" (b (pow1 #{}))
                "FIN({})" (b (fin #{}))
                "FIN1({})" (b (fin1 #{}))
                "card({})" (b (card #{}))
                "{E}*{F}" (b (cartesian-product #{:E} #{:F}))
                "{E}*{F}*{G}" (b (cartesian-product #{:E} #{:F} #{:G}))
                "{E}*{F}*{G}*{H}" (b (cartesian-product #{:E} #{:F} #{:G} #{:H}))
                "{E}\\/{F}" (b (union #{:E} #{:F}))
                "{E}\\/{F}\\/{G}" (b (union #{:E} #{:F} #{:G}))
                "{E}\\/{F}\\/{G}\\/{H}" (b (union #{:E} #{:F} #{:G} #{:H}))
                "{E}/\\{F}" (b (intersection #{:E} #{:F}))
                "{E}/\\{F}/\\{G}" (b (intersection #{:E} #{:F} #{:G}))
                "{E}/\\{F}/\\{G}/\\{H}" (b (intersection #{:E} #{:F} #{:G} #{:H}))
                "{E}\\{F}" (b (set- #{:E} #{:F}))
                "{E}\\{F}\\{G}" (b (set- #{:E} #{:F} #{:G}))
                "{E}\\{F}\\{G}\\{H}" (b (set- #{:E} #{:F} #{:G} #{:H}))
                "union({{}})" (b (unite-sets #{#{}}))
                "union({{E}})" (b (unite-sets #{#{:E}}))
                "union({{E}, {F}})" (b (unite-sets #{#{:E} #{:F}}))
                "inter({{}})" (b (intersect-sets #{#{}}))
                "inter({{E}})" (b (intersect-sets #{#{:E}}))
                "inter({{E}, {F}})" (b (intersect-sets #{#{:E} #{:F}}))
                "UNION(z).(z:NAT|1)" (b (union-pe #{:z} (contains? nat-set :z) 1))
                "INTER(z).(z:NAT|1)" (b (intersection-pe #{:z} (contains? nat-set :z) 1)))
    (are [b ir] (= b (ast->b (ir->ast ir)))
                "1:{}" (b (contains? #{} 1))
                "1:{} & 2:{}" (b (contains? #{} 1 2))
                "1/:{}" (b (not (contains? #{} 1)))
                "not(1:{} & 2:{})" (b (not (contains? #{} 1 2)))
                "{E}<:{F}" (b (subset? #{:E} #{:F}))
                "{E}<:{F} & {F}<:{G}" (b (subset? #{:E} #{:F} #{:G}))
                "{E}/<:{F}" (b (not (subset? #{:E} #{:F})))
                "not({E}<:{F} & {F}<:{G})" (b (not (subset? #{:E} #{:F} #{:G})))
                "{E}<<:{F}" (b (strict-subset? #{:E} #{:F}))
                "{E}<<:{F} & {F}<<:{G}" (b (strict-subset? #{:E} #{:F} #{:G}))
                "{E}/<<:{F}" (b (not (strict-subset? #{:E} #{:F})))
                "not({E}<<:{F} & {F}<<:{G})" (b (not (strict-subset? #{:E} #{:F} #{:G})))
                "{F}<:{E}" (b (superset? #{:E} #{:F}))
                "{G}<:{F} & {F}<:{E}" (b (superset? #{:E} #{:F} #{:G}))
                "{F}/<:{E}" (b (not (superset? #{:E} #{:F})))
                "not({G}<:{F} & {F}<:{E})" (b (not (superset? #{:E} #{:F} #{:G})))
                "{F}<<:{E}" (b (strict-superset? #{:E} #{:F}))
                "{G}<<:{F} & {F}<<:{E}" (b (strict-superset? #{:E} #{:F} #{:G}))
                "{F}/<<:{E}" (b (not (strict-superset? #{:E} #{:F})))
                "not({G}<<:{F} & {F}<<:{E})" (b (not (strict-superset? #{:E} #{:F} #{:G})))
                )))


(deftest booleans-test
  (testing "booleans"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                  "TRUE" (b true)
                  "FALSE" (b false)
                  "BOOL" (b bool-set)
                  "bool(TRUE=TRUE)" (b (pred->bool (= true true))))))


(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                  "TRUE=FALSE" (b (= true false))
                  "TRUE/=FALSE" (b (not= true false))
                  "1/=2 & 1/=3 & 2/=3" (b (distinct? 1 2 3))
                  "1/=1 & 1/=2 & 1/=3 & 2/=3" (b (distinct? 1 2 1 3)))))


(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [b ir] (= b (ast->b (ir->ast ir)))
                ;"1=1 & 2=2" {:tag :and, :preds [1]}
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
                "!(x).(x:NAT => 0<=x)" (b (for-all [:x] (member? :x nat-set) (<= 0 :x)))
                "!(x).(x:NAT => 0<=x)" (b (for-all [:x] (=> (member? :x nat-set) (<= 0 :x))))
                "!(x,y).(x:NAT & y:NAT => 0<=x+y)" (b (for-all [:x :y] (and (member? :x nat-set) (member? :y nat-set)) (<= 0 (+ :x :y))))
                "!(x,y).(x:NAT & y:NAT => 0<=x+y)" (b (for-all [:x :y] (=> (and (member? :x nat-set) (member? :y nat-set)) (<= 0 (+ :x :y)))))
                "#(x).(x:NAT & 0=x)" (b (exists [:x] (and (member? :x nat-set) (= 0 :x))))
                "#(x,y).(x:NAT & y:NAT & 0=x+y)" (b (exists [:x :y] (and (member? :x nat-set) (member? :y nat-set) (= 0 (+ :x :y)))))
                )))

(deftest tuples-test
  (testing "tuples are indeed translated to the correct AST"
    (is (= "(1, 2)" (ast->b (ir->ast (->Tuple 1 2)))))))
