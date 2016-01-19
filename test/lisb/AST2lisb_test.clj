(ns lisb.AST2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.AST2lisb :refer :all]))


(deftest pretty-printer-test
  (testing "for example integers"
    (is (= 1 (parse-print-read "1"))))
  (testing "booleans"
    (is (= true  (parse-print-read "TRUE")))
    (is (= false (parse-print-read "FALSE")))
    (is (= '(bpred->bool (b= 1 1)) (parse-print-read "bool(1=1)")))) 
  (testing "identifiers"
    (is (= '(b= a :b) (parse-print-read "a=b" #{"a"}))))
  (testing "sets"
    (is (= #{}    (parse-print-read "{}")))
    (is (= '(bset-enum 1)         (parse-print-read "{1}"))) 
    (is (= '(bset-enum 1 2)       (parse-print-read "{1,2}")))
    (is (= '(bset [:x] (b< :x 5)) (parse-print-read "{x|x<5}")))
    (is (= '(bcouple 1 2)         (parse-print-read "1|->2")))
    (is (= '(bbool-set)           (parse-print-read "BOOL")))
    (is (= '(bnat-set)            (parse-print-read "NAT")))
    (is (= '(bnat1-set)           (parse-print-read "NAT1")))
    (is (= '(bnatural-set)        (parse-print-read "NATURAL")))
    (is (= '(bnatural1-set)       (parse-print-read "NATURAL1")))
    (is (= '(bint-set)            (parse-print-read "INT")))
    (is (= '(binteger-set)        (parse-print-read "INTEGER"))))
  (testing "some integer operations"
    (are [x y] (= x (parse-print-read y))
      '(b- 1)      "-1"
      '(b+ 1 2)    "1+2"
      '(b- 1 2)    "1-2"
      '(b* 1 2)    "1*2"
      '(bdiv 1 2)  "1 / 2"
      '(b< 1 2)    "1<2"
      '(b> 1 2)    "1>2"
      '(b<= 1 2)   "1<=2"
      '(b>= 1 2)   "1>=2"
      '(bmod 1 2)  "1 mod 2"
      '(b= 1 2)    "1=2"
      '(b** 1 2)   "1 ** 2"
      '(b** 1 (b** 2 3)) "1 ** 2 ** 3"
      '(binterval 1 5)    "1..5"
      '(bmax (bset-enum 1 2)) "max({1,2})"
      '(bmin (bset-enum 1 2)) "min({1,2})"
      '(bsigma [:x] (b< :x 5) (b* :x :x)) "SIGMA(x).(x < 5|x*x)"
      '(bpi [:x] (b< :x 5) (b* :x :x)) "PI(x).(x < 5|x*x)"
      ))
  (testing "logical predicates"
    (are [x y] (= x (parse-print-read y))
      '(band (b= 1 1) (b= 1 2))   "(1=1) & (1=2)"
      '(bor (b= 1 1) (b= 1 2))    "(1=1) or (1=2)"
      '(b<=> (b= 1 1) (b= 1 2))   "(1=1) <=> (1=2)"
      '(b=> (b= 1 1) (b= 1 2))    "(1=1) => (1=2)"
      '(bnot (b= 1 1))            "not(1=1)"
      '(bnot= 1 2)                "1 /= 2"
      '(bforall [:x] (b=> (b= :x 1) (b= :x 1))) "!x.(x=1 => x=1)"
      '(bforall [:x :y] (b=> (b= :x 1) (b= :y 1))) "!x,y.(x=1 => y=1)"
      '(bexists [:x :y] (b=> (b= :x 1) (b= :y 1))) "#x,y.(x=1 => y=1)"
      ))
  (testing "sets operations"
    (are [x y] (= x (parse-print-read y))
      '(b* #{} #{})              "{} * {}"
      '(bunion #{} #{})          "{} \\/ {}"
      '(bintersection #{} #{})   "{} /\\ {}"
;      '(b- #{} #{})              "{} - {}"  ;; FIXME: I don't know if I want this to happen
      '(bmember 1 (bset-enum 1)) "1 : {1}"
      '(bpow (bset-enum 1))      "POW({1})"
      '(bpow1 (bset-enum 1))     "POW1({1})"
      '(bfin (bset-enum 1))      "FIN({1})"
      '(bfin1 (bset-enum 1))     "FIN1({1})"
      '(bsubset #{} (bset-enum 1)) "{} <: {1}"
      '(bsubset-strict #{} (bset-enum 1)) "{} <<: {1}"
      '(bcount (bset-enum 1))    "card({1})"
      '(bunion-pe [:x] (b< 0 :x) (bset-enum :x (b* :x :x))) "UNION(x).(0 < x|{x, x*x})"
      '(bintersection-pe [:x] (b< 0 :x) (bset-enum :x (b* :x :x))) "INTER(x).(0 < x|{x, x*x})"
      '(bunite-sets (bset-enum (bset-enum 1) (bset-enum 2 3))) "union({{1}, {2,3}})"
      '(bintersect-sets (bset-enum (bset-enum 1) (bset-enum 2 3))) "inter({{1}, {2,3}})"
      ))
  (testing "relations"
    (are [x y] (= x (parse-print-read y))
      '(b<-> #{} #{})  "{} <-> {}"
      '(b<| #{} #{})   "{} <| {}"
      '(b<<| #{} #{})  "{} <<| {}"
      '(b|> #{} #{})   "{} |> {}"
      '(b|>> #{} #{})  "{} |>> {}"
      '(b<+ #{} #{})   "{} <+ {}"
      '(b>< #{} #{})   "{} >< {}"
      '(bdom #{})      "dom({})"
      '(bran #{})      "ran({})"
      '(bimage #{} (bset-enum 1)) "{}[{1}]"
      '(binverse #{})      "{}~"
      '(bprj1 (bset-enum 1) (bset-enum 3 4)) "prj1({1}, {3,4})"
      '(bprj2 (bset-enum 1) (bset-enum 3 4)) "prj2({1}, {3,4})"
      ))
  (testing "functions"
    (are [x y] (= x (parse-print-read y))
      '(b+-> #{} #{})   "{} +-> {}"
      '(b--> #{} #{})   "{} --> {}"
      '(b+->> #{} #{})  "{} +->> {}"
      '(b-->> #{} #{})  "{} -->> {}"
      '(b>+> #{} #{})   "{} >+> {}"
      '(b>-> #{} #{})   "{} >-> {}"
      '(b>+>> #{} #{})  "{} >+>> {}"
      '(b>->> #{} #{})  "{} >->> {}"
      '(blambda [:x] (b< :x 2) (b* :x :x)) "%x.(x<2|x*x)"
      '(bapply (bset-enum (bcouple 1 2)) 1) "{1|->2}(1)"
      )))
 
