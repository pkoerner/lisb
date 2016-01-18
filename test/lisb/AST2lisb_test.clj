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
  (testing "sets"
    (is (= #{}    (parse-print-read "{}")))
    (is (= '(bset-enum 1)    (parse-print-read "{1}"))) 
    (is (= '(bset-enum 1 2)  (parse-print-read "{1,2}")))
    (is (= '(bcouple 1 2)    (parse-print-read "1|->2")))
    (is (= '(bbool-set)      (parse-print-read "BOOL")))
    (is (= '(bnat-set)       (parse-print-read "NAT")))
    (is (= '(bnat1-set)      (parse-print-read "NAT1")))
    (is (= '(bnatural-set)   (parse-print-read "NATURAL")))
    (is (= '(bnatural1-set)  (parse-print-read "NATURAL1")))
    (is (= '(bint-set)       (parse-print-read "INT")))
    (is (= '(binteger-set)   (parse-print-read "INTEGER"))))
  (testing "some integer operations"
    (are [x y] (= x (parse-print-read y))
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
      '(b** 1 (b** 2 3)) "1 ** 2 ** 3")
      '(bmax (benum-set 1 2)) "max({1,2})"
      '(bmin (benum-set 1 2)) "min({1,2})"
    )
  (testing "logical predicates"
    (are [x y] (= x (parse-print-read y))
      '(band (b= 1 1) (b= 1 2))   "(1=1) & (1=2)"
      '(bor (b= 1 1) (b= 1 2))    "(1=1) or (1=2)"
      '(b<=> (b= 1 1) (b= 1 2))   "(1=1) <=> (1=2)"
      '(b=> (b= 1 1) (b= 1 2))    "(1=1) => (1=2)"
      '(bnot (b= 1 1))            "not(1=1)"
      ))
  (testing "sets operations"
    (are [x y] (= x (parse-print-read y))
      '(b* #{} #{})              "{} * {}"
      '(bunion #{} #{})          "{} \\/ {}"
      '(bintersection #{} #{})   "{} /\\ {}"
      '(bmember 1 (bset-enum 1)) "1 : {1}"
      '(bpow (bset-enum 1))      "POW({1})"
      '(bpow1 (bset-enum 1))     "POW1({1})"
      '(bfin (bset-enum 1))      "FIN({1})"
      '(bfin1 (bset-enum 1))     "FIN1({1})"
      '(bsubset #{} (bset-enum 1)) "{} <: {1}"
      '(bsubset-strict #{} (bset-enum 1)) "{} <<: {1}"
      '(bcount (bset-enum 1))    "card({1})"
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
      '(b>->> #{} #{})  "{} >->> {}")))
 
