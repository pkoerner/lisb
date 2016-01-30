(ns lisb.AST2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.AST2lisb :refer :all]))




(deftest pretty-printer-test
  (testing "for example integers"
    (is (= 1 (bstr->lisb "1" #{}))))
  (testing "booleans"
    (is (= true  (bstr->lisb "TRUE" #{})))
    (is (= false (bstr->lisb "FALSE" #{})))
    (is (= '(lisb.representation/bpred->bool (lisb.representation/b= 1 1)) (bstr->lisb "bool(1=1)" #{})))) 
  (testing "identifiers"
    (is (= '(lisb.representation/b= a :b) (bstr->lisb "a=b" #{"a"}))))
  (testing "sets"
    (are [x y] (= x (bstr->lisb y #{}))
     #{}  "{}"
     '(lisb.representation/bset-enum 1)         "{1}" 
     '(lisb.representation/bset-enum 1 2)       "{1,2}" 
     '(lisb.representation/bset [:x] (lisb.representation/b< :x 5)) "{x|x<5}" 
     '(lisb.representation/btuple 1 2)          "1|->2" 
     '(lisb.representation/bbool-set)            "BOOL" 
     '(lisb.representation/bnat-set)             "NAT" 
     '(lisb.representation/bnat1-set)            "NAT1" 
     '(lisb.representation/bnatural-set)         "NATURAL" 
     '(lisb.representation/bnatural1-set)        "NATURAL1" ))
  (testing "some integer operations"
    (are [x y] (= x (bstr->lisb y #{}))
      '(lisb.representation/bminus 1)      "-1"
      '(lisb.representation/b+ 1 2)    "1+2"
      '(lisb.representation/bminus-or-set-subtract 1 2)    "1-2"
      '(lisb.representation/b* 1 2)    "1*2"
      '(lisb.representation/bdiv 1 2)  "1 / 2"
      '(lisb.representation/b< 1 2)    "1<2"
      '(lisb.representation/b> 1 2)    "1>2"
      '(lisb.representation/b<= 1 2)   "1<=2"
      '(lisb.representation/b>= 1 2)   "1>=2"
      '(lisb.representation/bmod 1 2)  "1 mod 2"
      '(lisb.representation/b= 1 2)    "1=2"
      '(lisb.representation/b** 1 2)   "1 ** 2"
      '(lisb.representation/b** 1 (lisb.representation/b** 2 3)) "1 ** 2 ** 3"
      '(lisb.representation/binterval 1 5)    "1..5"
      '(lisb.representation/bmax (lisb.representation/bset-enum 1 2)) "max({1,2})"
      '(lisb.representation/bmin (lisb.representation/bset-enum 1 2)) "min({1,2})"
      '(lisb.representation/bsigma [:x] (lisb.representation/b< :x 5) (lisb.representation/b* :x :x)) "SIGMA(x).(x < 5|x*x)"
      '(lisb.representation/bpi [:x] (lisb.representation/b< :x 5) (lisb.representation/b* :x :x)) "PI(x).(x < 5|x*x)"
      ))
  (testing "logical predicates"
    (are [x y] (= x (bstr->lisb y #{}))
      '(lisb.representation/band (lisb.representation/b= 1 1) (lisb.representation/b= 1 2))   "(1=1) & (1=2)"
      '(lisb.representation/bor (lisb.representation/b= 1 1) (lisb.representation/b= 1 2))    "(1=1) or (1=2)"
      '(lisb.representation/b<=> (lisb.representation/b= 1 1) (lisb.representation/b= 1 2))   "(1=1) <=> (1=2)"
      '(lisb.representation/b=> (lisb.representation/b= 1 1) (lisb.representation/b= 1 2))    "(1=1) => (1=2)"
      '(lisb.representation/bnot (lisb.representation/b= 1 1))            "not(1=1)"
      '(lisb.representation/bnot= 1 2)                "1 /= 2"
      '(lisb.representation/bforall [:x] (lisb.representation/b=> (lisb.representation/b= :x 1) (lisb.representation/b= :x 1))) "!x.(x=1 => x=1)"
      '(lisb.representation/bforall [:x :y] (lisb.representation/b=> (lisb.representation/b= :x 1) (lisb.representation/b= :y 1))) "!x,y.(x=1 => y=1)"
      '(lisb.representation/bexists [:x :y] (lisb.representation/b=> (lisb.representation/b= :x 1) (lisb.representation/b= :y 1))) "#x,y.(x=1 => y=1)"
      ))
  (testing "sets operations"
    (are [x y] (= x (bstr->lisb y #{}))
      '(lisb.representation/b* #{} #{})              "{} * {}"
      '(lisb.representation/bunion #{} #{})          "{} \\/ {}"
      '(lisb.representation/bintersection #{} #{})   "{} /\\ {}"
      '(lisb.representation/bminus-or-set-subtract #{} #{})              "{} - {}"  ;; FIXME: I don't know if I want this to happen
      '(lisb.representation/bmember 1 (lisb.representation/bset-enum 1)) "1 : {1}"
      '(lisb.representation/bpow (lisb.representation/bset-enum 1))      "POW({1})"
      '(lisb.representation/bpow1 (lisb.representation/bset-enum 1))     "POW1({1})"
      '(lisb.representation/bfin (lisb.representation/bset-enum 1))      "FIN({1})"
      '(lisb.representation/bfin1 (lisb.representation/bset-enum 1))     "FIN1({1})"
      '(lisb.representation/bsubset #{} (lisb.representation/bset-enum 1)) "{} <: {1}"
      '(lisb.representation/bsubset-strict #{} (lisb.representation/bset-enum 1)) "{} <<: {1}"
      '(lisb.representation/bcount (lisb.representation/bset-enum 1))    "card({1})"
      '(lisb.representation/bunion-pe [:x] (lisb.representation/b< 0 :x) (lisb.representation/bset-enum :x (lisb.representation/b* :x :x))) "UNION(x).(0 < x|{x, x*x})"
      '(lisb.representation/bintersection-pe [:x] (lisb.representation/b< 0 :x) (lisb.representation/bset-enum :x (lisb.representation/b* :x :x))) "INTER(x).(0 < x|{x, x*x})"
      '(lisb.representation/bunite-sets (lisb.representation/bset-enum (lisb.representation/bset-enum 1) (lisb.representation/bset-enum 2 3))) "union({{1}, {2,3}})"
      '(lisb.representation/bintersect-sets (lisb.representation/bset-enum (lisb.representation/bset-enum 1) (lisb.representation/bset-enum 2 3))) "inter({{1}, {2,3}})"
      ))
  (testing "relations"
    (are [x y] (= x (bstr->lisb y #{}))
      '(lisb.representation/b<-> #{} #{})  "{} <-> {}"
      '(lisb.representation/b<| #{} #{})   "{} <| {}"
      '(lisb.representation/b<<| #{} #{})  "{} <<| {}"
      '(lisb.representation/b|> #{} #{})   "{} |> {}"
      '(lisb.representation/b|>> #{} #{})  "{} |>> {}"
      '(lisb.representation/b<+ #{} #{})   "{} <+ {}"
      '(lisb.representation/b>< #{} #{})   "{} >< {}"
      '(lisb.representation/bdom #{})      "dom({})"
      '(lisb.representation/bran #{})      "ran({})"
      '(lisb.representation/bimage #{} (lisb.representation/bset-enum 1)) "{}[{1}]"
      '(lisb.representation/binverse #{})      "{}~"
      '(lisb.representation/bprj1 (lisb.representation/bset-enum 1) (lisb.representation/bset-enum 3 4)) "prj1({1}, {3,4})"
      '(lisb.representation/bprj2 (lisb.representation/bset-enum 1) (lisb.representation/bset-enum 3 4)) "prj2({1}, {3,4})"
      ))
  (testing "functions"
    (are [x y] (= x (bstr->lisb y #{}))
      '(lisb.representation/b+-> #{} #{})   "{} +-> {}"
      '(lisb.representation/b--> #{} #{})   "{} --> {}"
      '(lisb.representation/b+->> #{} #{})  "{} +->> {}"
      '(lisb.representation/b-->> #{} #{})  "{} -->> {}"
      '(lisb.representation/b>+> #{} #{})   "{} >+> {}"
      '(lisb.representation/b>-> #{} #{})   "{} >-> {}"
      '(lisb.representation/b>+>> #{} #{})  "{} >+>> {}"
      '(lisb.representation/b>->> #{} #{})  "{} >->> {}"
      '(lisb.representation/blambda [:x] (lisb.representation/b< :x 2) (lisb.representation/b* :x :x)) "%x.(x<2|x*x)"
      '(lisb.representation/bapply (lisb.representation/bset-enum (lisb.representation/btuple 1 2)) 1) "{1|->2}(1)"
      ))
  (testing "multiple arity"
    (are [x y] (= x (bstr->lisb y #{}))
      '(lisb.representation/band (lisb.representation/b= 1 1)
                                 (lisb.representation/b= 2 2)
                                 (lisb.representation/b= 3 3)
                                 (lisb.representation/b= 4 4)) "1=1 & 2=2 & 3=3 & 4=4")))
