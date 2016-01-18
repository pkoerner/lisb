(ns lisb.AST2lisb-test
  (:require [clojure.test :refer :all]
            [lisb.AST2lisb :refer :all]))


(deftest pretty-printer-test
  (testing "for example integers"
    (is (= 1 (parse-print-read "1"))))
  (testing "booleans"
    (is (= true  (parse-print-read "TRUE")))
    (is (= false (parse-print-read "FALSE"))))
  (testing "sets"
    (is (= #{}    (parse-print-read "{}")))
    (is (= #{1}   (parse-print-read "{1}"))) 
    (is (= #{1 2} (parse-print-read "{1,2}"))))
    (is (= '#{(bcouple 1 2)} (parse-print-read "{1|->2}"))) 
  (testing "some integer operations"
    (are [x y] (= x (parse-print-read y))
      '(b+ 1 2)    "1+2"
      '(b- 1 2)    "1-2"
      '(b* 1 2)    "1*2"
      '(b< 1 2)    "1<2"
      '(b> 1 2)    "1>2"
      '(b<= 1 2)   "1<=2"
      '(b>= 1 2)   "1>=2"
      '(bmod 1 2)  "1 mod 2"
      '(b= 1 2)    "1=2"
      '(b** 1 2)   "1 ** 2"
      '(b** 1 (b** 2 3)) "1 ** 2 ** 3"))
  (testing "logical predicates"
    (are [x y] (= x (parse-print-read y))
      '(band (b= 1 1) (b= 1 2))   "(1=1) & (1=2)"
      '(bor (b= 1 1) (b= 1 2))    "(1=1) or (1=2)"
      '(b<=> (b= 1 1) (b= 1 2))   "(1=1) <=> (1=2)"
      '(b=> (b= 1 1) (b= 1 2))    "(1=1) => (1=2)"))
  (testing "sets operations"
    (are [x y] (= x (parse-print-read y))
      '(b* #{} #{})        "{} * {}"
      '(bunion #{1} #{2})  "{1} \\/ {2}"
      '(bintersection #{1} #{2})  "{1} /\\ {2}"))
  (testing "relations"
    (are [x y] (= x (parse-print-read y))
      '(b<-> #{} #{})  "{} <-> {}"
      '(b<| #{} #{})   "{} <| {}"
      '(b<<| #{} #{})  "{} <<| {}"
      '(b|> #{} #{})   "{} |> {}"
      '(b|>> #{} #{})  "{} |>> {}"
      '(b<+ #{} #{})   "{} <+ {}"
      '(b>< #{} #{})   "{} >< {}"))
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
 
