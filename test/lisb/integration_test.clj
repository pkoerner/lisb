(ns lisb.integration-test
  (:require [clojure.test :refer :all]
            [lisb.core :refer [eval-ir-formula ir-state-space!]]
            [lisb.translation.util :refer :all]))

(deftest integration
  (testing "no exception is thrown when executing implemented command;
            this test only checks that a valid (i.e. accepted) AST is constructed
            without concern about semantics
            for now only 'truish' predicates are evaluated because eval-lisb-predicate cannot handle
            ComputationNotCompletedResults yet"
    (is (eval-ir-formula (b= 1 1)))
    (is (eval-ir-formula (b= :x 1)))
    (is (eval-ir-formula (b= :x true)))
    (is (eval-ir-formula (b= :x false)))

    (is (eval-ir-formula (bnot= 0 1)))

    (is (eval-ir-formula (bdistinct? 0 1)))
    (is (eval-ir-formula (bdistinct? 0 1 2)))
    (is (eval-ir-formula (bnot (bdistinct? 0 1 1))))

    (is (eval-ir-formula (b< 1 2)))
    (is (eval-ir-formula (b< 1 2 3)))

    (is (eval-ir-formula (b> 2 1)))
    (is (eval-ir-formula (b> 3 2 1)))

    (is (eval-ir-formula (b<= 1 2)))
    (is (eval-ir-formula (b<= 1 1 3)))

    (is (eval-ir-formula (b>= 2 1)))
    (is (eval-ir-formula (b>= 3 3 1)))

    (is (eval-ir-formula (b< 1 (b+ 1 2))))
    (is (eval-ir-formula (b< 1 (b+ 1 2 3))))

    (is (eval-ir-formula (b< 1 (b- 10 2))))
    (is (eval-ir-formula (b< 1 (b- 10 2 3))))

    (is (eval-ir-formula (b= 6 (b* 2 3))))
    (is (eval-ir-formula (b= 24 (b* 2 3 4))))

    (is (eval-ir-formula (b= 6 (bdiv 24 4))))
    (is (eval-ir-formula (b= 2 (bdiv 24 4 3))))

    (is (eval-ir-formula (band (b< 1 2) (b< 2 3))))
    (is (eval-ir-formula (band (b< 1 2) (b< 2 3) (b< 3 4))))

    (is (eval-ir-formula (bor (b< 1 2) (b< 2 3))))
    (is (eval-ir-formula (bor (b< 1 2) (b< 2 3) (b< 3 4))))

    (is (eval-ir-formula (bequivalence (b< 1 2) (b< 2 3))))
    (is (eval-ir-formula (bequivalence (b< 1 2) (b< 2 3) (b< 3 4))))

    (is (eval-ir-formula (bnot (b< 2 1))))

    (is (eval-ir-formula (b= true (bpred->bool (b< 1 2)))))

    (is (eval-ir-formula (b= :x #{1 2})))
    (is (eval-ir-formula (b= :x #{1 2 (b+ 1 2)})))

    (is (eval-ir-formula (b= #{1 2 3} (bcomprehension-set [:x] (b< 0 :x 4)))))

    (is (eval-ir-formula (b= :x (bpow #{1 2}))))

    (is (eval-ir-formula (b= :x (bpow1 #{1 2}))))

    (is (eval-ir-formula (b= :x (bfin #{1 2}))))

    (is (eval-ir-formula (b= :x (bfin1 #{1 2}))))

    (is (eval-ir-formula (b= 2 (bcard #{1 2}))))

    (is (eval-ir-formula (b= #{} (bcartesian-product #{} #{}))))

    ;; FIXME: without the additional set the translator breaks
    (is (eval-ir-formula (b= :x #{(bcartesian-product #{1 2} #{2 3})})))

    (is (eval-ir-formula (b= :x #{(bcartesian-product #{1 2} #{2 3} #{3 4})})))

    (is (eval-ir-formula (b= #{1 2 3} (bunion #{1 2} #{2 3}))))
    (is (eval-ir-formula (b= #{1 2 3 4} (bunion #{1 2} #{2 3} #{3 4}))))

    (is (eval-ir-formula (b= #{1 2 3 4} (bunite-sets #{#{1 2} #{2 3} #{3 4}}))))

    (is (eval-ir-formula (b= #{1 2 3 4 9 16} (bunion-pe [:x] (b< 0 :x 5) #{:x (b* :x :x)}))))

    (is (eval-ir-formula (b= #{2} (bintersection #{1 2} #{2 3}))))
    (is (eval-ir-formula (b= #{3 4} (bintersection #{1 2 3 4} #{2 3 4} #{3 4}))))

    (is (eval-ir-formula (b= #{3 4} (bintersect-sets #{#{1 2 3 4} #{2 3 4} #{3 4}}))))

    (is (eval-ir-formula (b= #{} (bintersection-pe [:x] (b< 0 :x 5) #{:x (b* :x :x)}))))

    (is (eval-ir-formula (b= #{1} (bset- #{1 2} #{2 3}))))
    (is (eval-ir-formula (b= #{1} (bset- #{1 2} #{2 3} #{3 4}))))

    (is (eval-ir-formula (bmember? 1 #{1})))
    (is (eval-ir-formula (bmember? 1 #{1})))

    (is (eval-ir-formula (bcontains? #{1 2 3} 1)))

    (is (eval-ir-formula (bsubset? #{1} #{1 2})))

    (is (eval-ir-formula (bsuperset? #{1 2} #{1})))

    (is (eval-ir-formula (bstrict-subset? #{1} #{1 2})))

    (is (eval-ir-formula (bstrict-superset? #{1 2} #{1})))

    (is (eval-ir-formula (bmember? true bbool-set)))
    (is (eval-ir-formula (bmember? 0 bnatural-set)))
    (is (eval-ir-formula (bmember? 1 bnatural1-set)))
    (is (eval-ir-formula (bmember? -1 bint-set)))
    (is (eval-ir-formula (bmember? 0 bnat-set)))
    (is (eval-ir-formula (bmember? 1 bnat1-set)))

    (is (eval-ir-formula (b= 3 (bmax #{1 2 3}))))
    (is (eval-ir-formula (b= 3 (bmax 1 2 3))))
    (is (eval-ir-formula (b= 3 (bmax 2 3))))

    (is (eval-ir-formula (b= 1 (bmin #{1 2 3}))))
    (is (eval-ir-formula (b= 1 (bmin 1 2 3))))
    (is (eval-ir-formula (b= 2 (bmin 2 3))))

    (is (eval-ir-formula (b= 2 (bmod 5 3))))

    (is (eval-ir-formula (b= 2 (bsuccessor 1))))

    (is (eval-ir-formula (b= 0 (bpredecessor 1))))

    (is (eval-ir-formula (b= :x #{(bmaplet 1 2)})))

    (is (eval-ir-formula (bmember? :x (brelation #{1 2} #{3 4}))))
    (is (eval-ir-formula (bmember? :x (brelation #{1 2} #{3 4} #{5 6}))))

    (is (eval-ir-formula (b= #{1 2} (bdom #{(bmaplet 1 0) (bmaplet 1 1) (bmaplet 2 42)}))))

    (is (eval-ir-formula (b= #{0 1 42} (bran #{(bmaplet 1 0) (bmaplet 1 1) (bmaplet 2 42)}))))

    (is (eval-ir-formula (b= #{(bmaplet 1 1)} (bid #{1}))))

    (is (eval-ir-formula (b= #{(bmaplet 1 1)} (bdomain-restriction #{1 2} #{(bmaplet 1 1) (bmaplet 3 0)}))))

    (is (eval-ir-formula (b= #{(bmaplet 3 0)} (bdomain-subtraction #{1 2} #{(bmaplet 1 1) (bmaplet 3 0)}))))

    (is (eval-ir-formula (b= #{(bmaplet 1 1)} (brange-restriction #{(bmaplet 1 1) (bmaplet 3 0)} #{1 2}))))

    (is (eval-ir-formula (b= #{(bmaplet 3 0)} (brange-subtraction #{(bmaplet 1 1) (bmaplet 3 0)} #{1 2}))))

    (is (eval-ir-formula (b= #{(bmaplet 3 0)} (binverse #{(bmaplet 0 3)}))))

    (is (eval-ir-formula (b= #{2} (bimage #{(bmaplet 1 2) (bmaplet 0 3)} #{1 2}))))

    (is (eval-ir-formula (b= #{(bmaplet 0 0)} (boverride #{(bmaplet 0 1)} #{(bmaplet 0 0)}))))
    (is (eval-ir-formula (b= #{(bmaplet 0 0) (bmaplet 1 0)} (boverride #{(bmaplet 0 1)} #{(bmaplet 0 0)} #{(bmaplet 1 0)}))))

    (is (eval-ir-formula (b= #{(bmaplet 0 (bmaplet 1 1)) (bmaplet 0 (bmaplet 2 1))} (bdirect-product #{(bmaplet 0 1) (bmaplet 0 2)} #{(bmaplet 0 1)}))))
    (is (eval-ir-formula (b= #{(bmaplet 0 (bmaplet (bmaplet 1 1) 3)) (bmaplet 0 (bmaplet (bmaplet 2 1) 3))} (bdirect-product #{(bmaplet 0 1) (bmaplet 0 2)} #{(bmaplet 0 1)} #{(bmaplet 0 3)}))))


    (is (eval-ir-formula (b= #{(bmaplet 0 0)} (bcomposition #{(bmaplet 0 1)} #{(bmaplet 1 0)}))))
    (is (eval-ir-formula (b= #{(bmaplet 0 5)} (bcomposition #{(bmaplet 0 1)} #{(bmaplet 1 2)} #{(bmaplet 2 5)}))))

    (is (eval-ir-formula (b= #{(bmaplet (bmaplet 0 0) (bmaplet 1 1)) (bmaplet (bmaplet 0 0) (bmaplet 2 1))} (bparallel-product #{(bmaplet 0 1) (bmaplet 0 2)} #{(bmaplet 0 1)}))))
    (is (eval-ir-formula (b= :a (bparallel-product #{(bmaplet 0 1) (bmaplet 0 2)} #{(bmaplet 0 1)} #{(bmaplet 1 1)})))) ;; if it works, that's good enough for me

    (is (eval-ir-formula (b= #{(bmaplet (bmaplet 1 3) 1) (bmaplet (bmaplet 1 4) 1)} (bprj1 #{1} #{3, 4}))))
    (is (eval-ir-formula (b= #{(bmaplet (bmaplet 1 3) 3) (bmaplet (bmaplet 1 4) 4)} (bprj2 #{1} #{3, 4}))))

    ; TODO: An enumeration warning was observed
    ;(is (eval-lisb-predicate ( (b= #{[1 1] [1 2] [2 2]} (bclosure #{[1 2]})))))
    ;(is (eval-lisb-predicate ( (b= #{[1 2]} (bclosure1 #{[1 2]})))))

    (is (eval-ir-formula (b= #{(bmaplet 1 3) (bmaplet 4 2) (bmaplet 5 3)} (biterate #{(bmaplet 1 2) (bmaplet 2 3) (bmaplet 4 5) (bmaplet 5 2)} 2))))

    (is (eval-ir-formula (b= #{(bmaplet 1 #{2 3}) (bmaplet 3 #{4})} (bfnc #{(bmaplet 1 2) (bmaplet 1 3) (bmaplet 3 4)}))))

    (is (eval-ir-formula (b= #{(bmaplet 1 2) (bmaplet 1 3) (bmaplet 3 4)} (brel #{(bmaplet 1 #{2 3}) (bmaplet 3 #{4})}))))

    (is (eval-ir-formula (b= #{#{} #{(bmaplet 1 2)} #{(bmaplet 1 2) (bmaplet 2 2)} #{(bmaplet 2 2)}} (bpartial-function #{1 2} #{2}))))
    ; TODO: de.hhu.stups.prob.translator.exceptions.TranslationException: de.be4.claicalb.core.parser.exceptions.BCompoundException: Invalid combination of symbols: '(' and '..'. Argument to binary operator is miing.
    ;(is (eval-lisb-predicate ( (b= :a #{(b+-> #{1 2} #{2} #{3 4})}))))

    (is (eval-ir-formula (b= #{#{(bmaplet 1 2) (bmaplet 2 2)}} (btotal-function #{1 2} #{2}))))
    (is (eval-ir-formula (b= #{#{(bmaplet #{(bmaplet 1 2) (bmaplet 2 2)} 3)}} (btotal-function #{1 2} #{2} #{3}))))

    (is (eval-ir-formula (b= #{#{(bmaplet 1 2)} #{(bmaplet 1 2) (bmaplet 2 2)} #{(bmaplet 2 2)}} (bpartial-surjection #{1 2} #{2}))))
    (is (eval-ir-formula (b= :a #{(bpartial-surjection #{1 2} #{2} #{3 4})})))

    (is (eval-ir-formula (b= #{#{(bmaplet 1 2) (bmaplet 2 2)}} (btotal-surjection #{1 2} #{2}))))
    (is (eval-ir-formula (b= #{} (btotal-surjection #{1 2} #{2} #{3 4}))))

    (is (eval-ir-formula (b= #{#{} #{(bmaplet 1 3)} #{(bmaplet 1 4)}} (bpartial-injection #{1} #{3 4}))))
    (is (eval-ir-formula (b= #{#{} #{(bmaplet #{} 2)} #{(bmaplet #{(bmaplet 1 3)} 2)} #{(bmaplet #{(bmaplet 1 4)} 2)}} (bpartial-injection #{1} #{3 4} #{2}))))

    (is (eval-ir-formula (b= #{#{(bmaplet 1 3)} #{(bmaplet 1 4)}} (btotal-injection #{1} #{3 4}))))
    (is (eval-ir-formula (b= :a #{(btotal-injection #{1} #{3 4} #{5 6})})))

    (is (eval-ir-formula (b= #{#{(bmaplet 1 3) (bmaplet 2 4)} #{(bmaplet 1 4) (bmaplet 2 3)}} (bpartial-bijection #{1 2} #{3 4}))))
    (is (eval-ir-formula (b= #{#{(bmaplet #{(bmaplet 1 3) (bmaplet 2 4)} 5)} #{(bmaplet #{(bmaplet 1 4) (bmaplet 2 3)} 5)}} (bpartial-bijection #{1 2} #{3 4} #{5}))))

    (is (eval-ir-formula (b= #{#{(bmaplet 1 3) (bmaplet 2 4)} #{(bmaplet 1 4) (bmaplet 2 3)}} (btotal-bijection #{1 2} #{3 4}))))
    (is (eval-ir-formula (b= :a #{(btotal-bijection #{1 2} #{3 4} #{5 6})})))

    (is (eval-ir-formula (b= #{(bmaplet 3 9) (bmaplet 4 16)} (blambda [:x] (b< 2 :x 5) (b* :x :x)))))

    (is (eval-ir-formula (b= 2 (bfn-call #{(bmaplet 1 2)} 1))))
    (is (eval-ir-formula (b= 3 (bfn-call #{(bmaplet (bmaplet 1 2) 3)} 1 2))))
    (is (eval-ir-formula (b= 3 (bfn-call #{(bmaplet (bmaplet 1 2) 3)} (bmaplet 1 2)))))

    (is (eval-ir-formula (bimplication (b= true true) (b= true true))))
    (is (eval-ir-formula (bimplication (b= true true) (b= true false) (b= true true))))

    (is (eval-ir-formula (bfor-all [:x] (bmember? :x bbool-set) (b= true true))))
    (is (eval-ir-formula (bfor-all [:x :y] (b< 0 :x :y 3) (b<= (bsuccessor :x) :y))))

    (is (eval-ir-formula (b= (binterval 1 5) #{1 2 3 4 5})))

    (is (eval-ir-formula (bexists [:x :y] (band (bcontains? bint-set :x) (bcontains? bint-set :y) (b< :x :y)))))

    (is (eval-ir-formula (b= (bsequence 3 2 1) #{(bmaplet 1 3) (bmaplet 2 2) (bmaplet 3 1)})))
    (is (eval-ir-formula (b= (bsequence) #{})))

    (is (eval-ir-formula (bmember? (bsequence (bmaplet 3 3) (bmaplet 2 2) (bmaplet 1 4)) (biseq (bsequence 4 2 3)))))
    (is (eval-ir-formula (bmember? (bsequence (bmaplet 3 3) (bmaplet 2 2) (bmaplet 1 4)) (biseq1 (bsequence 4 2 3)))))

    (is (eval-ir-formula (b= #{#{(bmaplet 1 (bmaplet 1 4)) (bmaplet 2 (bmaplet 2 2))} #{(bmaplet 1 (bmaplet 2 2)) (bmaplet 2 (bmaplet 1 4))}} (bperm (bsequence 4 2)))))

    (is (eval-ir-formula (b= (bsequence 3 4 1 5) (bconcat (bsequence 3 4) (bsequence 1 5)))))
    (is (eval-ir-formula (b= (bsequence 3 4 1 5 2) (bconcat (bsequence 3 4) (bsequence 1 5) (bsequence 2)))))

    (is (eval-ir-formula (b= (bsequence 3 1 4) (bprepend 3 (bsequence 1 4)))))

    (is (eval-ir-formula (b= (bsequence 3 1 4) (bappend (bsequence 3 1) 4))))
    (is (eval-ir-formula (b= (bsequence 3 1 4) (bappend (bsequence 3) 1 4))))

    (is (eval-ir-formula (b= (breverse (bsequence 3 1 4)) (bsequence 4 1 3))))

    (is (eval-ir-formula (b= (bfirst (bsequence 3 1 4)) 3)))
    (is (eval-ir-formula (b= (blast (bsequence 3 1 4)) 4)))

    (is (eval-ir-formula (b= (bfront (bsequence 3 1 4)) (bsequence 3 1))))
    (is (eval-ir-formula (b= (btail (bsequence 3 1 4)) (bsequence 1 4))))

    (is (eval-ir-formula (b= (btake 3 (bsequence 3 1 4 1 5)) (bsequence 3 1 4))))
    (is (eval-ir-formula (b= (bdrop 3 (bsequence 3 1 4 1 5)) (bsequence 1 5))))

    (is (eval-ir-formula (b= (b** 2 3) 8)))
    (is (eval-ir-formula (b= (b** 2 2 3) 256)))

    (is (eval-ir-formula (b= (bsigma [:a] (b<= 1 :a 4) :a) 10)))
    (is (eval-ir-formula (b= (bpi [:a] (b<= 1 :a 4) :a) 24)))

    (is (eval-ir-formula (bmember? (bsequence 0 2 2 0 1 0 0) (bseq #{0 1 2}))))
    (is (eval-ir-formula (bmember? #{} (bseq #{0 1 2}))))

    (is (eval-ir-formula (bmember? (bsequence 0 2 2 0 1 0 0) (bseq1 #{0 1 2}))))
    (is (eval-ir-formula (bnot (bmember? #{} (bseq1 #{0 1 2})))))

    (is (eval-ir-formula (b= (bsequence 3 1 4 1 5 9 2) (bconc (bsequence (bsequence 3 1 4) (bsequence 1 5 9 2))))))

    (is (eval-ir-formula (b= :x (bstruct :x #{1 2 3}))))

    (is (eval-ir-formula (bmember? (brecord :x 1) (bstruct :x #{1 2 3}))))

    (is (eval-ir-formula (b= 1 (brecord-get (brecord :x 1) :x))))

    (is (eval-ir-formula (b= :x "foo")))
    (is (eval-ir-formula (bmember? "foo" bstring-set)))

    ))

(deftest integration-real-test
  (testing "implementation of real numbers is working"
    (is (eval-ir-formula (b= :x (b+ 3.14 3.15))))
    (is (eval-ir-formula (b= 6.29 (b+ 3.14 3.15))))
    (is (eval-ir-formula (b= :x (bto-real 3))))
    (is (eval-ir-formula (b= (bto-real 3) (float 3))))
    (is (eval-ir-formula (bmember? 3.14 breal-set)))
    (is (eval-ir-formula (b= (bceil 3.14) 4)))
    (is (eval-ir-formula (b= (bfloor 3.14) 3)))))

(deftest fancy-fns-test
  (testing "fancier functions"
    (is (eval-ir-formula (b= #{2 3 4} (bmap-set (pred [x] (+ 1 x)) #{1 2 3}))))
    (is (eval-ir-formula (b= (bif (b< 1 2) 3 4) 3)))
    (is (eval-ir-formula (b= (bif (b> 1 2) 3 4) 4)))
    (is (eval-ir-formula (b= (brange 1 5) #{1 2 3 4})))
    (is (eval-ir-formula (eval `(b ~(ast->lisb (b-predicate->ast "1<2"))))))
    (is (eval-ir-formula (b= 2 (eval `(b ~(ast->lisb (b-expression->ast "1+1")))))))
    ;(is (eval-ir-formula (b= :x (bcall "CHOOSE" #{1 2 3})))) TODO: approach when definitions are implemented
    (is (eval-ir-formula (blet [:foo 1] (b< :foo 2))))
    (is (eval-ir-formula (blet [:foo 1 :bar 2] (b< :foo :bar))))
    (is (eval-ir-formula (b= 1 (blet [:foo 1] :foo))))
    (is (eval-ir-formula (b= 3 (blet [:foo 1 :bar 2] (b+ :foo :bar)))))
    (is (eval-ir-formula (b= -1 (b- 1 2))))
    (is (eval-ir-formula (b= #{1} (bset- #{1 2} #{2}))))))


(deftest b-haviour
  (testing "satisfiable predicates should have truthy return values"
    (is (eval-ir-formula (b (= 1 1))))
    (is (eval-ir-formula (b (= :x 1)))))
  (testing "the return values should be maps"
    (is (= {} (eval-ir-formula (b (= 1 1)))))
    (is (= {"x" 1} (eval-ir-formula (b (= :x 1))))))
  ;; TODO: test concerning the resulting data structures, i.e. sets, sequences, tuples...
  (testing "unsatisfiable predicates yield a nil value"
    (is (nil? (eval-ir-formula (b (= 1 2))))))
  (testing "timeouts / no found solution should throw an exception"
    (is (thrown? Exception (eval-ir-formula (b (< :x :y :x)))))))

