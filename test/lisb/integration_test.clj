(ns lisb.integration-test
  (:require [clojure.test :refer :all])
  (:require [lisb.core :refer [eval state-space]])
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]]))

(defonce te-ss-t (state-space))

(deftest integration
  (testing "no exception is thrown when executing implemented command;
            this test only checks that a valid (i.e. accepted) AST is constructed
            without concern about semantics
            for now only 'truish' predicates are evaluated because eval cannot handle
            ComputationNotCompletedResults yet"
    (let [ss te-ss-t]
      (is (eval ss (to-ast (b= 1 1))))
      (is (eval ss (to-ast (b= :x 1))))
      (is (eval ss (to-ast (b= 1 1 1))))
      (is (eval ss (to-ast (b= :x true))))
      (is (eval ss (to-ast (b= :x false))))

      (is (eval ss (to-ast (bnot= :x 1))))
      (is (eval ss (to-ast (bnot= :x 1 2))))

      (is (eval ss (to-ast (b< 1 2))))
      (is (eval ss (to-ast (b< 1 2 3))))

      (is (eval ss (to-ast (b> 2 1))))
      (is (eval ss (to-ast (b> 3 2 1))))

      (is (eval ss (to-ast (b<= 1 2))))
      (is (eval ss (to-ast (b<= 1 1 3))))

      (is (eval ss (to-ast (b>= 2 1))))
      (is (eval ss (to-ast (b>= 3 3 1))))

      (is (eval ss (to-ast (b< 1 (b+ 1 2)))))
      (is (eval ss (to-ast (b< 1 (b+ 1 2 3)))))

      (is (eval ss (to-ast (b< 1 (b- 10 2)))))
      (is (eval ss (to-ast (b< 1 (b- 10 2 3)))))

      (is (eval ss (to-ast (b= 6 (b* 2 3)))))
      (is (eval ss (to-ast (b= 24 (b* 2 3 4)))))

      (is (eval ss (to-ast (b= 6 (bdiv 24 4)))))
      (is (eval ss (to-ast (b= 2 (bdiv 24 4 3)))))

      (is (eval ss (to-ast (band (b< 1 2) (b< 2 3)))))
      (is (eval ss (to-ast (band (b< 1 2) (b< 2 3) (b< 3 4)))))

      (is (eval ss (to-ast (bor (b< 1 2) (b< 2 3)))))
      (is (eval ss (to-ast (bor (b< 1 2) (b< 2 3) (b< 3 4)))))

      (is (eval ss (to-ast (b<=> (b< 1 2) (b< 2 3)))))
      (is (eval ss (to-ast (b<=> (b< 1 2) (b< 2 3) (b< 3 4)))))

      (is (eval ss (to-ast (bnot (b< 2 1)))))

      (is (eval ss (to-ast (b= true (bpred->bool (b< 1 2))))))

      (is (eval ss (to-ast (b= :x #{1 2}))))
      (is (eval ss (to-ast (b= :x #{1 2 (b+ 1 2)}))))

      (is (eval ss (to-ast (b= #{1 2 3} (bset [:x] (b< 0 :x 4))))))
      
      (is (eval ss (to-ast (b= :x (bpow #{1 2})))))

      (is (eval ss (to-ast (b= :x (bpow1 #{1 2})))))

      (is (eval ss (to-ast (b= :x (bfin #{1 2})))))

      (is (eval ss (to-ast (b= :x (bfin1 #{1 2})))))

      (is (eval ss (to-ast (b= 2 (bcount #{1 2})))))

      (is (eval ss (to-ast (b= #{} (b* #{} #{})))))

      ;; FIXME: without the additional set the translator breaks
      (is (eval ss (to-ast (b= :x #{(b* #{1 2} #{2 3})}))))

      (is (eval ss (to-ast (b= :x #{(b* #{1 2} #{2 3} #{3 4})}))))

      (is (eval ss (to-ast (b= #{1 2 3} (bunion #{1 2} #{2 3})))))
      (is (eval ss (to-ast (b= #{1 2 3 4} (bunion #{1 2} #{2 3} #{3 4})))))

      (is (eval ss (to-ast (b= #{1 2 3 4} (bunite-sets #{#{1 2} #{2 3} #{3 4}})))))

      (is (eval ss (to-ast (b= #{1 2 3 4 9 16} (bunion-pe [:x] (b< 0 :x 5) #{:x (b* :x :x)})))))

      (is (eval ss (to-ast (b= #{2} (bintersection #{1 2} #{2 3})))))
      (is (eval ss (to-ast (b= #{3 4} (bintersection #{1 2 3 4} #{2 3 4} #{3 4})))))

      (is (eval ss (to-ast (b= #{3 4} (bintersect-sets #{#{1 2 3 4} #{2 3 4} #{3 4}})))))

      (is (eval ss (to-ast (b= #{} (bintersection-pe [:x] (b< 0 :x 5) #{:x (b* :x :x)})))))

      (is (eval ss (to-ast (b= #{1} (bset- #{1 2} #{2 3})))))
      (is (eval ss (to-ast (b= #{1} (bset- #{1 2} #{2 3} #{3 4})))))

      (is (eval ss (to-ast (bmember 1 #{1}))))
      (is (eval ss (to-ast (bmember 1 #{1} #{1 2}))))

      (is (eval ss (to-ast (bmembers #{1 2 3} 1))))
      (is (eval ss (to-ast (bmembers #{1 2 3} 1 2))))

      (is (eval ss (to-ast (bsubset #{1} #{1 2}))))
      (is (eval ss (to-ast (bsubset #{1} #{1 2} #{1 2 3}))))

      (is (eval ss (to-ast (bsuperset #{1 2} #{1}))))
      (is (eval ss (to-ast (bsuperset #{1 2 3} #{1 2} #{1}))))

      (is (eval ss (to-ast (bsubset-strict #{1} #{1 2}))))
      (is (eval ss (to-ast (bsubset-strict #{1} #{1 2} #{1 2 3}))))

      (is (eval ss (to-ast (bsuperset-strict #{1 2} #{1}))))
      (is (eval ss (to-ast (bsuperset-strict #{1 2 3} #{1 2} #{1}))))

      (is (eval ss (to-ast (bmember true (bbool-set)))))
      (is (eval ss (to-ast (bmember 0 (bnatural-set)))))
      (is (eval ss (to-ast (bmember 1 (bnatural1-set)))))
      (is (eval ss (to-ast (bmember -1 (bint-set)))))
      (is (eval ss (to-ast (bmember 0 (bnat-set)))))
      (is (eval ss (to-ast (bmember 1 (bnat1-set)))))

      (is (eval ss (to-ast (b= 3 (bmax #{1 2 3})))))
      (is (eval ss (to-ast (b= 3 (bmax 1 2 3)))))
      (is (eval ss (to-ast (b= 3 (bmax 2 3)))))

      (is (eval ss (to-ast (b= 1 (bmin #{1 2 3})))))
      (is (eval ss (to-ast (b= 1 (bmin 1 2 3)))))
      (is (eval ss (to-ast (b= 2 (bmin 2 3)))))

      (is (eval ss (to-ast (b= 2 (bmod 5 3)))))

      (is (eval ss (to-ast (b= 2 (binc 1)))))

      (is (eval ss (to-ast (b= 0 (bdec 1)))))
      
      (is (eval ss (to-ast (b= :x #{[1 2]}))))

      (is (eval ss (to-ast (bmember :x (b<-> #{1 2} #{3 4})))))
      (is (eval ss (to-ast (bmember :x (b<-> #{1 2} #{3 4} #{5 6})))))

      (is (eval ss (to-ast (b= #{1 2} (bdom #{[1 0] [1 1] [2 42]})))))

      (is (eval ss (to-ast (b= #{0 1 42} (bran #{[1 0] [1 1] [2 42]})))))

      (is (eval ss (to-ast (b= #{[1 1]} (bid #{1})))))

      (is (eval ss (to-ast (b= #{[1 1]} (b<| #{1 2} #{[1 1] [3 0]})))))

      (is (eval ss (to-ast (b= #{[3 0]} (b<<| #{1 2} #{[1 1] [3 0]})))))

      (is (eval ss (to-ast (b= #{[1 1]} (b|> #{[1 1] [3 0]} #{1 2})))))

      (is (eval ss (to-ast (b= #{[3 0]} (b|>> #{[1 1] [3 0]} #{1 2})))))

      (is (eval ss (to-ast (b= #{[3 0]} (binverse #{[0 3]})))))

      (is (eval ss (to-ast (b= #{2} (bimage #{[1 2] [0 3]} #{1 2})))))

      (is (eval ss (to-ast (b= #{[0 0]} (b<+ #{[0 1]} #{[0 0]})))))
      (is (eval ss (to-ast (b= #{[0 0] [1 0]} (b<+ #{[0 1]} #{[0 0]} #{[1 0]})))))

      (is (eval ss (to-ast (b= #{[0 [1 1]] [0 [2 1]]} (b>< #{[0 1] [0 2]} #{[0 1]})))))
      (is (eval ss (to-ast (b= #{[0 [[1 1] 3]] [0 [[2 1] 3]]} (b>< #{[0 1] [0 2]} #{[0 1]} #{[0 3]})))))


      (is (eval ss (to-ast (b= #{[0 0]} (bcomp #{[0 1]} #{[1 0]})))))
      (is (eval ss (to-ast (b= #{[0 5]} (bcomp #{[0 1]} #{[1 2]} #{[2 5]})))))

      (is (eval ss (to-ast (b= #{[[0 0] [1 1]] [[0 0] [2 1]]} (b|| #{[0 1] [0 2]} #{[0 1]})))))
      (is (eval ss (to-ast (b= :a (b|| #{[0 1] [0 2]} #{[0 1]} #{[1 1]}))))) ;; if it works, that's good enough for me

      (is (eval ss (to-ast (b= #{[[1 3] 1] [[1 4] 1]} (bprj1 #{1} #{3, 4})))))
      (is (eval ss (to-ast (b= #{[[1 3] 3] [[1 4] 4]} (bprj2 #{1} #{3, 4})))))

      (is (eval ss (to-ast (b= #{[1 1] [1 2] [2 2]} (bclosure #{[1 2]})))))
      (is (eval ss (to-ast (b= #{[1 2]} (bclosure1 #{[1 2]})))))

      (is (eval ss (to-ast (b= #{[1 3] [4 2] [5 3]} (biterate #{[1 2] [2 3] [4 5] [5 2]} 2)))))

      (is (eval ss (to-ast (b= #{[1 #{2 3}] [3 #{4}]} (bfnc #{[1 2] [1 3] [3 4]})))))

      (is (eval ss (to-ast (b= #{[1 2] [1 3] [3 4]} (brel #{[1 #{2 3}] [3 #{4}]})))))

      (is (eval ss (to-ast (b= #{#{} #{[1 2]} #{[1 2] [2 2]} #{[2 2]}} (b+-> #{1 2} #{2})))))
      (is (eval ss (to-ast (b= :a #{(b+-> #{1 2} #{2} #{3 4})}))))

      (is (eval ss (to-ast (b= #{#{[1 2] [2 2]}} (b--> #{1 2} #{2})))))
      (is (eval ss (to-ast (b= #{#{[#{[1 2] [2 2]} 3]}} (b--> #{1 2} #{2} #{3})))))

      (is (eval ss (to-ast (b= #{#{[1 2]} #{[1 2] [2 2]} #{[2 2]}} (b+->> #{1 2} #{2})))))
      (is (eval ss (to-ast (b= :a #{(b+->> #{1 2} #{2} #{3 4})}))))

      (is (eval ss (to-ast (b= #{#{[1 2] [2 2]}} (b-->> #{1 2} #{2})))))
      (is (eval ss (to-ast (b= #{} (b-->> #{1 2} #{2} #{3 4})))))

      (is (eval ss (to-ast (b= #{#{} #{[1 3]} #{[1 4]}} (b>+> #{1} #{3 4})))))
      (is (eval ss (to-ast (b= #{#{} #{[#{} 2]} #{[#{[1 3]} 2]} #{[#{[1 4]} 2]}} (b>+> #{1} #{3 4} #{2})))))

      (is (eval ss (to-ast (b= #{#{[1 3]} #{[1 4]}} (b>-> #{1} #{3 4})))))
      (is (eval ss (to-ast (b= :a #{(b>-> #{1} #{3 4} #{5 6})}))))

      (is (eval ss (to-ast (b= #{#{[1 3] [2 4]} #{[1 4] [2 3]}} (b>+>> #{1 2} #{3 4})))))
      (is (eval ss (to-ast (b= #{#{[#{[1 3] [2 4]} 5]} #{[#{[1 4] [2 3]} 5]}} (b>+>> #{1 2} #{3 4} #{5})))))

      (is (eval ss (to-ast (b= #{#{[1 3] [2 4]} #{[1 4] [2 3]}} (b>->> #{1 2} #{3 4})))))
      (is (eval ss (to-ast (b= :a #{(b>->> #{1 2} #{3 4} #{5 6})}))))

      (is (eval ss (to-ast (b= #{[3 9] [4 16]} (blambda [:x] (b< 2 :x 5) (b* :x :x))))))

      (is (eval ss (to-ast (b= 2 (bapply #{[1 2]} 1)))))
      (is (eval ss (to-ast (b= 3 (bapply #{[[1 2] 3]} 1 2)))))
      (is (eval ss (to-ast (b= 3 (bapply #{[[1 2] 3]} [1 2])))))

      (is (eval ss (to-ast (b=> (b= true true) (b= true true)))))
      (is (eval ss (to-ast (b=> (b= true true) (b= true false) (b= true true)))))

      (is (eval ss (to-ast (bforall [:x] (b=> (b= :x false) (b= true true))))))
      (is (eval ss (to-ast (bforall [:x :y] (b=> (b< 0 :x :y 3) (b<= (binc :x) :y))))))

      (is (eval ss (to-ast (b= (binterval 1 5) #{1 2 3 4 5}))))

      (is (eval ss (to-ast (bexists [:x :y] (b< :x :y)))))

      (is (eval ss (to-ast (b= (bsequence 3 2 1) #{[1 3] [2 2] [3 1]}))))
      (is (eval ss (to-ast (b= (bsequence) #{}))))

      (is (eval ss (to-ast (bmember (bsequence [3 3] [2 2] [1 4]) (biseq (bsequence 4 2 3))))))
      (is (eval ss (to-ast (bmember (bsequence [3 3] [2 2] [1 4]) (biseq1 (bsequence 4 2 3))))))

      (is (eval ss (to-ast (b= #{#{[1 [1 4]] [2 [2 2]]} #{[1 [2 2]] [2 [1 4]]}} (bperm (bsequence 4 2))))))

      (is (eval ss (to-ast (b= (bsequence 3 4 1 5) (bconcat (bsequence 3 4) (bsequence 1 5))))))
      (is (eval ss (to-ast (b= (bsequence 3 4 1 5 2) (bconcat (bsequence 3 4) (bsequence 1 5) (bsequence 2))))))

      (is (eval ss (to-ast (b= (bsequence 3 1 4) (b-> 3 (bsequence 1 4))))))

      (is (eval ss (to-ast (b= (bsequence 3 1 4) (b<- (bsequence 3 1) 4)))))
      (is (eval ss (to-ast (b= (bsequence 3 1 4) (b<- (bsequence 3) 1 4)))))

      (is (eval ss (to-ast (b= (breverse (bsequence 3 1 4)) (bsequence 4 1 3)))))

      (is (eval ss (to-ast (b= (bfirst (bsequence 3 1 4)) 3))))
      (is (eval ss (to-ast (b= (blast (bsequence 3 1 4)) 4))))

      (is (eval ss (to-ast (b= (bfront (bsequence 3 1 4)) (bsequence 3 1)))))
      (is (eval ss (to-ast (b= (btail (bsequence 3 1 4)) (bsequence 1 4)))))

      (is (eval ss (to-ast (b= (btake 3 (bsequence 3 1 4 1 5)) (bsequence 3 1 4)))))
      (is (eval ss (to-ast (b= (bdrop 3 (bsequence 3 1 4 1 5)) (bsequence 1 5)))))

      (is (eval ss (to-ast (b= (b** 2 3) 8))))
      (is (eval ss (to-ast (b= (b** 2 2 3) 256))))

      (is (eval ss (to-ast (b= (bsigma [:a] (b<= 1 :a 4) :a) 10))))
      (is (eval ss (to-ast (b= (bpi    [:a] (b<= 1 :a 4) :a) 24))))

      (is (eval ss (to-ast (bmember (bsequence 0 2 2 0 1 0 0) (bseq #{0 1 2})))))
      (is (eval ss (to-ast (bmember #{} (bseq #{0 1 2})))))

      (is (eval ss (to-ast (bmember (bsequence 0 2 2 0 1 0 0) (bseq1 #{0 1 2})))))
      (is (eval ss (to-ast (bnot (bmember #{} (bseq1 #{0 1 2}))))))

      (is (eval ss (to-ast (b= (bsequence 3 1 4 1 5 9 2) (bconc (bsequence (bsequence 3 1 4) (bsequence 1 5 9 2)))))))

      (is (eval ss (to-ast (b= :x (bstruct "x" #{1 2 3})))))
      (is (eval ss (to-ast (b= :x (bstruct {:x #{1 2 3}})))))

      (is (eval ss (to-ast (bmember (brecord "x" 1) (bstruct "x" #{1 2 3})))))

      (is (eval ss (to-ast (b= 1 (brec-get (brecord "x" 1) "x")))))


      (is (eval ss (to-ast (b= #{1 2 3} (bset-enum 1 2 3)))))

      (is (eval ss (to-ast (b= #{[1 2]} (bset-enum (btuple 1 2))))))

      )))



(deftest fancy-fns-test
  (testing "fancier functions"
    (let [ss te-ss-t]
      (is (eval ss (to-ast (b= #{2 3 4} (bmap-set (pred [x] (+ 1 x)) #{1 2 3})))))
      (is (eval ss (to-ast (b= (bif (b< 1 2) 3 4) 3))))
      (is (eval ss (to-ast (b= (bif (b> 1 2) 3 4) 4))))
          )))


(deftest b-haviour
  (let [ss (state-space)]
    (testing "satisfiable predicates should have truthy return values"
      (is (eval ss (to-ast (b (= 1 1)))))
      (is (eval ss (to-ast (b (= :x 1))))))
    (testing "the return values should be maps"
      (is (= {} (eval ss (to-ast (b (= 1 1))))))
      (is (= {"x" 1} (eval ss (to-ast (b (= :x 1)))))))
    ;; TODO: test concerning the resulting data structures, i.e. sets, sequences, tuples...
    (testing "unsatisfiable predicates yield a nil value"
      (is (nil? (eval ss (to-ast (b (= 1 2)))))))
    (testing "timeouts / no found solution should throw an exception"
      (is (thrown? Exception (eval ss (to-ast (b (< :x :y :x)))))))))
