(ns lisb.integration-test
  (:require [clojure.test :refer :all])
  (:require [lisb.core :refer [eval state-space]])
  (:require [lisb.representation :refer :all])
  (:require [lisb.translation :refer [to-ast]]))

(deftest integration
  (testing "no exception is thrown when executing implemented command;
            this test only checks that a valid (i.e. accepted) AST is constructed
            without concern about semantics
            for now only 'truish' predicates are evaluated because eval cannot handle
            ComputationNotCompletedResults yet"
    (let [ss (state-space)]
      (is (apply eval ss (to-ast (b= 1 1))))
      (is (apply eval ss (to-ast (b= :x 1))))
      (is (apply eval ss (to-ast (b= 1 1 1))))
      (is (apply eval ss (to-ast (b= :x true))))
      (is (apply eval ss (to-ast (b= :x false))))

      (is (apply eval ss (to-ast (bnot= :x 1))))
      (is (apply eval ss (to-ast (bnot= :x 1 2))))

      (is (apply eval ss (to-ast (b< 1 2))))
      (is (apply eval ss (to-ast (b< 1 2 3))))

      (is (apply eval ss (to-ast (b> 2 1))))
      (is (apply eval ss (to-ast (b> 3 2 1))))

      (is (apply eval ss (to-ast (b<= 1 2))))
      (is (apply eval ss (to-ast (b<= 1 1 3))))

      (is (apply eval ss (to-ast (b>= 2 1))))
      (is (apply eval ss (to-ast (b>= 3 3 1))))

      (is (apply eval ss (to-ast (b< 1 (b+ 1 2)))))
      (is (apply eval ss (to-ast (b< 1 (b+ 1 2 3)))))

      (is (apply eval ss (to-ast (b< 1 (b- 10 2)))))
      (is (apply eval ss (to-ast (b< 1 (b- 10 2 3)))))

      (is (apply eval ss (to-ast (b= 6 (b* 2 3)))))
      (is (apply eval ss (to-ast (b= 24 (b* 2 3 4)))))

      (is (apply eval ss (to-ast (band (b< 1 2) (b< 2 3)))))
      (is (apply eval ss (to-ast (band (b< 1 2) (b< 2 3) (b< 3 4)))))

      (is (apply eval ss (to-ast (bor (b< 1 2) (b< 2 3)))))
      (is (apply eval ss (to-ast (bor (b< 1 2) (b< 2 3) (b< 3 4)))))

      (is (apply eval ss (to-ast (b<=> (b< 1 2) (b< 2 3)))))
      (is (apply eval ss (to-ast (b<=> (b< 1 2) (b< 2 3) (b< 3 4)))))

      (is (apply eval ss (to-ast (bnot (b< 2 1)))))

      (is (apply eval ss (to-ast (b= true (bpred->bool (b< 1 2))))))

      (is (apply eval ss (to-ast (b= :x #{1 2}))))
      (is (apply eval ss (to-ast (b= :x #{1 2 (b+ 1 2)}))))

      (is (apply eval ss (to-ast (b= #{1 2 3} (bset [:x] (b< 0 :x 4))))))
      
      (is (apply eval ss (to-ast (b= :x (bpow #{1 2})))))

      (is (apply eval ss (to-ast (b= :x (bpow1 #{1 2})))))

      (is (apply eval ss (to-ast (b= :x (bfin #{1 2})))))

      (is (apply eval ss (to-ast (b= :x (bfin1 #{1 2})))))

      (is (apply eval ss (to-ast (b= 2 (bcount #{1 2})))))

      (is (apply eval ss (to-ast (b= #{} (bx #{} #{})))))

      ;; FIXME: without the additional set the translator breaks
      (is (apply eval ss (to-ast (b= :x #{(bx #{1 2} #{2 3})}))))

      (is (apply eval ss (to-ast (b= :x #{(bx #{1 2} #{2 3} #{3 4})}))))

      (is (apply eval ss (to-ast (b= #{1 2 3} (bunion #{1 2} #{2 3})))))
      (is (apply eval ss (to-ast (b= #{1 2 3 4} (bunion #{1 2} #{2 3} #{3 4})))))

      (is (apply eval ss (to-ast (b= #{2} (bintersect #{1 2} #{2 3})))))
      (is (apply eval ss (to-ast (b= #{3 4} (bintersect #{1 2 3 4} #{2 3 4} #{3 4})))))

      (is (apply eval ss (to-ast (b= #{1} (bset- #{1 2} #{2 3})))))
      (is (apply eval ss (to-ast (b= #{1} (bset- #{1 2} #{2 3} #{3 4})))))

      (is (apply eval ss (to-ast (bmember 1 #{1}))))
      (is (apply eval ss (to-ast (bmember 1 #{1} #{1 2}))))

      (is (apply eval ss (to-ast (bmembers #{1 2 3} 1))))
      (is (apply eval ss (to-ast (bmembers #{1 2 3} 1 2))))

      (is (apply eval ss (to-ast (bsubset #{1} #{1 2}))))
      (is (apply eval ss (to-ast (bsubset #{1} #{1 2} #{1 2 3}))))

      (is (apply eval ss (to-ast (bsuperset #{1 2} #{1}))))
      (is (apply eval ss (to-ast (bsuperset #{1 2 3} #{1 2} #{1}))))

      (is (apply eval ss (to-ast (bsubset-strict #{1} #{1 2}))))
      (is (apply eval ss (to-ast (bsubset-strict #{1} #{1 2} #{1 2 3}))))

      (is (apply eval ss (to-ast (bsuperset-strict #{1 2} #{1}))))
      (is (apply eval ss (to-ast (bsuperset-strict #{1 2 3} #{1 2} #{1}))))

      (is (apply eval ss (to-ast (bmember true (bbool-set)))))
      (is (apply eval ss (to-ast (bmember 0 (bnatural-set)))))
      (is (apply eval ss (to-ast (bmember 1 (bnatural1-set)))))
      (is (apply eval ss (to-ast (bmember -1 (bint-set)))))
      (is (apply eval ss (to-ast (bmember 0 (bnat-set)))))
      (is (apply eval ss (to-ast (bmember 1 (bnat1-set)))))

      (is (apply eval ss (to-ast (b= 3 (bmax #{1 2 3})))))
      (is (apply eval ss (to-ast (b= 3 (bmax 1 2 3)))))
      (is (apply eval ss (to-ast (b= 3 (bmax 2 3)))))

      (is (apply eval ss (to-ast (b= 1 (bmin #{1 2 3})))))
      (is (apply eval ss (to-ast (b= 1 (bmin 1 2 3)))))
      (is (apply eval ss (to-ast (b= 2 (bmin 2 3)))))
      )))
