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
      (apply eval ss (to-ast (b= 1 1)))
      (apply eval ss (to-ast (b= :x 1)))
      (apply eval ss (to-ast (b= 1 1 1)))
      (apply eval ss (to-ast (b= :x true)))
      (apply eval ss (to-ast (b= :x false)))

      (apply eval ss (to-ast (bnot= :x 1)))
      (apply eval ss (to-ast (bnot= :x 1 2)))

      (apply eval ss (to-ast (b< 1 2)))
      (apply eval ss (to-ast (b< 1 2 3)))

      (apply eval ss (to-ast (b< 1 (b+ 1 2))))
      (apply eval ss (to-ast (b< 1 (b+ 1 2 3))))

      (apply eval ss (to-ast (b< 1 (b- 10 2))))
      (apply eval ss (to-ast (b< 1 (b- 10 2 3))))

      (apply eval ss (to-ast (band (b< 1 2) (b< 2 3))))
      (apply eval ss (to-ast (band (b< 1 2) (b< 2 3) (b< 3 4))))

      (apply eval ss (to-ast (bor (b< 1 2) (b< 2 3))))
      (apply eval ss (to-ast (bor (b< 1 2) (b< 2 3) (b< 3 4))))

      (apply eval ss (to-ast (b<=> (b< 1 2) (b< 2 3))))
      (apply eval ss (to-ast (b<=> (b< 1 2) (b< 2 3) (b< 3 4))))

      (apply eval ss (to-ast (bnot (b< 2 1))))
      )))

