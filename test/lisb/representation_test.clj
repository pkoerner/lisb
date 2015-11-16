(ns lisb.representation-test
  (:require [clojure.test :refer :all]
            [lisb.representation :refer :all]))


(deftest less-test
  (testing "less works with two arguments"
    (is (= {:tag :less
            :children [:a :b]}
           (b< :a :b))))
  (testing "less works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :less :children [:a :b]}
                       {:tag :less :children [:b :c]}]}
           (b< :a :b :c)))))

(deftest greater
  (testing "greater works with two arguments"
    (is (= {:tag :greater
            :children [:a :b]}
           (b> :a :b))))
  (testing "greater works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :greater :children [:a :b]}
                       {:tag :greater :children [:b :c]}]}
           (b> :a :b :c)))))

(deftest less-eq-test
  (testing "less-eq works with two arguments"
    (is (= {:tag :less-eq
            :children [:a :b]}
           (b<= :a :b))))
  (testing "less-eq works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :less-eq :children [:a :b]}
                       {:tag :less-eq :children [:b :c]}]}
           (b<= :a :b :c)))))

(deftest greater-eq
  (testing "greater-eq works with two arguments"
    (is (= {:tag :greater-eq
            :children [:a :b]}
           (b>= :a :b))))
  (testing "greater-eq works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :greater-eq :children [:a :b]}
                       {:tag :greater-eq :children [:b :c]}]}
           (b>= :a :b :c)))))

(deftest plus-test
  (testing "plus works with two arguments"
    (is (= {:tag :plus
            :children [:a :b]}
           (b+ :a :b))))
  (testing "plus works with more than two arguments"
    (is (= {:tag :plus
            :children [{:tag :plus :children [:a :b]}
                       :c]}
           (b+ :a :b :c)))))

(deftest mul-test
  (testing "mul works with two arguments"
    (is (= {:tag :mul
            :children [:a :b]}
           (b* :a :b))))
  (testing "mul works with more than two arguments"
    (is (= {:tag :mul
            :children [{:tag :mul :children [:a :b]}
                       :c]}
           (b* :a :b :c)))))

(deftest div-test
  (testing "div works with two arguments"
    (is (= {:tag :div
            :children [:a :b]}
           (bdiv :a :b))))
  (testing "div works with more than two arguments"
    (is (= {:tag :div
            :children [{:tag :div :children [:a :b]}
                       :c]}
           (bdiv :a :b :c)))))

(deftest and-test
  (testing "and works with two arguments"
    (is (= {:tag :and
            :children [:a :b]}
           (band :a :b))))
  (testing "and works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :and :children [:a :b]}
                       :c]}
           (band :a :b :c)))))

(deftest minus-test
  (testing "minus is special and must have a unary version"
    (is (= {:tag :unaryminus
            :children [:a]}
           (b- :a))))
  (testing "minus also works with two arguments"
    (is (= {:tag :minus
            :children [:a :b]}
           (b- :a :b))))
  (testing "minus also works as expected with more than two arguments"
    (is (= {:tag :minus
            :children [{:tag :minus :children [:a :b]}
                       :c]}
           (b- :a :b :c)))))

(deftest equals-test
  (testing "equals works with two arguments"
    (is (= {:tag :equals
            :children [:a :b]}
           (b= :a :b))))
  (testing "equals works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :equals :children [:a :b]}
                       {:tag :equals :children [:b :c]}]}
           (b= :a :b :c)))))


(deftest equivalence-test
  (testing "equals works with two arguments"
    (is (= {:tag :equivalence
            :children [:a :b]}
           (b<=> :a :b))))
  (testing "equals works with more than two arguments"
    (is (= {:tag :and
            :children [{:tag :equivalence :children [:a :b]}
                       {:tag :equivalence :children [:b :c]}]}
           (b<=> :a :b :c)))))



(deftest or-test
  (testing "or works with two arguments"
    (is (= {:tag :or
            :children [:a :b]}
           (bor :a :b))))
  (testing "or works with more than two arguments"
    (is (= {:tag :or
            :children [{:tag :or :children [:a :b]}
                       :c]}
           (bor :a :b :c)))))

(deftest not-test
  (testing "not is unary"
    (is (= {:tag :not
            :children [:a]}
           (bnot :a)))))

(deftest not-equals-test
  (testing "not-equals works with two arguments"
    (is (= {:tag :not-equals
            :children [:a :b]}
           (bnot= :a :b))))
  (testing "not-equals works with more than two arguments"
    (is (= {:tag :and,
            :children [{:tag :and, :children [{:tag :not-equals, :children [:a :b]}
                                              {:tag :not-equals, :children [:a :c]}]}
                       {:tag :not-equals :children [:b :c]}]}
           (bnot= :a :b :c))))
  (testing "not-equals generates data in a set-like semantics"
    (is (= {:tag :and
            :children [{:tag :not-equals :children [:a :a]}
                       {:tag :not-equals :children [:a :b]}]}
           (bnot= :a :b :a)))))

(deftest to-bool-test
  (testing "to-bool works as intended"
    (is (= {:tag :to-bool
            :children [:a]}
           (bpred->bool :a)))))

(deftest bset-test
  (testing "construction of a comprehension set"
    (is (= {:tag :comp-set
            :children [{:tag :var-list :children [:x]}
                       {:tag :and :children [{:tag :less :children [1 :x]}
                                             {:tag :less :children [:x 5]}]}]}
           (bset [:x] (b< 1 :x 5))))))

(deftest powerset-test
  (testing "powerset representation"
    (is (= {:tag :power-set
            :children [#{1 2}]}
           (bpow #{1 2})))))

(deftest powerset1-test
  (testing "powerset1 representation"
    (is (= {:tag :power1-set
            :children [#{1 2}]}
           (bpow1 #{1 2})))))

(deftest finset-test
  (testing "finite subset representation"
    (is (= {:tag :finite-subset
            :children [#{1 2}]}
           (bfin #{1 2})))))

(deftest finset1-test
  (testing "finite subset representation"
    (is (= {:tag :finite1-subset
            :children [#{1 2}]}
           (bfin1 #{1 2})))))

(deftest card-test
  (testing "cardinality operator representation"
    (is (= {:tag :card
            :children [#{1 2}]}
           (bcount #{1 2})))))

(deftest cartesian-product-test
  (testing "cartesian product with two arguments"
    (is (= {:tag :cartesian-product
            :children [#{1 2} #{2 3}]}
           (bx #{1 2} #{2 3}))))
  (testing "cartesian product with more than two arguments"
    (is (= {:tag :cartesian-product
            :children [{:tag :cartesian-product :children [#{1 2} #{2 3}]}
                       #{3 4}]}
           (bx #{1 2} #{2 3} #{3 4})))))

(deftest set-union-test
  (testing "set union with two arguments"
    (is (= {:tag :set-union
            :children [#{1 2} #{2 3}]}
           (bunion #{1 2} #{2 3}))))
  (testing "set union with more than two arguments"
    (is (= {:tag :set-union
            :children [{:tag :set-union :children [ #{1 2} #{2 3}]}
                       #{3 4}]}
           (bunion #{1 2} #{2 3} #{3 4})))))

(deftest set-intersection-test
  (testing "set intersection with two arguments"
    (is (= {:tag :set-intersection
            :children [#{1 2} #{2 3}]}
           (bintersect #{1 2} #{2 3}))))
  (testing "set intersection with more than two arguments"
    (is (= {:tag :set-intersection
            :children [{:tag :set-intersection :children [ #{1 2} #{2 3}]}
                       #{3 4}]}
           (bintersect #{1 2} #{2 3} #{3 4})))))

(deftest set-difference-test
  (testing "set difference with two arguments"
    (is (= {:tag :set-difference
            :children [#{1 2} #{2 3}]}
           (bset- #{1 2} #{2 3}))))
  (testing "set intersection with more than two arguments"
    (is (= {:tag :set-difference
            :children [{:tag :set-difference :children [ #{1 2} #{2 3}]}
                       #{3 4}]}
           (bset- #{1 2} #{2 3} #{3 4})))))

(deftest member-test
  (testing "member representation with two arguments"
    (is (= {:tag :member
            :children [1 #{1}]}
           (bmember 1 #{1}))))
  (testing "member representation with more than two arguments"
    (is (= {:tag :and :children [{:tag :member :children [1 #{1}]}
                                 {:tag :member :children [1 #{2}]}]}
           (bmember 1 #{1} #{2})))))

(deftest members-test
  (testing "members representation with two arguments"
    (is (= {:tag :member
            :children [1 #{1}]}
           (bmembers #{1} 1))))
  (testing "members representation with more than two arguments"
    (is (= {:tag :and :children [{:tag :member :children [1 #{1}]}
                                 {:tag :member :children [2 #{1}]}]}
           (bmembers #{1} 1 2)))))

(deftest subset-test
  (testing "subset representation with two arguments"
    (is (= {:tag :subset
            :children [#{1} #{2}]}
           (bsubset #{1} #{2})
           (bsuperset #{2} #{1}))))
  (testing "subset representation with more than two arguments"
    (is (= {:tag :and :children [{:tag :subset :children [#{1} #{2}]}
                                 {:tag :subset :children [#{2} #{3}]}]}
           (bsubset #{1} #{2} #{3})
           (bsuperset #{3} #{2} #{1})))))

(deftest subset-strict-test
  (testing "subset-strict representation with two arguments"
    (is (= {:tag :subset-strict
            :children [#{1} #{2}]}
           (bsubset-strict #{1} #{2})
           (bsuperset-strict #{2} #{1}))))
  (testing "subset-strict representation with more than two arguments"
    (is (= {:tag :and :children [{:tag :subset-strict :children [#{1} #{2}]}
                                 {:tag :subset-strict :children [#{2} #{3}]}]}
           (bsubset-strict #{1} #{2} #{3})
           (bsuperset-strict #{3} #{2} #{1})))))

(deftest bool-set-test
  (testing "have a set of booleans"
    (is (= {:tag :bool-set :children []}
           (bbool-set)))))

(deftest natural-set-test
  (testing "have a set of naturals"
    (is (= {:tag :natural-set :children []}
           (bnatural-set)))))

(deftest natural1-set-test
  (testing "have a set of naturals1"
    (is (= {:tag :natural1-set :children []}
           (bnatural1-set)))))

(deftest int-set-test
  (testing "have a set of integers"
    (is (= {:tag :int-set :children []}
           (bint-set)))))

(deftest nat-set-test
  (testing "have a set of nat"
    (is (= {:tag :nat-set :children []}
           (bnat-set)))))

(deftest nat1-set-test
  (testing "have a set of nat1"
    (is (= {:tag :nat1-set :children []}
           (bnat1-set)))))

(deftest max-test
  (testing "max with a set"
    (is (= {:tag :max :children [#{1 2 3}]}
           (bmax #{1 2 3}))))
  (testing "max with more arguments"
    (is (= {:tag :max :children [#{1 2}]}
           (bmax 1 2)))
    (is (= {:tag :max :children [#{1 2 3}]}
           (bmax 1 2 3)))))

(deftest min-test
  (testing "min with a set"
    (is (= {:tag :min :children [#{1 2 3}]}
           (bmin #{1 2 3}))))
  (testing "min with more arguments"
    (is (= {:tag :min :children [#{1 2}]}
           (bmin 1 2)))
    (is (= {:tag :min :children [#{1 2 3}]}
           (bmin 1 2 3)))))

(deftest mod-test
  (testing "modulo"
    (is (= {:tag :mod :children [5 3]}
           (bmod 5 3)))))

(deftest inc-test
  (testing "inc adds one"
    (is (= {:tag :plus :children [5 1]}
           (binc 5)))))

(deftest dec-test
  (testing "dec subtracts one"
    (is (= {:tag :minus :children [5 1]}
           (bdec 5)))))


(deftest relation-test
  (testing "relation works with two arguments"
    (is (= {:tag :relation
            :children [#{1 2} #{2 3}]}
           (b<-> #{1 2} #{2 3}))))
  (testing "relation works with more than two arguments"
    (is (= {:tag :relation
            :children [{:tag :relation :children [ #{1 2} #{2 3}]}
                       #{3 4}]}
           (b<-> #{1 2} #{2 3} #{3 4})))))


(deftest domain-test
  (testing "domain works"
    (is (= {:tag :domain
            :children [:a]}
           (bdom :a)))))

(deftest range-test
  (testing "range works"
    (is (= {:tag :range
            :children [:a]}
           (bran :a)))))

(deftest identity-test
  (testing "id works"
    (is (= {:tag :identity-relation
            :children [:a]}
           (bid :a)))))


(deftest domain-restriction-test
  (testing "domain restriction representation"
    (is (= {:tag :domain-restriction
            :children [#{1 2} #{[1 0] [3 1]}]}
           (b<| #{1 2} #{[1 0] [3 1]})))))

(deftest domain-subtraction-test
  (testing "domain subtraction representation"
    (is (= {:tag :domain-subtraction
            :children [#{1 2} #{[1 0] [3 1]}]}
           (b<<| #{1 2} #{[1 0] [3 1]})))))

(deftest range-restriction-test
  (testing "range restriction representation"
    (is (= {:tag :range-restriction
            :children [#{[1 0] [3 1]} #{1 2}]}
           (b|> #{[1 0] [3 1]} #{1 2})))))

(deftest range-subtraction-test
  (testing "range subtraction representation"
    (is (= {:tag :range-subtraction
            :children [#{[1 0] [3 1]} #{1 2}]}
           (b|>> #{[1 0] [3 1]} #{1 2})))))

(deftest inverse-test
  (testing "inverse relation representation"
    (is (= {:tag :inverse-relation
            :children [#{[1 0] [3 1]}]}
           (binverse #{[1 0] [3 1]})))))

(deftest image-test
  (testing "relational image representation"
    (is (= {:tag :relational-image
            :children [#{[1 0] [3 1]} #{0 1 2}]}
           (bimage #{[1 0] [3 1]} #{0 1 2})))))

(deftest override-test
  (testing "relational override representation with two args"
    (is (= {:tag :relational-override
            :children [#{[1 0] [3 1]} #{[1 1]}]}
           (b<+ #{[1 0] [3 1]} #{[1 1]}))))
  (testing "relational override representation with more than two args"
    (is (= {:tag :relational-override
            :children [{:tag :relational-override :children [#{[1 0] [3 1]} #{[1 1]}]}
                       #{[2 2]}
                        ]}
           (b<+ #{[1 0] [3 1]} #{[1 1]} #{[2 2]})))))


(deftest product-test
  (testing "direct product representation with two args"
    (is (= {:tag :direct-product
            :children [#{[1 0] [3 1]} #{[1 1]}]}
           (b>< #{[1 0] [3 1]} #{[1 1]}))))
  (testing "direct product representation with more than two args"
    (is (= {:tag :direct-product
            :children [{:tag :direct-product :children [#{[1 0] [3 1]} #{[1 1]}]}
                       #{[2 2]}
                        ]}
           (b>< #{[1 0] [3 1]} #{[1 1]} #{[2 2]})))))

(deftest composition-test
  (testing "relational composition representation with two args"
    (is (= {:tag :relational-composition
            :children [#{[1 0] [3 1]} #{[1 1]}]}
           (bcomp #{[1 0] [3 1]} #{[1 1]}))))
  (testing "relational composition representation with more than two args"
    (is (= {:tag :relational-composition
            :children [{:tag :relational-composition :children [#{[1 0] [3 1]} #{[1 1]}]}
                       #{[2 2]}]}
           (bcomp #{[1 0] [3 1]} #{[1 1]} #{[2 2]})))))

(deftest parallel-product-test
  (testing "parallel product representation with two args"
    (is (= {:tag :parallel-product
            :children [#{[1 0] [3 1]} #{[1 1]}]}
           (b|| #{[1 0] [3 1]} #{[1 1]}))))
  (testing "parallel product representation with more than two args"
    (is (= {:tag :parallel-product
            :children [{:tag :parallel-product :children [#{[1 0] [3 1]} #{[1 1]}]}
                       #{[2 2]}]}
           (b|| #{[1 0] [3 1]} #{[1 1]} #{[2 2]})))))


(deftest prj1-test
  (testing "projection1 function representation"
    (is (= {:tag :proj1
            :children [#{3 1} #{0 1 2}]}
           (bprj1 #{3 1} #{0 1 2})))))

(deftest prj2-test
  (testing "projection2 function representation"
    (is (= {:tag :proj2
            :children [#{3 1} #{0 1 2}]}
           (bprj2 #{3 1} #{0 1 2})))))

(deftest closure-test
  (testing "transitive closure representation"
    (is (= {:tag :closure
            :children [#{[1 2]}]}
           (bclosure #{[1 2]})))))

(deftest closure1-test
  (testing "transitive closure1 representation"
    (is (= {:tag :closure1
            :children [#{[1 2]}]}
           (bclosure1 #{[1 2]})))))

(deftest iteration-test
  (testing "iteration representation"
    (is (= {:tag :iterate
            :children [#{[1 2] [2 3]} 2]}
           (biterate #{[1 2] [2 3]} 2)))))

(deftest fnc-test
  (testing "function translation representation"
    (is (= {:tag :functionise
            :children [#{[1 2]}]}
           (bfnc #{[1 2]})))))

(deftest rel-test
  (testing "function translation representation"
    (is (= {:tag :relationise
            :children [#{[1 #{2}]}]}
           (brel #{[1 #{2}]})))))

(deftest partial-fn-test
  (testing "partial function with two arguments"
    (is (= {:tag :partial-fn
            :children [#{1 2} #{3 4}]}
           (b+-> #{1 2} #{3 4})))
    (is (= {:tag :partial-fn
            :children [{:tag :partial-fn
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b+-> #{1 2} #{3 4} #{5 6})))))
