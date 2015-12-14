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
           (bnone= :a :b))))
  (testing "not-equals works with more than two arguments"
    (is (= {:tag :and,
            :children [{:tag :and, :children [{:tag :not-equals, :children [:a :b]}
                                              {:tag :not-equals, :children [:a :c]}]}
                       {:tag :not-equals :children [:b :c]}]}
           (bnone= :a :b :c))))
  (testing "not-equals generates data in a set-like semantics"
    (is (= {:tag :and
            :children [{:tag :not-equals :children [:a :a]}
                       {:tag :not-equals :children [:a :b]}]}
           (bnone= :a :b :a)))))

(deftest to-bool-test
  (testing "to-bool works as intended"
    (is (= {:tag :to-bool
            :children [:a]}
           (bpred->bool :a)))))

(deftest bset-test
  (testing "construction of a comprehension set"
    (is (= {:tag :comp-set
            :children [{:tag :list :children [:x]}
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

(deftest union-test
  (testing "set union with two arguments"
    (is (= {:tag :set-union
            :children [#{1 2} #{2 3}]}
           (bunion #{1 2} #{2 3}))))
  (testing "set union with more than two arguments"
    (is (= {:tag :set-union
            :children [{:tag :set-union :children [ #{1 2} #{2 3}]}
                       #{3 4}]}
           (bunion #{1 2} #{2 3} #{3 4})))))

(deftest unite-sets-test
  (testing "generalised union over set of sets"
    (is (= {:tag :general-union
            :children [#{#{1} #{2} #{3}}]}
           (bunite-sets #{#{1} #{2} #{3}})))))

(deftest union-pe-test
  (testing "generalised intersection with predicate"
    (is (= {:tag :union-pe
            :children [{:tag :list :children [:x]}
                       {:tag :and :children [{:tag :less :children [0 :x]}
                                             {:tag :less :children [:x 5]}]}
                       #{:x {:tag :mul :children [:x :x]}}]}
           (bunion-pe [:x] (b< 0 :x 5) #{:x (b* :x :x)})))))

(deftest intersection-test
  (testing "set intersection with two arguments"
    (is (= {:tag :set-intersection
            :children [#{1 2} #{2 3}]}
           (bintersection #{1 2} #{2 3}))))
  (testing "set intersection with more than two arguments"
    (is (= {:tag :set-intersection
            :children [{:tag :set-intersection :children [ #{1 2} #{2 3}]}
                       #{3 4}]}
           (bintersection #{1 2} #{2 3} #{3 4})))))

(deftest intersect-sets-test
  (testing "generalised intersection over set of sets"
    (is (= {:tag :general-intersection
            :children [#{#{1} #{2} #{3}}]}
           (bintersect-sets #{#{1} #{2} #{3}})))))

(deftest intersection-pe-test
  (testing "generalised intersection with predicate"
    (is (= {:tag :intersection-pe
            :children [{:tag :list :children [:x]}
                       {:tag :and :children [{:tag :less :children [0 :x]}
                                             {:tag :less :children [:x 5]}]}
                       #{:x {:tag :mul :children [:x :x]}}]}
           (bintersection-pe [:x] (b< 0 :x 5) #{:x (b* :x :x)})))))

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
           (b+-> #{1 2} #{3 4}))))
  (testing "partial function with more than two arguments"
    (is (= {:tag :partial-fn
            :children [{:tag :partial-fn
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b+-> #{1 2} #{3 4} #{5 6})))))

(deftest total-fn-test
  (testing "total function with two arguments"
    (is (= {:tag :total-fn
            :children [#{1 2} #{3 4}]}
           (b--> #{1 2} #{3 4}))))
  (testing "total function with more than two arguments"
    (is (= {:tag :total-fn
            :children [{:tag :total-fn
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b--> #{1 2} #{3 4} #{5 6})))))

(deftest partial-surj-test
  (testing "partial surjection with two arguments"
    (is (= {:tag :partial-surjection
            :children [#{1 2} #{3 4}]}
           (b+->> #{1 2} #{3 4}))))
  (testing "partial surjection with more than two arguments"
    (is (= {:tag :partial-surjection
            :children [{:tag :partial-surjection
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b+->> #{1 2} #{3 4} #{5 6})))))

(deftest total-surj-test
  (testing "total surjection with two arguments"
    (is (= {:tag :total-surjection
            :children [#{1 2} #{3 4}]}
           (b-->> #{1 2} #{3 4}))))
  (testing "total surjection with more than two arguments"
    (is (= {:tag :total-surjection
            :children [{:tag :total-surjection
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b-->> #{1 2} #{3 4} #{5 6})))))

(deftest partial-inj-test
  (testing "partial injection with two arguments"
    (is (= {:tag :partial-injection
            :children [#{1 2} #{3 4}]}
           (b>+> #{1 2} #{3 4}))))
  (testing "partial injection with more than two arguments"
    (is (= {:tag :partial-injection
            :children [{:tag :partial-injection
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b>+> #{1 2} #{3 4} #{5 6})))))

(deftest total-inj-test
  (testing "total injection with two arguments"
    (is (= {:tag :total-injection
            :children [#{1 2} #{3 4}]}
           (b>-> #{1 2} #{3 4}))))
  (testing "total injection with more than two arguments"
    (is (= {:tag :total-injection
            :children [{:tag :total-injection
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b>-> #{1 2} #{3 4} #{5 6})))))

(deftest partial-bij-test
  (testing "partial bijection with two arguments"
    (is (= {:tag :partial-bijection
            :children [#{1 2} #{3 4}]}
           (b>+>> #{1 2} #{3 4}))))
  (testing "partial bijection with more than two arguments"
    (is (= {:tag :partial-bijection
            :children [{:tag :partial-bijection
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b>+>> #{1 2} #{3 4} #{5 6})))))

(deftest total-bij-test
  (testing "total bijection with two arguments"
    (is (= {:tag :total-bijection
            :children [#{1 2} #{3 4}]}
           (b>->> #{1 2} #{3 4}))))
  (testing "total bijection with more than two arguments"
    (is (= {:tag :total-bijection
            :children [{:tag :total-bijection
                        :children [#{1 2} #{3 4}]}
                        #{5 6}]}
           (b>->> #{1 2} #{3 4} #{5 6})))))


(deftest lambda-test
  (testing "lambda representation"
    (is (= {:tag :lambda
            :children [{:tag :list
                        :children [:x :y]}
                       {:tag :and
                        :children [{:tag :equals :children [:x :y]}
                                   {:tag :and
                                    :children [{:tag :less :children [2 :x]}
                                               {:tag :less :children [:x 5]}]}]}
                       :x]}
           (blambda [:x :y] (band (b= :x :y) (b< 2 :x 5)) :x)))))

(deftest apply-test
  (testing "function application representation"
    (is  (= {:tag :fn-application
             :children [#{[[1 2] 3]}
                        1
                        2]}
            (bapply #{[[1 2] 3]} 1 2)))))

(deftest implication-test
  (testing "implication representation with two arguments"
    (is (= {:tag :implication
            :children [:a :b]}
           (b=> :a :b))))
  (testing "implication representation with more than two arguments"
    (is (= {:tag :implication
            :children [{:tag :implication
                        :children [:a :b]}
                       :c]}
           (b=> :a :b :c)))))

(deftest forall-test
  (testing "universal quantification representation"
    (is (= {:tag :forall
            :children [{:tag :list
                        :children [:x]}
                       {:tag :implication
                        :children [:a :b]}]}
           (bforall [:x] (b=> :a :b))))))

(deftest exists-test
  (testing "existential quantification representation"
    (is (= {:tag :exists
            :children [{:tag :list
                        :children [:x]}
                       {:tag :member
                        :children [:x #{1}]}]}
           (bexists [:x] (bmember :x #{1}))))))

(deftest interval-test
  (testing "interval representation"
    (is (= {:tag :interval
            :children [1 4]}
           (binterval 1 4)))))

(deftest sequence-test
  (testing "sequence representation"
    (is (= {:tag :sequence
            :children [3 2 1]}
           (bsequence 3 2 1))))
  (testing "empty sequence"
    (is (= {:tag :sequence
            :children []}))))

(deftest iseq-test
  (testing "set of injective sequences"
    (is (= {:tag :iseq
            :children [{:tag :sequence
                       :children [4 2 3]}]}
           (biseq (bsequence 4 2 3))))))

(deftest iseq1-test
  (testing "set of non-empty injective sequences"
    (is (= {:tag :iseq1
            :children [{:tag :sequence
                       :children [4 2 3]}]}
           (biseq1 (bsequence 4 2 3))))))

(deftest perm-test
  (testing "set of bijective sequences"
    (is (= {:tag :perm
            :children [{:tag :sequence
                       :children [4 2 3]}]}
           (bperm (bsequence 4 2 3))))))

(deftest concat-test
  (testing "sequence concatenation with two args"
    (is (= {:tag :concat
            :children [{:tag :sequence :children [4 2]}
                       {:tag :sequence :children [5 9]}]}
           (bconcat (bsequence 4 2) (bsequence 5 9)))))
  (testing "sequence concatentation with more than two args"
    (is (= {:tag :concat
            :children [{:tag :concat
                        :children [{:tag :sequence :children [4 2]}
                                   {:tag :sequence :children [5 9]}]}
                       {:tag :sequence :children [3 1 4]}]}
           (bconcat (bsequence 4 2) (bsequence 5 9) (bsequence 3 1 4))))))

(deftest prepend-test
  (testing "prepend representation"
    (is (= {:tag :prepend
            :children [2 {:tag :sequence :children [1 4]}]}
           (b-> 2 (bsequence 1 4))))))

(deftest append-test
  (testing "append representation with two args"
    (is (= {:tag :append
            :children [{:tag :sequence :children [3]}
                       1]}
           (b<- (bsequence 3) 1))))
  (testing "append representation with more than two args"
    (is (= {:tag :append
            :children [{:tag :append :children [{:tag :sequence :children [3]}
                                                1]}
                       4]}
           (b<- (bsequence 3) 1 4)))))


(deftest reverse-test
  (testing "reverse representation"
    (is (= {:tag :reverse
            :children [{:tag :sequence :children [3 1 4]}]}
           (breverse (bsequence 3 1 4))))))

(deftest first-test
  (testing "first representation"
    (is (= {:tag :first
            :children [{:tag :sequence :children [3 1 4]}]}
           (bfirst (bsequence 3 1 4))))))

(deftest last-test
  (testing "last-representation"
    (is (= {:tag :last
            :children [{:tag :sequence :children [3 1 4]}]}
           (blast (bsequence 3 1 4))))))


(deftest front-test
  (testing "front-representation"
    (is (= {:tag :front
            :children [{:tag :sequence :children [3 1 4]}]}
           (bfront (bsequence 3 1 4))))))

(deftest tail-test
  (testing "tail representation"
    (is (= {:tag :tail
            :children [{:tag :sequence :children [3 1 4]}]}
           (btail (bsequence 3 1 4))))))

(deftest restrict-front-test
  (testing "take first n elements of sequence representation"
    (is (= {:tag :restrict-front
            :children [{:tag :sequence :children [3 1 4]}
                       2]}
           (brestrict-front (bsequence 3 1 4) 2)
           (btake 2 (bsequence 3 1 4))))))

(deftest restrict-tail-test
  (testing "drop first n elements of sequence representation"
    (is (= {:tag :restrict-tail
            :children [{:tag :sequence :children [3 1 4]}
                       2]}
           (brestrict-tail (bsequence 3 1 4) 2)
           (bdrop 2 (bsequence 3 1 4))))))

(deftest pow-test
  (testing "power representation with two arguments"
    (is (= {:tag :pow :children [2 3]}
           (b** 2 3))))
  (testing "power representation with more than two arguments"
    (is (= {:tag :pow
            :children [1
                       {:tag :pow :children [2 3]}]}
           (b** 1 2 3)))))


(deftest sigma-test
  (testing "set summation representation"
    (is (= {:tag :sigma
            :children [{:tag :list :children [:a]}
                       {:tag :and :children [{:tag :less :children [1 :a]}
                                             {:tag :less :children [:a 4]}]}
                       :a]}
           (bsigma [:a] (b< 1 :a 4) :a)))))


(deftest pi-test
  (testing "set product representation"
    (is (= {:tag :pi
            :children [{:tag :list :children [:a]}
                       {:tag :and :children [{:tag :less :children [1 :a]}
                                             {:tag :less :children [:a 4]}]}
                       :a]}
           (bpi [:a] (b< 1 :a 4) :a)))))

(deftest seq-test
  (testing "seq representation"
    (is (= {:tag :seq
            :children [{:tag :sequence :children [3 1 4]}]}
           (bseq (bsequence 3 1 4))))))

(deftest seq1-test
  (testing "seq1 representation"
    (is (= {:tag :seq1
            :children [{:tag :sequence :children [3 1 4]}]}
           (bseq1 (bsequence 3 1 4))))))

(deftest conc-test
  (testing "conc representation"
    (is (= {:tag :conc
            :children [{:tag :sequence :children [{:tag :sequence :children [3 1 4]}
                                                  {:tag :sequence :children [1 5 9 2]}]}]}
           (bconc (bsequence (bsequence 3 1 4) (bsequence 1 5 9 2)))))))

(deftest ite-test
  (testing "if then else representation"
    (is (= {:tag :if
            :children [{:tag :less :children [1 2]}
                       3
                       4]}
           (bif (b< 1 2) 3 4)))))


(deftest struct-test
  (testing "struct representation"
    (let [m {:a (bint-set) :b (bbool-set)}]
      (is (= {:tag :struct
              :children [{:tag :list :children (map (comp name first) (seq m))}
                         {:tag :list :children (map second (seq m))}]}
             (bstruct m))))))

(deftest record-test
  (testing "record representation"
    (let [m {:a (bint-set) :b (bbool-set)}]
      (is (= {:tag :record
              :children [{:tag :list :children (map (comp name first) (seq m))}
                         {:tag :list :children (map second (seq m))}]}
             (brecord m))))))


(deftest rec-get-test
  (testing "record get representation"
    (is (= {:tag :record-get
            :children [{:tag :record :children [{:tag :list :children ["a"]} 
                                                {:tag :list :children [1]}]}
                       "a"]}
           (brec-get (brecord "a" 1) "a")))))

(deftest pred-test
  (testing "the pred macro allows to write b-macrofied expressions
            and returns a function"
    (is (fn? (pred [] (+ 1 2)))))
  
  (testing "the pred macro has a parameter list just like fn"
    (is (fn? (pred [x y] (+ 1 2)))))

  (testing "the resulting function generates a representation
            which replaces the parameter symbols with the values provided"
    (is (= ((pred [x y] (< x y)) 1 2)
           {:tag :less :children [1 2]})))
  
  (testing "the pred macro flattens sets properly"
    (is (= (count ((pred [] #{:x :y})))
            2)))) 


(deftest enumerated-set-test
  (testing "alternative non-syntactical representation of a set"
    (is (= {:tag :enumerated-set
            :children [1 2 3]}
           (bset-enum 1 2 3)))))


(deftest tuple-test
  (testing "alternative non-syntactical representation of a tuple"
    (is (= {:tag :tuple
            :children [1 2]}
           (btuple 1 2)))))



(deftest string-test
  (testing "string expression representation"
    (is (= {:tag :string :children ["foo"]}
           (bstr "foo")))))


(deftest external-function
  (testing "representation of a function call, e.g. choose"
    (is (= {:tag :fn-call :children ["CHOOSE" #{1 2 3}]}
           (bcall "CHOOSE" #{1 2 3})))))

(deftest let-tests
  (testing "one binding and predicate"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo]}
                       {:tag :list :children [1]}
                       {:tag :less :children [:foo 2]}]}
           (blet-pred [:foo 1] (b< :foo 2)))))
  (testing "more bindings and a predicate"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo :bar]}
                       {:tag :list :children [1 2]}
                       {:tag :less :children [:foo :bar]}]}
           (blet-pred [:foo 1 :bar 2] (b< :foo :bar)))))
  (testing "one binding and expression"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo]}
                       {:tag :list :children [1]}
                       {:tag :plus :children [:foo 2]}]}
           (blet-pred [:foo 1] (b+ :foo 2)))))
  (testing "more bindings and an expression"
    (is (= {:tag :let-pred
            :children [{:tag :list :children [:foo :bar]}
                       {:tag :list :children [1 2]}
                       {:tag :plus :children [:foo :bar]}]}
           (blet-pred [:foo 1 :bar 2] (b+ :foo :bar))))))
