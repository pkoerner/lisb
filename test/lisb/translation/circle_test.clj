(ns lisb.translation.circle-test
  (:require [clojure.test :refer :all]
            [lisb.translation.lisb2ir :refer [b]]
            [lisb.translation.ast2lisb :refer [ast->lisb]]
            [lisb.translation.ir2ast :refer [ir->ast]]))

(deftest machine-test
  (testing "machine"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (machine
                   (machine-variant)
                   (machine-header :Empty []))))))

(deftest machine-clauses-test
  (testing "machine-clauses"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (constraints (= 1 1)))
              (b (sets (deferred-set :S) (enumerated-set :T :e1 :e2)))
              (b (constants :a :b :c))
              (b (properties (= true true)))
              ;(b (definitions))
              (b (variables :x :y))
              (b (invariant (= true true)))
              (b (assertions (= true true) (= false false)))
              (b (init (assign :x 0 :y 0)))
              (b (operations (operation () :inc () (assign :x (+ :x 1))) (operation () :dec () (assign :x (- :x 1)))))
              )))

(deftest substitutions-test
  (testing "substitutions"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b skip)
              (b (assign :x :E))
                  ;"f(x) := E"
              (b (becomes-element-of [:x] :S))
              (b (becomes-such [:x] (> :x 0)))
              (b (operation-call [:x] :OP [:y]))
              (b (parallel-substitution skip skip))
              (b (parallel-substitution skip skip skip))
              (b (sequence-substitution skip skip))
              (b (sequence-substitution skip skip skip))
              (b (any [:x] (> :x 0) skip))
              (b (let-sub [:x] (= :x 1) skip))
              (b (var-sub [:x] skip))
              (b (pre (= 1 2) skip))
              (b (assert (= 1 2) skip))
              (b (choice skip skip))
              (b (if-sub (= 1 2) skip))
              (b (if-sub (= 1 2) skip skip))
              (b (cond (= 1 2) skip (= 1 3) skip))
              (b (cond (= 1 2) skip (= 1 3) skip skip))
              (b (select (= 1 2) skip))
              (b (select (= 1 2) skip (assign :x 1)))
              (b (select (= 1 1) skip (= 2 2) skip))
              (b (select (= 1 1) skip (= 2 2) skip skip))
                  ;"CASE E OF EITHER m THEN G OR n THEN H END END"
                  ;"CASE E OF EITHER m THEN G OR n THEN H ELSE I END END"
                  )))

(deftest if-test
  (testing "if"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (if-expr (= 1 1) 2 3)))))

(deftest let-test
  (testing "let"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (let-expr [:x :y] (and (= :x 1) (= :y 2)) 3)))
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (let-pred [:x :y] (and (= :x 1) (= :y 2)) (= 0 0))))))

(deftest strings-test
  (testing "strings"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b "astring")
              (b string-set)
              (b (count-seq "s"))
              (b (reverse "s"))
              (b (concat "s" "t"))
              (b (conc (sequence "s" "t"))))))

(deftest struct-test
  (testing "structs"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (struct :n nat-set))
              (b (struct :n nat-set, :b bool-set))
              (b (record :n 1))
              (b (record :n 1, :b true))
              (b (rec-get :R :n)))))

(deftest sequences-test
  (testing "sequences"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (sequence))
              (b (sequence :E))
              (b (sequence :E :F))
              (b (seq :S))
              (b (seq1 :S))
              (b (iseq :S))
              (b (iseq1 :S))
              (b (perm :S))
              (b (count-seq :S))
              (b (concat :s :t))
              (b (concat :s :t :u))
              (b (concat :s :t :u :v))
              (b (cons :s :E))
              (b (cons :s :E :F))
              (b (cons :s :E :F :G))
              (b (append :s :E))
              (b (append :s :E :F))
              (b (append :s :E :F :G))
              (b (reverse :S))
              (b (first :S))
              (b (last :S))
              (b (drop-last :S))
              (b (rest :S))
              (b (conc :S))
              (b (take :n :s))
              (b (drop :n :s)))))

(deftest function-test
  (testing "functions"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (+-> :S :T))
              (b (--> :S :T))
              (b (+->> :S :T))
              (b (-->> :S :T))
              (b (>+> :S :T))
              (b (>-> :S :T))
              (b (>->> :S :T))
              (b (lambda [:x] (= 1 1) 1))
              ; TODO: handle calls
              ;(b (call :f :E))
              )))

(deftest relation-test
  (testing "relations"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (<-> :S :T))
              (b (total-relation :S :T))
              (b (surjective-relation :S :T))
              (b (total-surjective-relation :S :T))
              (b [:E :F])
              (b (dom :r))
              (b (ran :r))
              (b (identity :S))
              (b (<| :S :r))
              (b (<<| :S :r))
              (b (|> :r :S))
              (b (|>> :r :S))
              (b (inverse :r))
              (b (image :r :S))
              (b (<+ :r1 :r2))
              (b (>< :r1 :r2))
              (b (comp :r1 :r2))
              (b (|| :r1 :r2))
              (b (prj1 :S :T))
              (b (prj2 :S :T))
              (b (closure1 :r))
              (b (closure :r))
              (b (iterate :r :n))
              (b (fnc :r))
              (b (rel :r)))))

(deftest numbers-test
  (testing "numbers"
    (testing "expressions"
      (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
                (b 1)
                (b -1)
                (b (- :x))
                (b integer-set)
                (b natural-set)
                (b natural1-set)
                (b int-set)
                (b nat-set)
                (b nat1-set)
                (b (range 1 3))
                (b min-int)
                (b max-int)
                (b (max nat-set))
                (b (max 1 2 3))
                (b (min nat-set))
                (b (min 1 2 3))
                (b (pi [:z] (contains? nat-set  :z) 1))
                (b (sigma [:z] (contains? nat-set :z) 1)))
      (testing "arithmetic"
        (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
                  (b (+ 1 2))
                  (b (- 1 2))
                  (b (* 1 2))
                  (b (/ 1 2))
                  (b (** 1 2))
                  (b (mod 1 2))
                  (b (inc 1))
                  (b (dec 1)))))
    (testing "predicates"
      (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
                (b (> 1 2))
                (b (< 1 2))
                (b (>= 1 2))
                (b (<= 1 2))))))

(deftest sets-test
  (testing "sets"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b #{})
              (b #{:E})
              (b #{:E :F})
              (b (comp-set [:x] (contains? nat-set :x)))
              (b (pow #{}))
              (b (pow1 #{}))
              (b (fin #{}))
              (b (fin1 #{}))
              (b (count #{}))
              (b (* #{:E} #{:F}))
              (b (* #{:E} #{:F} #{:G}))
              (b (* #{:E} #{:F} #{:G} #{:H}))
              (b (union #{:E} #{:F}))
              (b (union #{:E} #{:F} #{:G}))
              (b (union #{:E} #{:F} #{:G} #{:H}))
              (b (intersection #{:E} #{:F}))
              (b (intersection #{:E} #{:F} #{:G}))
              (b (intersection #{:E} #{:F} #{:G} #{:H}))
              (b (- #{:E} #{:F}))
              (b (- #{:E} #{:F} #{:G}))
              (b (- #{:E} #{:F} #{:G} #{:H}))
              (b (unite-sets #{#{}}))
              (b (unite-sets #{#{:E}}))
              (b (unite-sets #{#{:E} #{:F}}))
              (b (intersect-sets #{#{}}))
              (b (intersect-sets #{#{:E}}))
              (b (intersect-sets #{#{:E} #{:F}}))
              (b (union-pe [:z] (contains? nat-set :z) 1))
              (b (intersection-pe [:z] (contains? nat-set :z) 1)))
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (contains? #{} 1))
              (b (not (contains? #{} 1)))
              (b (subset? #{:E} #{:G}))
              (b (not (subset? #{:E} #{:G})))
              (b (subset-strict? #{:E} #{:G}))
              (b (not (subset-strict? #{:E} #{:G})))
              (b (superset? #{:E} #{:G}))
              (b (not (superset? #{:E} #{:G})))
              (b (superset-strict? #{:E} #{:G}))
              (b (not (superset-strict? #{:E} #{:G}))))))

(deftest booleans-test
  (testing "booleans"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b true)
              (b false)
              (b bool-set)
              (b (pred->bool (= true true))))))

(deftest equality-predicates-test
  (testing "equality-predicates"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (= true false))
              (b (not= true false)))))

(deftest logical-predicates-test
  (testing "logical-predicates"
    (are [ir] (= ir (eval `(b ~(ast->lisb (ir->ast ir)))))
              (b (and (= 1 1) (= 2 2)))
              (b (or (= 1 1) (= 2 2)))
              (b (=> (= 1 1) (= 2 2)))
              (b (<=> (= 1 1) (= 2 2)))
              (b (not (= 1 1)))
              (b (for-all [:x] (=> (contains? nat-set :x) (< 0 :x))))
              (b (exists [:x] (and (= 1 1) (= 2 2)))))))
