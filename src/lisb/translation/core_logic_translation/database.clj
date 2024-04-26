(ns lisb.translation.core-logic-translation.database
  (:require [clojure.core.logic.pldb :as pldb] 
            [clojure.core.logic :refer :all]))



(pldb/db-rel matches ^:index lisbop ^:index irop)
(pldb/db-rel has-args ^:index op ^:index types)
(pldb/db-rel override op condition f)

; (defmacro to-db [functor m]
;   `(pldb/db
;     ~@(for [[k v] m]
;         [functor `(quote ~k) v])))
; 
; order 
; (def order
;   (into {} (map-indexed (fn [idx [sym k]] [k idx])
;    (partition 2 '[+ :add, - :sub, / :div, ** :pow, * :mul,
;         ; LOGICAL PREDICATES
;         mod :mod, and :and, not :not, or :or, => :implication, implication :implication,
;         <=> :equivalence, equivalence :equivalence, exists :exists, for-all :for-all,
;         ; EQUALITY
;         = :equals, not= :not-equals,
;         ; BOOLEANS
;         bool-set :bool-set, pred->bool :pred->bool,
;         ; SETS
;         comprehension-set :comprehension-set, pow :power-set, pow1 :power1-set,
;         fin :fin, fin1 :fin1, card :cardinality, cart-or-mult :cartesian-product-or-multiplication,
;         cartesian-product :cartesian-product, union :union, intersection :intersection, set- :difference,
;         member? :member, subset? :subset?, superset? :superset?, strict-subset? :strict-subset?, 
;         strict-superset? :strict-superset?, unite-sets :unite-sets, intersect-sets :intersect-sets
;         union-pe :union-pe, intersection-pe :intersection-pe
;         ; NUMBERS
;         integer-set :integer-set, natural-set :natural-set, int-set :int-set, nat-set :nat-set,
;         nat1-set :nat1-set, interval :interval, min-int :min-int, max-int :max-int,
;         > :greater, >= :greater-equals, < :less, <= :less-equals, max :max, min :min,
;         pi :product, sigma :sum, successor :successor, predecessor :predecessor, 
;         ; RELATIONS
;         <-> :relation, <<-> :total-relation, <->> :surjective-relation, <<->> :total-surjective-relation,
;         |-> :maplet, dom :dom, ran :ran, <| :domain-restriction, <<| :domain-subtraction,
;         |> :range-restriction, |>> :range-subtraction, inverse :inverse, image :image,
;         <+ :override, >< :direct-product, composition :composition, parallel-product :parallel-product,
;         prj1 :prj1, prj2 :prj2, closure :closure, closure1 :closure1, iterate :iterate,
;         fnc :fnc, rel :rel, 'id :id
;         ; FUNCTIONS
;         +-> :partial-fn, --> :total-fn, +->> :partial-surjection, -->> :total-surjection,
;         >+> :partial-injection, >-> :total-injection, >+>> :partial-bijection, >->> :total-bijection,
;         lambda :lambda, fn-call :fn-call, 
;         ; SEQUENCES
;         sequence :sequence, seq :seq, seq1 :seq1, iseq :iseq, iseq1 :iseq1, perm :perm,
;         size :size, concat :concat, -> :prepend, <- :append, reverse :reverse,
;         first :first, last :last, front :front, tail :tail, take :take, drop :drop,
;         conc :conc,
;         ; RECORDS
;         struct :struct, record :record, record-get :record-get,
;         ; STRINGS
;         string-set :string-set,
;         ; LET AND IF-THEN-ELSE
;         ite :if-then-else, let-in :let-in,
;         ; SUBSTITUTIONS
;         skip :skip, assign :assignment, becomes-element-of :becomes-element-of, becomes-such :becomes-such;
;         op-call :op-call
;         ;|| :parallel-substitution
;         parallel-sub :parallel-sub, sequential-sub :sequential-sub, any :any,
;         let-sub :let-sub, ;;id-vals als vec
;         var :var, pre :precondition, precondition :precondition, assert :assert, choice :choice
;         if-sub :if-sub, ;; mehrere arg-tags?
;         cond-sub :cond-sub, select :select, case :case
;         ; MACHINE REFERENCES
;         uses :uses, includes :includes, sees :sees, extends :extends, promotes :promotes,
;         ; MACHINE SECTION
;         constraints :constraints, sets :sets, constants :constants, properties :properties,
;         variables :variables, invariants :invariants, assertions :assertions, init :init,
;         operations :operations,
;         ; SET DEFINITIONS
;         deferred-set :deferred-set, enumerated-set :enumerated-set ;; lisb auch: id #{el1 el2}
;         ; OPERATIONS DEFINITIONS
;         op :op
;         ; DEFINITIONS
;         definitions :definitions, expression-definition :expression-definition, predicate-definition :predicate-definition;
;         substitution-definition :substitution-definition, file-definition :file-definition
;         ; MACHINE 
;         machine-reference :machine-reference, machine :machine, model :model;
;         system :system, refinement :refinement, implementation :implementation,
;         ; MACHINE NAME
;         name :name]))))
; 
; (clojure.pprint/pprint (let [matches (first (vals (first (vals facts_ops))))
;       has_args (first (vals (first (vals facts_args_ir)))) 
;       m1 (into {} (map (comp vec reverse) matches))
;       m2 (into {} (map (comp vec ) has_args))
;       merged (merge-with (fn [v1 v2] [v1 (vec v2)]) m1 m2)
;       data (sort-by (comp #(get order % 999) first) merged)
;       ]
;   data
;   (for [[x [y z]] data]
;     ['relation x (quote y) z]
;     )
;   ))


(pldb/db-rel rules ^:index tag ^:index lisb args)
(defmacro lisb-translation-rules->db [functor rule-tuples]
  `(pldb/db
     ~@(map (fn [[x y z]] [functor x `(quote ~y) z]) rule-tuples)))

(def rules-tag-sym-args
  (lisb-translation-rules->db rules
  ;; form: 3-tuple
  ;; first: IR-tag
  ;; second: internal DSL symbol
  ;; third: keywords in map, ordering is important and must align with the argument order in the internal DSL.
  ;;        If the keyword is wrapped in an additional vector, e.g.
  ;;             [:ids :pred [:subs]] 
  ;;        this means, all remaining arguments are collected as a collection associated with this keyword.
  [[:add + [[:nums]]]
   [:sub - [[:nums]]]
   [:div / [[:nums]]]
   [:pow ** [[:nums]]]
   [:mul * [[:nums]]]
   [:mod mod [[:nums]]]
   [:and and [[:preds]]]
   [:not not [:pred]]
   [:or or [[:preds]]]
   [:implication implication [[:preds]]]
   [:implication => [[:preds]]]
   [:equivalence <=> [[:preds]]]
   [:exists exists [:ids :pred]]
   [:for-all for-all [:ids :implication]]
   [:equals = [:left :right]]
   [:not-equals not= [:left :right]]
   [:bool-set bool-set []]
   [:pred->bool pred->bool [:pred]]
   [:comprehension-set comprehension-set [:ids :pred]]
   [:power-set pow [:set]]
   [:power1-set pow1 [:set]]
   [:fin fin [:set]]
   [:fin1 fin1 [:set]]
   [:cardinality card [:set]]
   [:cartesian-product-or-multiplication cart-or-mult [[:nums-or-sets]]]
   [:cartesian-product cartesian-product [[:sets]]]
   [:union union [[:sets]]]
   [:intersection intersection [[:sets]]]
   [:difference set- [[:sets]]]
   [:member member? [:elem :set]]
   [:subset? subset? [[:sets]]]
   [:superset? superset? [[:sets]]]
   [:strict-subset? strict-subset? [[:sets]]]
   [:strict-superset? strict-superset? [[:sets]]]
   [:unite-sets unite-sets [:set-of-sets]]
   [:intersect-sets intersect-sets [:set-of-sets]]
   [:union-pe union-pe [:ids :pred :expr]]
   [:intersection-pe intersection-pe [:ids :pred :expr]]
   [:integer-set integer-set []]
   [:natural-set natural-set []]
   [:int-set int-set []]
   [:nat-set nat-set []]
   [:nat1-set nat1-set []]
   [:interval interval [:from :to]]
   [:min-int min-int []]
   [:max-int max-int []]
   [:greater > [[:nums]]]
   [:greater-equals >= [[:nums]]]
   [:less < [[:nums]]]
   [:less-equals <= [[:nums]]]
   [:max max [:set]]
   [:min min [:set]]
   [:product pi [:ids :pred :expr]]
   [:sum sigma [:ids :pred :expr]]
   [:successor successor [:num]]
   [:predecessor predecessor [:num]]
   [:relation <-> [[:sets]]]
   [:total-relation <<-> [[:sets]]]
   [:surjective-relation <->> [[:sets]]]
   [:total-surjective-relation <<->> [[:sets]]]
   [:maplet |-> [:left :right]]
   [:dom dom [:rel]]
   [:ran ran [:rel]]
   [:domain-restriction <| [:rel :set]]
   [:domain-subtraction <<| [:rel :set]]
   [:range-restriction |> [:rel :set]]
   [:range-subtraction |>> [:rel :set]]
   [:inverse inverse [:rel]]
   [:image image [:rel]]
   [:override <+ [:rels]]
   [:direct-product >< [:rels]]
   [:composition composition [:rels]]
   [:parallel-product parallel-product [:rels]]
   [:prj1 prj1 [:set1 :set2]]
   [:prj2 prj2 [:set1 :set2]]
   [:closure closure [:rel]]
   [:closure1 closure1 [:rel]]
   [:iterate iterate [:rel :num]]
   [:fnc fnc [:rel]]
   [:rel rel [:rel]]
   [:id id [:set]]
   [:partial-fn +-> [[:sets]]]
   [:total-fn --> [[:sets]]]
   [:partial-surjection +->> [[:sets]]]
   [:total-surjection -->> [[:sets]]]
   [:partial-injection >+> [[:sets]]]
   [:total-injection >-> [[:sets]]]
   [:partial-bijection >+>> [[:sets]]]
   [:total-bijection >->> [[:sets]]]
   [:lambda lambda [:ids :pred :expr]]
   [:fn-call fn-call [:f :args]]
   [:sequence sequence [:elems]]
   [:seq seq [:set]]
   [:seq1 seq1 [:set]]
   [:iseq iseq [:set]]
   [:iseq1 iseq1 [:set]]
   [:perm perm [:set]]
   [:size size [:seq]]
   [:concat concat [:seqs]]
   [:prepend -> [:elem :seq]]
   [:append <- [:seq :elem]]
   [:reverse reverse [:seq]]
   [:first first [:seq]]
   [:last last [:seq]]
   [:front front [:seq]]
   [:tail tail [:seq]]
   [:take take [:num :seq]]
   [:drop drop [:num :seq]]
   [:conc conc [:seq-of-seqs]]
   [:struct struct [[:id-types]]]
   [:record record [[:id-vals]]]
   [:record-get record-get [:rec :id]]
   [:string-set string-set []]
   [:if-then-else ite [:cond :then :else]]
   [:let-in let-in [:id-vals :expr-or-pred]]
   [:skip skip []]
   [:assignment assign [:id-vals]]
   [:becomes-element-of becomes-element-of [:ids :set]]
   [:becomes-such becomes-such [:ids :set]]
   [:op-call op-call [:returns :op :args]]
   [:parallel-sub parallel-sub [[:subs]]]
   [:sequential-sub sequential-sub [[:subs]]]
   [:any any [:ids :pred [:subs]]]
   [:let-sub let-sub [:id-vals [:subs]]]
   [:var var [:ids [:subs]]]
   [:precondition pre [:pred [:subs]]]
   [:assert assert [:pred [:subs]]]
   [:choice choice [[:subs]]]
   [:if-sub if-sub [:cond :then :else]]
   [:cond cond [[:clauses]]]
   [:select select [[:clauses]]]
   [:case case [:expr [:clauses]]]
   [:uses uses [[:values]]]
   [:includes includes [[:values]]]
   [:sees sees [[:values]]]
   [:extends extends [[:values]]]
   [:promotes promotes [[:values]]]
   [:constraints constraints [[:values]]]
   [:sets sets [[:values]]]
   [:constants constants [[:values]]]
   [:properties properties [[:values]]]
   [:variables variables [[:values]]]
   [:invariants invariants [[:values]]]
   [:assertions assertions [[:values]]]
   [:init init [[:values]]]
   [:operations operations [[:values]]]
   [:deferred-set deferred-set [:id]]
   [:enumerated-set enumerated-set [:id :elems]]
   [:op op [:returns :name :args :body]]
   [:definitions definitions [[:values]]]
   [:expression-definition expression-definition [:name :args :expr]]
   [:predicate-definition predicate-definition [:name :args :pred]]
   [:substitution-definition substitution-definition [:name :args :sub]]
   [:file-definition file-definition [:file]]
   [:machine-reference machine-reference [:name :args :body]]
   [:machine machine [:name [:machine-clauses]]]
   [:model model [:name [:machine-clauses]]]
   [:system system [:name [:machine-clauses]]]
   [:refinement
    refinement
    [:name :abstract-machine-name [:machine-clauses]]]
   [:implementation
    implementation
    [:name :abstract-machine-name [:machine-clauses]]]
   [:name name [:name]]
   [:empty-sequence :empty-sequence []]
   [:has-args :cond :then]]))

;#_(spit "db.tmp" (with-out-str (clojure.pprint/pprint (let [matches (first (vals (first (vals facts_ops))))
;      has_args (first (vals (first (vals facts_args_ir)))) 
;      m1 (into {} (map (comp vec reverse) matches))
;      m2 (into {} (map (comp vec ) has_args))
;      merged (merge-with (fn [v1 v2] [v1 (vec v2)]) m1 m2)
;      data (sort-by (comp #(get order % 999) first) merged)
;      ]
;  data
;  (for [[x [y z]] data]
;    [x y z]
;    )
;  ))))

;(def facts_ops
;  (to-db matches 
;       {+ :add, - :sub, / :div, ** :pow, * :mul,
;        ; LOGICAL PREDICATES
;        mod :mod, and :and, not :not, or :or, => :implication, implication :implication,
;        <=> :equivalence, equivalence :equivalence, exists :exists, for-all :for-all,
;        ; EQUALITY
;        = :equals, not= :not-equals,
;        ; BOOLEANS
;        bool-set :bool-set, pred->bool :pred->bool,
;        ; SETS
;        comprehension-set :comprehension-set, pow :power-set, pow1 :power1-set,
;        fin :fin, fin1 :fin1, card :cardinality, cart-or-mult :cartesian-product-or-multiplication,
;        cartesian-product :cartesian-product, union :union, intersection :intersection, set- :difference,
;        member? :member, subset? :subset?, superset? :superset?, strict-subset? :strict-subset?, 
;        strict-superset? :strict-superset?, unite-sets :unite-sets, intersect-sets :intersect-sets
;        union-pe :union-pe, intersection-pe :intersection-pe
;        ; NUMBERS
;        integer-set :integer-set, natural-set :natural-set, int-set :int-set, nat-set :nat-set,
;        nat1-set :nat1-set, interval :interval, min-int :min-int, max-int :max-int,
;        > :greater, >= :greater-equals, < :less, <= :less-equals, max :max, min :min,
;        pi :product, sigma :sum, successor :successor, predecessor :predecessor, 
;        ; RELATIONS
;        <-> :relation, <<-> :total-relation, <->> :surjective-relation, <<->> :total-surjective-relation,
;        |-> :maplet, dom :dom, ran :ran, <| :domain-restriction, <<| :domain-subtraction,
;        |> :range-restriction, |>> :range-subtraction, inverse :inverse, image :image,
;        <+ :override, >< :direct-product, composition :composition, parallel-product :parallel-product,
;        prj1 :prj1, prj2 :prj2, closure :closure, closure1 :closure1, iterate :iterate,
;        fnc :fnc, rel :rel,
;        ; FUNCTIONS
;        +-> :partial-fn, --> :total-fn, +->> :partial-surjection, -->> :total-surjection,
;        >+> :partial-injection, >-> :total-injection, >+>> :partial-bijection, >->> :total-bijection,
;        lambda :lambda, fn-call :fn-call, id :id,
;        ; SEQUENCES
;        sequence :sequence, seq :seq, seq1 :seq1, iseq :iseq, iseq1 :iseq1, perm :perm,
;        size :size, concat :concat, -> :prepend, <- :append, reverse :reverse,
;        first :first, last :last, front :front, tail :tail, take :take, drop :drop,
;        conc :conc
;        ; RECORDS
;        struct :struct, record :record, record-get :record-get,
;        ; STRINGS
;        string-set :string-set,
;        ; LET AND IF-THEN-ELSE
;        ite :if-then-else, let-in :let-in,
;        ; SUBSTITUTIONS
;        skip :skip, assign :assignment, becomes-element-of :becomes-element-of, becomes-such :becomes-such;
;        op-call :op-call
;        ;|| :parallel-substitution
;        parallel-sub :parallel-sub, sequential-sub :sequential-sub, any :any,
;        let-sub :let-sub, ;;id-vals als vec
;        var :var, pre :precondition, precondition :precondition, assert :assert, choice :choice
;        if-sub :if-sub, ;; mehrere arg-tags?
;        cond-sub :cond-sub, select :select, case :case
;        ; MACHINE REFERENCES
;        uses :uses, includes :includes, sees :sees, extends :extends, promotes :promotes,
;        ; MACHINE SECTION
;        constraints :constraints, sets :sets, constants :constants, properties :properties,
;        variables :variables, invariants :invariants, assertions :assertions, init :init,
;        operations :operations,
;        ; SET DEFINITIONS
;        deferred-set :deferred-set, enumerated-set :enumerated-set ;; lisb auch: id #{el1 el2}
;        ; OPERATIONS DEFINITIONS
;        op :op
;        ; DEFINITIONS
;        definitions :definitions, expression-definition :expression-definition, predicate-definition :predicate-definition;
;        substitution-definition :substitution-definition, file-definition :file-definition
;        ; MACHINE 
;        machine-reference :machine-reference, machine :machine, model :model;
;        system :system, refinement :refinement, implementation :implementation,
;        ; MACHINE NAME
;        name :name  }))


(def facts_ops
  (pldb/db
   [matches '+ :add]
   [matches '- :sub]
   [matches '/ :div]
   [matches '** :pow]
   [matches '* :mul]
                 ; LOGICAL PREDICATES
   [matches 'mod :mod]
   [matches 'and :and]
   [matches 'not :not]
   [matches 'or :or]
   [matches '=> :implication]
   [matches 'implication :implication]
   [matches '<=> :equivalence]
   [matches 'equivalence :equivalence]
   [matches 'exists :exists]
   [matches 'for-all :for-all]
                 ; EQUALITY
   [matches '= :equals]
   [matches 'not= :not-equals]
                 ; BOOLEANS
   [matches 'bool-set :bool-set]
   [matches 'pred->bool :pred->bool]
                 ; SETS
   [matches 'comprehension-set :comprehension-set]
   [matches 'pow :power-set]
   [matches 'pow1 :power1-set]
   [matches 'fin :fin]
   [matches 'fin1 :fin1]
   [matches 'card :cardinality]
   [matches 'cart-or-mult :cartesian-product-or-multiplication]
   [matches 'cartesian-product :cartesian-product]
   [matches 'union :union]
   [matches 'intersection :intersection]
   [matches 'set- :difference]
   [matches 'member? :member]
   [matches 'subset? :subset?]
   [matches 'superset? :superset?]
   [matches 'strict-subset? :strict-subset?]
   [matches 'strict-superset? :strict-superset?]
   [matches 'unite-sets :unite-sets]
   [matches 'intersect-sets :intersect-sets]
   [matches 'union-pe :union-pe]
   [matches 'intersection-pe :intersection-pe]
                 ; NUMBERS
   [matches 'integer-set :integer-set]
   [matches 'natural-set :natural-set]
   [matches 'int-set :int-set]
   [matches 'nat-set :nat-set]
   [matches 'nat1-set :nat1-set]
   [matches 'interval :interval]
   [matches 'min-int :min-int]
   [matches 'max-int :max-int]
   [matches '> :greater]
   [matches '>= :greater-equals]
   [matches '< :less]
   [matches '<= :less-equals]
   [matches 'max :max]
   [matches 'min :min]
   [matches 'pi :product]
   [matches 'sigma :sum]
   [matches 'successor :successor]
   [matches 'predecessor :predecessor]
                 ; RELATIONS
   [matches '<-> :relation]
   [matches '<<-> :total-relation]
   [matches '<->> :surjective-relation]
   [matches '<<->> :total-surjective-relation]
   [matches '|-> :maplet]
   [matches 'dom :dom]
   [matches 'ran :ran]
   [matches '<| :domain-restriction]
   [matches '<<| :domain-subtraction]
   [matches '|> :range-restriction]
   [matches '|>> :range-subtraction]
   [matches 'inverse :inverse]
   [matches 'image :image]
   [matches '<+ :override]
   [matches '>< :direct-product]
   [matches 'composition :composition]
   [matches 'parallel-product :parallel-product]
   [matches 'prj1 :prj1]
   [matches 'prj2 :prj2]
   [matches 'closure :closure]
   [matches 'closure1 :closure1]
   [matches 'iterate :iterate]
   [matches 'fnc :fnc]
   [matches 'rel :rel]
                 ; FUNCTIONS
   [matches '+-> :partial-fn]
   [matches '--> :total-fn]
   [matches '+->> :partial-surjection]
   [matches '-->> :total-surjection]
   [matches '>+> :partial-injection]
   [matches '>-> :total-injection]
   [matches '>+>> :partial-bijection]
   [matches '>->> :total-bijection]
   [matches 'lambda :lambda]
   [matches 'fn-call :fn-call]
                 ; SEQUENCES
   [matches 'sequence :sequence]
   [matches 'seq :seq]
   [matches 'seq1 :seq1]
   [matches 'iseq :iseq]
   [matches 'iseq1 :iseq1]
   [matches 'perm :perm]
   [matches 'size :size]
   [matches 'concat :concat]
   [matches '-> :prepend]
   [matches '<- :append]
   [matches 'reverse :reverse]
   [matches 'first :first]
   [matches 'last :last]
   [matches 'front :front]
   [matches 'tail :tail]
   [matches 'take :take]
   [matches 'drop :drop]
                 ; RECORDS
   [matches 'struct :struct]
   [matches 'record :record]
   [matches 'record-get :record-get]
                 ; STRINGS
   [matches 'string-set :string-set]
                 ; LET AND IF-THEN-ELSE
   [matches 'ite :if-then-else]
   [matches 'let-in :let-in]
                 ; SUBSTITUTIONS
   [matches 'skip :skip]
   [matches 'assign :assignment]
   [matches 'becomes-element-of :becomes-element-of]
   [matches 'becomes-such :becomes-such]
   [matches 'op-call :op-call]
   ;[matches '|| :parallel-substitution]
   [matches 'parallel-sub :parallel-sub]
   [matches 'sequential-sub :sequential-sub]
   [matches 'any :any]
   [matches 'let-sub :let-sub] ;;id-vals als vec
   [matches 'var :var]
   [matches 'pre :precondition]
   [matches 'precondition :precondition]
   [matches 'assert :assert]
   [matches 'choice :choice]
   [matches 'if-sub :if-sub] ;; mehrere arg-tags?
   [matches 'cond-sub :cond-sub]
   [matches 'select :select]
   [matches 'case :case]
                 ; MACHINE CLAUSES
   [matches 'uses :uses]
   [matches 'includes :includes]
   [matches 'sees :sees]
   [matches 'extends :extends]
   [matches 'promotes :promotes]
                
                 ; MACHINE SECTION
   [matches 'constraints :constraints]
   [matches 'sets :sets]
   [matches 'constants :constants]
   [matches 'properties :properties]
   [matches 'variables :variables]
   [matches 'invariants :invariants]
   [matches 'assertions :assertions]
   [matches 'init :init]
   [matches 'operations :operations]
                 ; SET DEFINITIONS
   [matches 'deferred-set :deferred-set]
   [matches 'enumerated-set :enumerated-set] ;; lisb auch: id #{el1 el2}
                 ; OPERATIONS DEFINITIONS
   [matches 'op :op]
                 ; DEFINITIONS
   [matches 'definitions :definitions]
   [matches 'expression-definition :expression-definition]
   [matches 'predicate-definition :predicate-definition]
   [matches 'substitution-definition :substitution-definition]
   [matches 'file-definition :file-definition]
                 ; MACHINE 
   [matches 'machine-reference :machine-reference]
   [matches 'machine :machine]
   [matches 'model :model]
   [matches 'system :system]
   [matches 'refinement :refinement]
   [matches 'implementation :implementation]
                 ; MACHINE NAME
   [matches 'name :name]))


(def facts_args_ir
  (pldb/db
   [has-args :add '(:nums)]
   [has-args :sub '(:nums)]
   [has-args :pow '(:nums)]
   [has-args :div '(:nums)]
   [has-args :mul '(:nums)]
   [has-args :mod '(:nums)]
   [has-args :and '(:preds)]
   [has-args :not '(:pred)]
   [has-args :or '(:preds)]
   [has-args :implication '(:preds)]
   [has-args :equivalence '(:preds)]
   [has-args :exists '(:ids :pred)]
   [has-args :for-all '(:ids :implication)]
  ; EQUALITY
   [has-args :equals '(:left :right)]
   [has-args :not-equals '(:left :right)]
   ; BOOLEANS
   [has-args :bool-set '()]
   [has-args :pred->bool '(:pred)]
                   ; SETS
   [has-args :comprehension-set '(:ids :pred)]
   [has-args :power-set '(:set)]
   [has-args :power1-set '(:set)]
   [has-args :fin '(:set)]
   [has-args :fin1 '(:set)]
   [has-args :cardinality '(:set)]
   [has-args :cartesian-product-or-multiplication '(:nums-or-sets)] ; -> warum??
   [has-args :cartesian-product '(:sets)]
   [has-args :union '(:sets)]
   [has-args :intersection '(:sets)]
   [has-args :difference '(:sets)]
   [has-args :member '(:elem :set)]
   [has-args :subset? '(:sets)]
   [has-args :superset? '(:sets)]
   [has-args :strict-subset? '(:sets)]
   [has-args :strict-superset? '(:sets)]
   [has-args :unite-sets '(:set-of-sets)]
   [has-args :intersect-sets '(:set-of-sets)]
   [has-args :union-pe '(:ids :pred :expr)]
   [has-args :intersection-pe '(:ids :pred :expr)]
    ; NUMBERS
   [has-args :integer-set '()]
   [has-args :natural-set '()]
   [has-args :int-set '()]
   [has-args :nat-set '()]
   [has-args :nat1-set '()]
   [has-args :interval '(:from :to)]
   [has-args :min-int '()]
   [has-args :max-int '()]
   [has-args :greater '(:nums)]
   [has-args :greater-equals '(:nums)]
   [has-args :less '(:nums)]
   [has-args :less-equals '(:nums)]
   [has-args :max '(:set)]
   [has-args :min '(:set)]
   [has-args :product '(:ids :pred :expr)]
   [has-args :sum '(:ids :pred :expr)]
   [has-args :successor '(:num)]
   [has-args :predecessor '(:num)]
                   ; RELATIONS
   [has-args :relation '(:sets)]
   [has-args :total-relation '(:sets)]
   [has-args :surjective-relation '(:sets)]
   [has-args :total-surjective-relation '(:sets)]
   [has-args :maplet '(:left :right)]
   [has-args :dom '(:rel)]
   [has-args :ran '(:rel)]
   [has-args :id '(:set)]
   [has-args :domain-restriction '(:rel :set)]
   [has-args :domain-subtraction '(:rel :set)]
   [has-args :range-restriction '(:rel :set)]
   [has-args :range-subtraction '(:rel :set)]
   [has-args :inverse '(:rel)]
   [has-args :image '(:rel)]
   [has-args :override '(:rels)]
   [has-args :direct-product '(:rels)]
   [has-args :composition '(:rels)]
   [has-args :parallel-product '(:rels)]
   [has-args :prj1 '(:set1 :set2)]
   [has-args :prj2 '(:set1 :set2)]
   [has-args :closure '(:rel)]
   [has-args :closure1 '(:rel)]
   [has-args :iterate '(:rel :num)]
   [has-args :fnc '(:rel)]
   [has-args :rel '(:rel)]
    ; FUNCTIONS
   [has-args :partial-fn '(:sets)]
   [has-args :total-fn '(:sets)]
   [has-args :partial-surjection '(:sets)]
   [has-args :total-surjection '(:sets)]
   [has-args :partial-injection '(:sets)]
   [has-args :total-injection '(:sets)]
   [has-args :partial-bijection '(:sets)]
   [has-args :total-bijection '(:sets)]
   [has-args :lambda '(:ids :pred :expr)]
   [has-args :fn-call '(:f :args)]
    ; SEQUENCES
   [has-args :empty-sequence '()]
   [has-args :sequence '(:elems)]
   [has-args :seq '(:set)]
   [has-args :seq1 '(:set)]
   [has-args :iseq '(:set)]
   [has-args :iseq1 '(:set)]
   [has-args :perm '(:set)]
   [has-args :size '(:seq)]
   [has-args :concat '(:seqs)]
   [has-args :prepend '(:elem :seq)] ; -> soll das seq heißen?
   [has-args :append '(:seq :elem)]
   [has-args :reverse '(:seq)]
   [has-args :first '(:seq)]
   [has-args :last  '(:seq)]
   [has-args :front '(:seq)]
   [has-args :tail  '(:seq)]
   [has-args :conc '(:seq-of-seqs)]
   [has-args :take  '(:num :seq)]
   [has-args :drop  '(:num :seq)]
    ; RECORDS
   [has-args :struct '(:id-types)]
   [has-args :record '(:id-vals)]
   [has-args :record-get '(:rec :id)]
    ; STRINGS
   [has-args :string-set '()]
    ; LET AND IF-THEN-ELSE
   [has-args :if-then-else '(:cond :then :else)]
   [has-args :let-in '(:id-vals :expr-or-pred)]
    ; SUBSTITUTIONS
   [has-args :skip '()]
   [has-args :assignment '(:id-vals)]
   [has-args :becomes-element-of '(:ids :set)]
   [has-args :becomes-such '(:ids :set)]
   [has-args :op-call '(:returns :op :args)]
   [has-args :parallel-sub '(:subs)]
   [has-args :sequential-sub '(:subs)]
   [has-args :any '(:ids :pred :subs)]
   [has-args :let-sub '(:id-vals :subs)] ;;id-vals als vec
   [has-args :var '(:ids :subs)]
   [has-args :precondition '(:pred :subs)]
   [has-args :assert '(:pred :subs)]
   [has-args :choice '(:subs)]
   ;[has-args :if-sub '(:cond :then)]
   [has-args :if-sub '(:cond :then :else)]
   ;[has-args :has-args '(:cond :then)]
   [has-args :has-args '(:cond :then :else)];; mehrere arg-tags?
   [has-args :cond-sub '(:clauses)]
   [has-args :select  '(:clauses)]
   [has-args :case  '(:expr :clauses)] ; ?? 
    ; MACHINE CLAUSES
   [has-args :uses '(:values)]
   [has-args :includes '(:values)]
   [has-args :sees '(:values)]
   [has-args :extends '(:values)]
   [has-args :promotes '(:values)]
    ; MACHINE REFERENCE
   [has-args :name '(:name)]
   [has-args :name '(:name :args)] ;; machine name with parameters?
    ; MACHINE SECTION
   [has-args :constraints '(:values)]
   [has-args :sets '(:values)]
   [has-args :constants '(:values)]
   [has-args :properties '(:values)]
   [has-args :variables '(:values)]
   [has-args :invariants '(:values)]
   [has-args :assertions '(:values)]
   [has-args :init '(:values)]
   [has-args :operations '(:values)]
    ; SET DEFINITIONS
   [has-args :deferred-set '(:id)]
   [has-args :enumerated-set '(:id :elems)] ;; lisb auch: id #{el1 el2}
    ; OPERATIONS DEFINITIONS
   [has-args :op '(:returns :name :args :body)]
    ; DEFINITIONS
   [has-args :definitions '(:values)]
   [has-args :expression-definition '(:name :args :expr)]
   [has-args :predicate-definition '(:name :args :pred)]
   [has-args :substitution-definition '(:name :args :sub)]
   [has-args :file-definition '(:file)]
    ; MACHINE 
   [has-args :machine '(:name :machine-clauses)]
   [has-args :machine-reference '(:name :args :body)]
   ;; TODO: Reihenfolge anpassen
   [has-args :model '(:name :machine-clauses)]
   [has-args :system '(:name :machine-clauses)]
   [has-args :refinement '(:name :abstract-machine-name :machine-clauses)]
   [has-args :implementation '(:name :abstract-machine-name :machine-clauses )]
    ; MACHINE NAME 
   ))

