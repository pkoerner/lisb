# Work in progress lisb api doc

## Logical predicates
| B                                                    | Lisb                               | IR                                                                         | Description                |
|------------------------------------------------------|------------------------------------|----------------------------------------------------------------------------|----------------------------|
| `pred1 & pred2 & ...`                                | `(and & preds)`                    | `{:tag :and, :preds preds}`                                                | conjunction                |
| `pred1 or pred2 or ...`                              | `(or & preds)`                     | `{:tag :or, :preds preds}`                                                 | disjunction                |
| `pred1 => pred2 => ...`                              | `(=> & preds)`                     | `{:tag :implication, :preds preds}`                                        | implication                |
|                                                      | `(implication & preds)`            | `{:tag :implication, :preds preds}`                                        | sugar                      |
| `pred1 <=> pred2 <=> ...`                            | `(<=> & preds)`                    | `{:tag :equivalence, :preds preds}`                                        | equivalence                |
|                                                      | `(equivalence & preds)`            | `{:tag :equivalence, :preds preds}`                                        | sugar                      |
| `not(pred)`                                          | `(not pred)`                       | `{:tag :not, :pred pred}`                                                  | not                        |
| `!(id1,id2,...).(implication)`                       | `(for-all ids implication)`        | `{:tag :for-all, :ids ids, :implication implication}`                      | universal quantification   |
|                                                      | `(for-all ids premise conclusion)` | `{:tag :for-all, :ids ids, :implication (implication premise conclusion)}` | sugar                      |
| `#(id1,id2,...).(pred)`                              | `(exists ids pred)`                | `{:tag :exists, :ids ids, :pred pred}`                                     | existential quantification |

## Equality
| B                                                         | Lisb                  | IR                                                 | Description |
|-----------------------------------------------------------|-----------------------|----------------------------------------------------|-------------|
| `left = right`                                            | `(= left right)`      | `{:tag :equals, :left left, :right right}`         | equality    |
| `left /= right`                                           | `(not= left right)`   | `{:tag :not-equals, :left left, :right right}`     | disequality |
| `elem1/=elem2 & elem1/=elem3 & ... & elem2/=elem3 & ... ` | `(distinct? & elems)` | `(band (not= elem1 elem2) (not= elem1 elem3) ...)` | distinct    |

## Booleans
| B            | Lisb                | IR                               | Description                          |
|--------------|---------------------|----------------------------------|--------------------------------------|
| `TRUE`       | `true`              | `true`                           | true                                 |
| `FALSE`      | `false`             | `false`                          | false                                |
| `BOOL`       | `bool-set`          | `{:tag :bool-set}`               | set of boolean values ({TRUE,FALSE}) |
| `bool(pred)` | `(pred->bool pred)` | `{:tag :pred->bool, :pred pred}` | convert predicate into BOOL value    |

## Sets
| B                             | Lisb                                       | IR                                                                 | Description                                |
|-------------------------------|--------------------------------------------|--------------------------------------------------------------------|--------------------------------------------|
| `{elem1,elem2,...}`           | `#{elem1 elem2 ...}`                       | `#{elem1 elem2 ...}`                                               | set enumeration                            |
| `{id1,id2,...&#124;pred}`     | `(comprehension-set ids pred)`             | `{:tag :comprehension-set, :ids ids, :pred pred}`                  | comprehension set                          |
|                               | `#{ids &#124; pred}`                       | `{:tag :comprehension-set, :ids ids, :pred pred}`                  | sugar                                      |
| `POW(set)`                    | `(pow set)`                                | `{:tag :power-set, :set set}`                                      | power set                                  |
| `POW1(set)`                   | `(pow1 set)`                               | `{:tag :power1-set, :set set}`                                     | set of non-empty subsets                   |
| `FIN(set)`                    | `(fin set)`                                | `{:tag :fin, :set set}`                                            | set of all finite subsets                  |
| `FIN1(set)`                   | `(fin1 set)`                               | `{:tag :fin1, :set set}`                                           | set of all non-empty finite subsets        |
| `card(set)`                   | `(card set)`                               | `{:tag :cardinality, :set set}`                                    | cardinality                                |
| `set1*set2*...`               | `(cart-or-mult & sets)`                    | `{:tag :cartesian-product-or-multiplication, :nums-or-sets sets}`  | cartesian product or multiplication        |
|                               | `(cartesian-product & sets)`               | `{:tag :cartesian-product, :sets sets}`                            | cartesian product                          | 
|                               | `(x & sets)`                               | `{:tag :cartesian-product, :sets set}`                             | sugar                                      |
| `set1\/set2\/...`             | `(union & sets)`                           | `{:tag :union, :sets sets}`                                        | set union                                  |
| `set1/\set2/\...`             | `(intersection & sets)`                    | `{:tag :intersection, :sets sets}`                                 | set intersection                           |
| `set1-set2-...`               | `(set- & sets)`                            | `{:tag :difference, :sets sets}`                                   | set difference                             |
| `elem:set`                    | `(member? elem set)`                       | `{:tag :member, :elem elem, :set set}`                             | element of                                 |
|                               | `(in elem set)`                            | `{:tag :member, :elem elem, :set set}`                             | sugar                                      | 
|                               | `(contains? set & elems)`                  | `(and (member? elem1 set) (member? elem2 set) ...)`                | sugar                                      | 
| `elem/:set`                   | `(not (member? set elem))`                 |                                                                    | not element of                             | 
| `set1<:set2&set2<:...`        | `(subset? & sets)`                         | `{:tag :subset?, :sets sets}`                                      | subset of                                  |
|                               | `(superset? & sets)`                       | `{:tag :subset?, :sets (clore.core/reverse sets)}`                 | sugar                                      |
| `set1/<:set2&set2/<:...`      | `(not (subset? sets))`                     |                                                                    | not subset of                              |
| `set1<<:set2&set2<<:...`      | `(strict-subset? sets)`                    | `{:tag :strict-subset?, :sets sets}`                               | strict subset of                           |
|                               | `(strict-superset? sets)`                  | `{:tag :strict-subset?, :sets (clojure.core/reverse sets)}`        | sugar                                      |
| `set1/<<:set2&set2/<<:...`    | `(not (strict-subset? sets))`              |                                                                    | not strict subset of                       |
| `union(set-of-sets)`          | `(unite-sets set-of-sets)`                 | `{:tag :unite-sets, :set-of-sets set-of-sets}`                     | generalised union over sets of sets        |
| `inter(set-of-sets)`          | `(intersect-sets set-of-sets)`             | `{:tag :intersect-sets, :set-of-sets set-of-sets}`                 | generalised intersection over sets of sets |
| `UNION(ids).(pred&#124;expr)` | `(union-pe ids pred expr)`                 | `{:tag :union-pe, :ids ids, :pred pred, :expr expr}`               | generalised union with predicate           |
| `INTER(ids).(pred&#124;expr)` | `(intersection-pe ids pred expr)`          | `{:tag :intersection-pe, :ids ids, :pred pred, :expr expr}`        | generalised intersection with predicate    |

## Numbers
| B                       | Lisb                   | IR                                                  | Description                                                          |
|-------------------------|------------------------|-----------------------------------------------------|----------------------------------------------------------------------|
| `INTEGER`               | `integer-set`          | `{:tag :integer-set}`                               | set of integers                                                      |
| `NATURAL`               | `natural-set`          | `{:tag :natural-set}`                               | set of natural numbers                                               |
| `NATURAL1`              | `natural1-set`         | `{:tag :natural1-set}`                              | set of non-zero natural numbers                                      |
| `INT`                   | `int-set`              | `{:tag :int-set}`                                   | set of implementable integers (MININT..MAXINT)                       |
| `NAT`                   | `nat-set`              | `{:tag :nat-set}`                                   | set of implementable natural numbers                                 |
| `NAT1`                  | `nat1-set`             | `{:tag :nat1-set}`                                  | set of non-zero implementable natural numbers                        |
| `from..to`              | `(interval from to)`   | `{:tag :interval, :from from, :to to}`              | set of numbers from n to m                                           |
|                         | `(range from to)`      | `{:tag :interval, :from from, :to (dec to)}`        | clojure                                                              |
| `MININT`                | `min-int`              | `{:tag :min-int}`                                   | the minimum implementable integer                                    |
| `MAXINT`                | `max-int`              | `{:tag :max-int}`                                   | the maximum implementable integer                                    |
| `num1>num2>...`         | `(> & nums)`           | `{:tag :greater, :nums nums}`                       | greater than                                                         |
| `num1<num2<...`         | `(< & nums)`           | `{:tag :less, :nums nums}`                          | less than                                                            |
| `num1=>num2=>...`       | `(>= & nums)`          | `{:tag :greater-equals, :nums nums}`                | greater than or equal                                                |
| `nums1<=num2<=...`      | `(<= & nums)`          | `{:tag :less-equals, :nums nums}`                   | less than or equal                                                   |
| `max(S)`                | `(max S)`              | `{:tag :max, :set set}`                             | maximum of a set of numbers                                          |
| `max({m,n,o})`          | `(max m n o)`          | `{:tag :max, :set set}`                             | sugar                                                                |
| `min(S)`                | `(min S)`              | `{:tag :min, :set set}`                             | minimum of a set of numbers                                          |
| `min({m,n,o})`          | `(min m n o)`          | `{:tag :min, :set set}`                             | sugar                                                                |
| `num1+num2+...`         | `(+ & nums)`           | `{:tag :add, :nums nums}`                           | addition                                                             |
| `num1-num2-...`         | `(- & nums)`           | `{:tag :sub, :nums nums}`                           | difference                                                           |
| `num1*num2*...`         | `(cart-or-mul & nums)` | `{:tag :cart-or-mul, :nums-or-sets nums}`           | cartesian product or multiplication                                  |
|                         | `(* & elems)`          | `{:tag :mul, :elems elems}`                         | multiplication                                                       | 
| `num1/num2/...`         | `(div & nums)`         | `{:tag :div, :nums nums}`                           | division                                                             |
| `num1/num2/...`         | `(/ & nums)`           | `{:tag :div, :nums nums}`                           | sugar                                                                |
| `num1**num2**...`       | `(** & nums)`          | `{:tag :pow, :nums nums}`                           | power                                                                |
| `num1 mod num2 mod ...` | `(mod & nums)`         | `{:tag :mod, :nums nums}`                           | remainder of division                                                |
| `PI(z).(P&#124;E)`      | `(π #{z} P E)`         | `{:tag :product, :ids ids, :pred pred, :expr expr}` | Set product                                                          | 
|                         | `(pi #{z} P E)`        | `{:tag :product, :ids ids, :pred pred, :expr expr}` | sugar                                                                | 
| `SIGMA(z).(P&#124;E)`   | `(Σ #{z} P E)`         | `{:tag :sum, :ids ids, :pred pred, :expr expr}`     | Set summation                                                        |
|                         | `(sigma #{z} P E)`     | `{:tag :sum, :ids ids, :pred pred, :expr expr}`     | sugar                                                                | 
| `succ(n)`               | `(successor n)`        | `{:tag :successor, :num num}`                       | successor (n+1)                                                      |
|                         | `(inc n)`              | `{:tag :successor, :num num}`                       | sugar                                                                |
| `pred(n)`               | `(predecessor n)`      | `{:tag :predecessor, :num num}`                     | predecessor (n-1)                                                    |
|                         | `(dec n)`              | `{:tag :predecessor, :num num}`                     | sugar                                                                |
| `2`                     | `2`                    | `2`                                                 | integer literal                                                      |
| `-2`                    | `-2`                   | `-2`                                                | integer literal                                                      |
| `0xF`                   | `15`                   | `15`                                                | hexadecimal literal (cannot be retranslated)                         |

## Relations
| B                                                    | Lisb                                | IR                                               | Description                                                      |
|------------------------------------------------------|-------------------------------------|--------------------------------------------------|------------------------------------------------------------------|
| `set1<->set2<->...`                                  | `(<-> & sets)`                      | `{:tag :relation, :sets sets}`                   | relation                                                         |
| `set1<->set2<->...`                                  | `(relation & sets)`                 | `{:tag :relation, :sets sets}`                   | relation                                                         |
| `set1<<->set2<<->...`                                | `(<<-> & sets)`                     | `{:tag :total relation, :sets sets}`             | total relation                                                   |
| `set1<->>set2<<->...`                                | `(<->> & sets)`                     | `{:tag :surjective relation , :sets sets}`       | surjective relation                                              |
| `set1<<->>set2<<->...`                               | `(<<->> & sets)`                    | `{:tag :total surjective relation, :sets sets}`  | total surjective relation                                        |
| <code>left&#124;->right</code>                       |  <code>(&#124;-> left right)</code> | `{:tag :maplet, :left left, :right right}`       | maplet                                                           |
|                                                      | `(maplet left right)`               | `{:tag :maplet, :left left, :right right}`       | sugar                                                            | 
| `dom(rel)`                                           | `(dom rel)`                         | `{:tag :dom, :rel rel}`                          | domain of relation                                               |
| `ran(rel)`                                           | `(ran rel)`                         | `{:tag :ran, :rel rel}`                          | range of relation                                                |
| `id(set)`                                            | `(id set)`                          | `{:tag :id, :set set}`                           | identity relation                                                |
| <code>set<&#124;rel</code>                           | <code>(<&#124; set rel)</code>      | `{:tag :domain-restriction, :rel rel, :set set}` | domain restriction                                               | 
|                                                      | `(domain-restriction set rel)`      | `{:tag :domain-restriction, :rel rel, :set set}` | sugar                                                            | 
| <code>set<<&#124;rel</code>                          | <code>(<<&#124; set rel)</code>     | `{:tag :domain-subtraction, :rel rel, :set set}` | domain subtraction                                               | 
|                                                      |`(domain-subtraction set rel)`       | `{:tag :domain-subtraction, :rel rel, :set set}` | sugar                                                            | 
| <code>rel&#124;>set</code>                           | <code>(&#124;> rel set)</code>      | `{:tag :range-restriction, :rel rel, :set set}`  | range restriction                                                | 
|                                                      |`(range-restriction set rel)`        | `{:tag :range-restriction, :rel rel, :set set}`  | sugar                                                            | 
| <code>rel&#124;>>set</code>                          | <code>(&#124;>> rel set)</code>     | `{:tag :range-subtraction, :rel rel, :set set}`  | range subtraction                                                | 
|                                                      | `(range-subtraction set rel)`       | `{:tag :range-subtraction, :rel rel, :set set}`  | sugar                                                            | 
| `rel~`                                               | `(inverse rel)`                     | `{:tag :inverse, :rel rel}`                      | inverse of relation                                              |
| `rel[set]`                                           | `(image rel set)`                   | `{:tag :image, :rel rel, :set set}`              | relational image                                                 |
| `rel1<+rel2<+...`                                    | `(override & rels)`                 | `{:tag :override, :rels rels}`                   | relational overriding (r2 overrides r1)                          |
|                                                      | `(<+ & rels)`                       | `{:tag :override, :rels rels}`                   | sugar                                                            |
| `rel1><rel2><...`                                    | `(direct-product & rels)`           | `{:tag :direct-product, :rels rels}`             | direct product {x,(y,z) &#124; x,y:r1 & x,z:r2}                  |
|                                                      | `(>< & rels)`                       | `{:tag :direct-product, :rels rels}`             | sugar                                                            |
| `((rel1;rel2);...)`                                  | `(composition & rels)`              | `{:tag :composition, :rels rels}`                | relational composition {x,y&#124; x&#124;->z:r1 & z&#124;->y:r2} |
| <code>((rel1&#124;&#124;rel2)&#124;&#124;...)</code> | `(parallel-product & rels)`         | `{:tag :parallel-product, :rels rels}`           | parallel product {((x,v),(y,w)) &#124; x,y:r1 & v,w:r2}          | 
| `prj1(set1, set2)`                                   | `(prj1 set1 set2)`                  | `{:tag :prj1, :set1 set1, :set2 set2}`           | projection function (usage prj1(Dom,Ran)(Pair))                  | 
| `prj2(set1, set2)`                                   | `(prj2 set1 set2)`                  | `{:tag :prj2, :set1 set1, :set2 set2}`           | projection function (usage prj2(Dom,Ran)(Pair))                  | 
| `closure1(rel)`                                      | `(closure1 rel)`                    | `{:tag :closure1, :rel rel}`                     | transitive closure                                               |
| `closure(rel)`                                       | `(closure rel)`                     | `{:tag :closure, :rel rel}`                      | reflexive & transitive closure                                   |
| `iterate(rel,num)`                                   | `(iterate rel num)`                 | `{:tag :iterate, :rel rel, :num num}`            | iteration of r with n>=0                                         |
| `fnc(rel)`                                           | `(fnc rel)`                         | `{:tag :fnc, :rel rel}`                          | translate relation A<->B into function A+->POW(B)                |
| `rel(rel)`                                           | `(rel rel)`                         | `{:tag :rel, :rel rel}`                          | translate relation A<->POW(B) into relation A<->B                |

## Functions
| B                                | Lisb                          | IR                                                 | Description          |
|----------------------------------|-------------------------------|----------------------------------------------------|----------------------|
| `set1+->set2+->...`              | `(+-> & sets)`                | `{:tag :partial-fn, :sets sets}`                   | partial function     |
|                                  | `(partial-function & sets)`   | `{:tag :partial-fn, :sets sets}`                   | sugar                |
| `set1-->set2-->...`              | `(--> & sets)`                | `{:tag :total-fn, :sets sets}`                     | total function       |
|                                  | `(total-function & sets)`     | `{:tag :total-fn, :sets sets}`                     | sugar                |
| `set1+->>set2+->>...`            | `(+->> & sets)`               | `{:tag :partial-surjection, :sets sets}`           | partial surjection   |
|                                  | `(partial-surjection & sets)` | `{:tag :partial-surjection, :sets sets}`           | sugar                |
| `set1-->>set2-->>...`            | `(-->> & sets)`               | `{:tag :total-surjection, :sets sets}`             | total surjection     |
|                                  | `(total-surjection & sets)`   | `{:tag :total-surjection, :sets sets}`             | sugar                |
| `set1>+>set2>+>...`              | `(>+> & sets)`                | `{:tag :partial-injection, :sets sets}`            | partial injection    |
|                                  | `(partial-injection & sets)`  | `{:tag :partial-injection, :sets sets}`            | sugar                |
| `set1>->set2>->...`              | `(>-> & sets)`                | `{:tag :total-injection, :sets sets}`              | total injection      |
|                                  | `(total-injection & sets)`    | `{:tag :total-injection, :sets sets}`              | sugar                |
| `set1>+>>set2>+>>...`            | `(>+>> & sets)`               | `{:tag :partial-bijection, :sets sets}`            | partial bijection    |
|                                  | `(partial-bijection & sets)`  | `{:tag :partial-bijection, :sets sets}`            | sugar                |
| `set1>->>set2>->>...`            | `(>->> & sets)`               | `{:tag :total-bijection, :sets sets}`              | total bijection      |
|                                  | `(total-bijection & sets)`    | `{:tag :total-bijection, :sets sets}`              | sugar                |
| `%id1,id2,... .(pred&#124;expr)` | `(lambda ids pred expr)`      | `{:tag :lambda, :ids ids, :pred pred, :expr expr}` | lambda abstraction   |
| `f(arg1,arg2,...)`               | `(fn-call f & args)`          | `{:tag :fn-call, :f f, :args args}`                | function application |

<!-- | `%id1,id2,... .(id1:type1&id2:type2&... &#124;expr)`        | `(fn [id-types] expr)`   | `{:tag :fn, :id-types id-types, :expr expr}`       | sugar                | -->

## Sequences
| B                        | Lisb                  | IR                                       | Description                               |
|--------------------------|-----------------------|------------------------------------------|-------------------------------------------|
| `<> or []`               | `(sequence)`          | `{:tag :empty-sequence}`                 | empty sequence                            | <!-- [] in lisb and ir? -->
| `[elem1,elem2,...]`      | `(sequence & elemes)` | `{:tag :sequence, :elems elems}`         | constructed sequence                      | <!-- [elem1,elem2,..] in lisb and ir? -->
| `seq(set)`               | `(seq set)`           | `{:tag :seq, :set set}`                  | set of sequences over Sequence            |
| `seq1(set)`              | `(seq1 set)`          | `{:tag :seq1, :set set}`                 | set of non-empty sequences over Sequence  |
| `iseq(set)`              | `(iseq set)`          | `{:tag :iseq, :set set}`                 | set of injective sequences                |
| `iseq1(set)`             | `(iseq1 set)`         | `{:tag :iseq1, :set set}`                | set of non-empty injective sequences      |
| `perm(set)`              | `(perm set)`          | `{:tag :perm, :set set}`                 | set of bijective sequences (permutations) |
| `size(seq)`              | `(size seq)`          | `{:tag :size, :seq seq}`                 | size of sequence                          |
| `seq1^seq2...`           | `(concat seqs)`       | `{:tag :concat, :seqs seqs}`             | concatenation                             |
| `elem->seq`              | `(-> elem seq)`       | `{:tag :prepend, :elem elem, :set seq}`  | prepend element                           |
|                          | `(prepend elem seq)`  | `{:tag :prepend, :elem elem, :set seq}`  | prepend element                           |
| `(seq<-elem1)<-elem2...` | `(<- seq elems)`      | `{:tag :append, :set seq, :elem elems}`  | append element                            |
|                          | `(append seq elems)`  | `{:tag :append, :set seq, :elem elems}`  | append element                            |
| `rev(seq)`               | `(reverse seq)`       | `{:tag :reverse, :seq seq}`              | reverse of sequence                       |
| `first(seq)`             | `(first seq)`         | `{:tag :first, :seq seq}`                | first element                             |
| `last(seq)`              | `(last seq)`          | `{:tag :last, :seq seq}`                 | last element                              |
| `front(seq)`             | `(front seq)`         | `{:tag :front, :seq seq}`                | front of sequence (all but last element)  |
|                          | `(drop-last seq)`     | `{:tag :front, :seq seq}`                | clojure                                   |
| `tail(seq)`              | `(tail seq)`          | `{:tag :tail, :seq seq}`                 | tail of sequence (all but first element)  |
|                          | `(rest seq)`          | `{:tag :tail, :seq seq}`                 | clojure                                   |
| `conc(seq-of-seqs)`      | `(conc seq-of-seqs)`  | `{:tag :conc, :seq-of-seqs seq-of-seqs}` | concatenation of sequence of sequences    |
| `seq/&#124;\num`         | `(take num seq)`      | `{:tag :take, :num num, :seq seq}`       | take first n elements of sequence         |
| `seq\&#124;/num`         | `(drop num seq)`      | `{:tag :drop, :num num, :seq seq}`       | drop first n elements from sequence       |

## Records
| B                                 | Lisb                  | IR                                     | Description                                          |
|-----------------------------------|-----------------------|----------------------------------------|------------------------------------------------------|
| `struct(id1:type1,id2:type2,...)` | `(struct & id-types)` | `{:tag :struct, :id-types id-types}`   | set of records with given fields and field types     |
| `rec(id1:val1,id2:val2,...)`      | `(record & id-vals)`  | `{:tag :record, :id-vals id-vals}`     | construct a record with given field names and values |
| `rec'id`                          | `(record-get rec id)` | `{:tag :record-get, :rec rec, :id id}` | get value of field with name ID                      |

## Strings
| B               | Lisb           | IR                   | Description                                                           |
|-----------------|----------------|----------------------|-----------------------------------------------------------------------|
| `"astring"`     | `"astring"`    | `"astring"`          | a specific (single-line) string value                                 |
| `'''astring'''` |                |                      | an alternate way of writing (multi-line) strings, no need to escape " |
| `STRING`        | `string-set`   | `{:tag :string-set}` | the set of all strings                                                |

## Reals

## Trees

## Let and If-Then-Else
| B                                                                | Lisb                            | IR                                                              | Description                                 |
|------------------------------------------------------------------|---------------------------------|-----------------------------------------------------------------|---------------------------------------------|
| `IF cond THEN then ELSE else END`                                | `(ite cond then else)`          | `{:tag :if-then-else, :cond cond, :then then, :else else}`      | conditional for expressions and predicates  | <!-- detect and rewrite to if -->
| `LET id1,id2,... BE id1=val1 & id2=val2 ... IN expr-or-pred END` | `(let-in id-vals expr-or-pred)` | `{:tag :let-in, :id-vals id-vals, :expr-or-pred :expr-or-pred}` | let for expression and predicates           |

## Substitutions
| B                                                                                  | Lisb                                          | IR                                                      | Description                            |
|------------------------------------------------------------------------------------|-----------------------------------------------|---------------------------------------------------------|----------------------------------------|
| `skip`                                                                             | `skip`                                        | `{:tag :skip}`                                          | no operation                           |
| `id1,id2,... := val1,val2,...`                                                     | `(assign & id-vals)`                          | `{:tag :assignment, :id-vals id-vals}`                  | assignment                             |
|                                                                                    | `(set! & id-vals)`                            | `{:tag :assignment, :id-vals id-vals}`                  | sugar                                  |
| `f(x) := E`                                                                        | `(assign (fn-call f x) E) `                   | `(bassign (bfn-call f x) E`                             | functional override                    |
| `ids :: set`                                                                       | `(becomes-element-of ids set)`                | `{:tag :becomes-element-of, :ids ids, :set set}`        | choice from set                        |
|                                                                                    | `(becomes-member ids set)`                    | `{:tag :becomes-element-of, :ids ids, :set set}`        | sugar                                  |
| `ids : (pred)`                                                                     | `(becomes-such ids set)`                      | `{:tag :becomes-such, :ids ids, :pred pred}`            | choice by pred (constraining ids)      |
| `return1,return2,... <-- op(arg1,arg2,...)`                                        | `(<-- returns (op-call op args))`             | `{:tag :op-call, :returns returns, :op op, :args args}` | call operation and assign return value |
| `sub1&#124;&#124;sub2&#124;...`                                                    | `(&#124;&#124; & subs)`                       | `{:tag :parallel-substitution, :subs subs}`             | parallel substitution                  |
|                                                                                    | `(parallel-sub & subs)`                       | `{:tag :parallel-substitution, :subs subs}`             | sugar                                  |
| `sub1;sub2;...`                                                                    | `(sequential-sub & subs)`                     | `{:tag :sequential-substitution, :subs subs}`           | sequential composition                 |
| `ANY id1,id2,... WHERE pred THEN subs END`                                         | `(any ids pred subs)`                         | `{:tag :any, :ids ids, :pred pred, :subs subs}`         | non deterministic choice               |
| `LET id1,id2,... BE id1=val1&id2=val2... IN subs END`                              | `(let-sub [id1 val1 id2 val2 ...] subs)`      | `{:tag :let-sub, :id-vals id-vals, :subs subs}`         |                                        |
| `VAR x,... IN subs END`                                                            | `(var ids subs`                               | `{:tag :var, :ids ids, :subs subs}`                     | generate local variables               |
| `PRE pred THEN subs END`                                                           | `(pre pred & subs)`                           | `{:tag :precondition, :pred pred, :subs subs}`          |                                        |
| `ASSERT pred THEN subs END`                                                        | `(assert pred & subs)`                        | `{:tag :assert, :pred pred, :subs subs}`                |                                        |
| `CHOICE sub1 OR sub2 OR ... END`                                                   | `(choice & subs)`                             | `{:tag :choice, :subs subs}`                            |                                        |
| `IF cond THEN then END`                                                            | `(if-sub cond then)`                          | `{:tag :if-sub, :cond cond, :then then}`                |                                        |
| `IF cond THEN then ELSE else END`                                                  | `(if-sub cond then else)`                     | `{:tag :if-sub, :cond cond, :then then, :else else}`    |                                        |
| `IF cond1 THEN sub1 ELSIF cond2 THEN sub2 ... END`                                 | `(cond-sub & clauses)`                        | `{:tag :cond-sub, :clauses clauses}`                    |                                        |
| `IF cond1 THEN sub2 ESLIF cond2 THEN sub2 ... ELSE else-sub END`                   | `(cond-sub & clauses)`                        | `{:tag :cond-sub, :clauses clauses}`                    |                                        |
| `SELECT cond1 THEN sub1 WHEN cond2 THEN sub2 ... END`                              | `(select & clauses)`                          | `{:tag :select, :clauses clauses}`                      |                                        |
| `SELECT cond1 THEN sub1 WHEN cond2 THEN sub2 ... ELSE sub-else END`                | `(select & clauses)`                          | `{:tag :select, :clauses clauses}`                      |                                        |
| `CASE expr OF EITHER cond1 THEN sub1 OR cond2 THEN sub2 ... END END`               | `(case expr & cases)`                         |                                                         |                                        |
| `CASE expr OF EITHER cond1 THEN sub1 OR cond2 THEN sub2 ... ELSE sub-else END END` | `(case expr & case)`                          |                                                         |                                        |

## Machine clauses
### Machine inclusion
| B                                | Lisb                              | IR                                            | Description                          |
|----------------------------------|-----------------------------------|-----------------------------------------------|--------------------------------------|
| `USES mch-name1,mch-name2,...`   | `(uses & machine-names)`          | `{:tag :uses :values machine-names}`          |                                      |
| `INDLUDES mch-ref1,mch-ref2,...` | `(includes & machine-references)` | `{:tag :includes :values machine-references}` |                                      |
| `SEES mch-name1,mch-name2,...`   | `(sees & machine-names)`          | `{:tag :sees :values machine-names}`          |                                      |
| `EXTENDS mch-ref1,mch-ref2,...`  | `(extends & machine-references)`  | `{:tag :extends :values machine-references}`  |                                      |
| `PROMOTES op1,op2,...`           | `(promotes & ops)`                | `{:tag :promotes :values ops}`                |                                      |
### Machine reference
| B                     | Lisb            | IR                                                  | Description                  |
|-----------------------|-----------------|-----------------------------------------------------|------------------------------|
| `name`                | `name`          | `{:tag :machine-reference, :name name}`             | machine name                 |
| `name(arg1,arg2,...)` | `[name & args]` | `{:tag :machine-reference, :name name, :args args}` | machine name with parameters |
### Machine section
| B                                  | Lisb                     | IR                                    | Description                          |
|------------------------------------|--------------------------|---------------------------------------|--------------------------------------|
| `CONSTRAINTS pred1 & pred2 & ...`  | `(constraints & preds)`  | `{:tag :constraints, :values preds}`  | contraints                           |
| `SETS set-def1;set-def2;...`       | `(sets & set-defs)`      | `{:tag :sets, :values set-defs}`      | sets                                 |
| `CONSTANTS id1,id2,...`            | `(constants & ids)`      | `{:tag :constants, :values ids}`      | constants                            |
| `CONCRETE_CONSTANTS cx,cy,..`      |                          |                                       | synonym for constants                |
| `PROPERTIES pred1 & pred2 & ...`   | `(properties preds)`     | `{:tag :properties, :values preds}`   | properties                           |
| `VARIABLES id1,id2,...`            | `(variables & ids)`      | `{:tag :variables, :values ids}`      | variables                            |
| `CONCRETE_VARIABLES cv,cw,...`     |                          |                                       | synonym for variables                |
| `INVARIANT pred1 & pred2 & ...`    | `(invariants & preds)`   | `{:tag :invariants, :values preds}`   | invariant                            |
| `ASSERTIONS pred1;pred2;...`       | `(assertions & preds)`   | `{:tag :assertions, :values preds}`   | assertions                           |
| `INITIALISATION subs1;subs2;...`   | `(init & subs)`          | `{:tag :init, :values subs}`          | initialisation                       |
| `OPERATIONS op-def1;op-def2;...`   | `(operations & op-defs)` | `{:tag :operations, :values op-defs}` | operations                           |
#### Set definitions
| B                      | Lisb                          | IR                                             | Description    |
|------------------------|-------------------------------|------------------------------------------------|----------------|
| `id`                   | `id`                          | `{:tag :deferred-set, :id id}`                 | deferred set   |
|                        | `(deferred-set id)`           | `{:tag :deferred-set, :id id}`                 | deferred set   | 
| `id={elem1,elem2,...}` | `id #{elem1 elem2 ...}`       | `{:tag :enumerated-set, :id id, :elems elems}` | enumerated set |
|                        | `(enumerated-set id & elems)` | `{:tag :enumerated-set, :id id, :elems elems}` | enumerated set | 
#### Operation definitions
| B                                                    | Lisb                             | IR                                                                     | Description                           |
|------------------------------------------------------|----------------------------------|------------------------------------------------------------------------|---------------------------------------|
| `name = body`                                        | `(name [] body)`                 | `{:tag :op, :returns [], :name name, :args [], :body body}`            | operation                             | 
| `name(arg1,arg2,...) = body`                         | `(name args body)`               | `{:tag :op, :returns [], :name name, :args args, :body body}`          | operation with parameters             | 
| `return1,return2,... <-- name = body`                | `(<-- returns (name [] body))`   | `{:tag :op, :returns return-vals, :name name, :args [], :body body}`   | operation with returns                |
| `return1,return2,... <-- name(arg1,arg2,...) = body` | `(<-- returns (name args body))` | `{:tag :op, :returns return-vals, :name name, :args args, :body body}` | operation with parameters and returns |
### Definitions
| B                          | Lisb                   | IR                                 | Description |
|----------------------------|------------------------|------------------------------------|-------------|
| `DEFINITIONS def1;def2...` | `(definitions & defs)` | `{:tag :definitions :values defs}` |             |
#### Definition definitions
| B                             | Lisb                                      | IR                                                               | Description |
|-------------------------------|-------------------------------------------|------------------------------------------------------------------|-------------|
| `name == expr`                | `(expression-definition name [] expr)`    | `{:tag :expression-definition :name name :args [] :expr expr}`   |             |
| `name(arg1,arg2,...) == expr` | `(expression-definition name args expr)`  | `{:tag :expression-definition :name name :args args :expr expr}` |             |
| `name == pred`                | `(predicate-definition name [] pred)`     | `{:tag :predicate-definition :name name :args [] :pred pred}`    |             |
| `name(arg1,arg2,...) == pred` | `(predicate-definition name args pred)`   | `{:tag :predicate-definition :name name :args args :pred pred}`  |             |
| `name == sub`                 | `(substitution-definition name [] sub)`   | `{:tag :substitution-definition :name name :args [] :sub sub}`   |             |
| `name(arg1,arg2,...) == sub`  | `(substitution-definition name args sub)` | `{:tag :substitution-definition :name name :args args :sub sub}` |             |
| `name == sub`                 | `(substitution-definition name [] sub)`   | `{:tag :substitution-definition :name name :args [] :sub sub}`   |             |
| `name(arg1,arg2,...) == sub`  | `(substitution-definition name args sub)` | `{:tag :substitution-definition :name name :args args :sub sub}` |             |
| `"FILE.def"`                  | `(file-definition "FILE.def")`            | `{:tag :file-definition :file "FILE.def"}`                       |             |


## Machine
| B                                                                       | Lisb                                                                    | IR                                                                                                                            | Description         |
|-------------------------------------------------------------------------|-------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|---------------------|
| `MACHINE machine-name clauses END`                                      | `(machine machine-name & machine-clauses)`                              | `(merge {:tag :machine, :machine-clauses machine-clauses} machine-name)`                                                      | machine             |
| `MODEL machine-name clauses END`                                        | `(model machine-name & machine-clauses)`                                | `(merge {:tag :model, :machine-clauses machine-clauses} machine-name)`                                                        | synonym for machine |
| `SYSTEM machine-name clauses END`                                       | `(system machine-name & machine-clauses)`                               | `(merge {:tag :system, :machine-clauses machine-clauses} machine-name)`                                                       | synonym for machine |
| `REFINEMENT machine-name REFINES abstract-machine-name clauses END`     | `(refinement machine-name abstract-machine-name & machine-clauses)`     | `(merge {:tag :refinement, :abstract-machine-name abstract-machine-name, :machine-clauses machine-clauses} machine-name)`     | refinement          |
| `IMPLEMENTATION machine-name REFINES abstract-machine-name clauses END` | `(implementation machine-name abstract-machine-name & machine-clauses)` | `(merge {:tag :implementation, :abstract-machine-name abstract-machine-name, :machine-clauses machine-clauses} machine-name)` | implementation      |
### Machine name
| B                         | Lisb              | IR                             | Description                  |
|---------------------------|-------------------|--------------------------------|------------------------------|
| `name`                    | `name`            | `{:name name}`                 | machine name                 |
| `name(arg,arg,...)`       | `[name & args]`   | `{:name name, :args args}`     | machine name with parameters |
