# Work in progress lisb api doc

##Logical predicates
| B                                                    | Lisb                             | IR                                                                 | Description                |
|------------------------------------------------------|----------------------------------|--------------------------------------------------------------------|----------------------------|
| `pred1 & pred2 & ...`                                | `(and & preds)`                  | `{:tag :and, :preds preds}`                                        | conjunction                |
| `pred1 or pred2 or ...`                              | `(or & preds)`                   | `{:tag :or, :preds preds}`                                         | disjunction                |
| `pred1 => pred2 => ...`                              | `(=> & preds)`                   | `{:tag :=>, :preds preds}`                                         | implication                |
| `pred1 <=> pred2 <=> ...`                            | `(<=> & preds)`                  | `{:tag :<=>, :preds preds}`                                        | equivalence                |
| `not(pred)`                                          | `(not pred)`                     | `{:tag :not, :pred pred}`                                          | not                        |
| `!(id1,id2,...).(=>-left => =>-right)`               | `(for-all ids =>-left =>-right)` | `{:tag :bfor-all, :ids ids, :=>-left =>-left, :=>-right =>-right}` | universal quantification   | <!-- improve naming -->
| `!(id1,id2,...).((id1:type1&id2:type2&...) => pred)` | `(for-all id-types pred)`        | `{:tag :for-all, :id-types id-types, :pred pred}`                  | sugar                      |
| `#(id1,id2,...).(pred)`                              | `(exists ids pred)`              | `{:tag :exists, :ids ids, :pred pred}`                             | existential quantification |

##Equality
| B                                                         | Lisb                  | IR                                       | Description |
|-----------------------------------------------------------|-----------------------|------------------------------------------|-------------|
| `left = right`                                            | `(= left right)`      | `{:tag :=, :left left, :right right}`    | equality    |
| `left /= right`                                           | `(not= left right)`   | `{:tag :not=, :left left, :right right}` | disequality |
| `elem1/=elem2 & elem1/=elem3 & ... & elem2/=elem3 & ... ` | `(distinct? & elems)` | `{:tag :distinct?, :elems elems}`        | distinct    |

##Booleans
| B            | Lisb                | IR                               | Description                          |
|--------------|---------------------|----------------------------------|--------------------------------------|
| `TRUE`       | `true`              | `true`                           |                                      |
| `FALSE`      | `false`             | `false`                          |                                      |
| `BOOL`       | `bool-set`          | `{:tag :bool-set}`               | set of boolean values ({TRUE,FALSE}) |
| `bool(pred)` | `(pred->bool pred)` | `{:tag :pred->bool, :pred pred}` | convert predicate into BOOL value    |

##Sets
| B                             | Lisb                                       | IR                                                                 | Description                                |
|-------------------------------|--------------------------------------------|--------------------------------------------------------------------|--------------------------------------------|
| `{elem1,elem2,...}`           | `#{elem1 elem2 ...}`                       | `#{elem1 elem2 ...}`                                               | set enumeration                            | <!-- merge set enumerations -->
| `{id1,id2,...&#124;pred}`     | `(comp-set ids pred)`                      | `{:tag :comp-set, :ids ids, :pred pred}`                           | comprehension set                          | <!-- test ids -->
| `{id1,id2,...&#124;pred}`     | `#{id1,id2,... &#124; pred}`               | `{:tag :comp-set, :ids ids, :pred pred}`                           | sugar                                      | <!-- implement -->
| `POW(set)`                    | `(pow set)`                                | `{:tag :power-set, :set set}`                                      | power set                                  | <!-- improve naming -->
| `POW1(set)`                   | `(pow1 set)`                               | `{:tag :power1-set, :set set}`                                     | set of non-empty subsets                   | <!-- improve naming -->
| `FIN(set)`                    | `(fin set)`                                | `{:tag :fin, :set set}`                                            | set of all finite subsets                  |
| `FIN1(set)`                   | `(fin1 set)`                               | `{:tag :fin1, :set set}`                                           | set of all non-empty finite subsets        |
| `card(set)`                   | `(card set)`                               | `{:tag :card, :set set}`                                           | cardinality                                |
| `set1*set2*...`               | `(* & elems)`                              | `{:tag :*, :elems elems}`                                          | cartesian product                          | <!-- distinct between cart and mul? -->
| `set1\/set2\/...`             | `(union & sets)`                           | `{:tag :union, :sets sets}`                                        | set union                                  |
| `set1/\set2/\...`             | `(intersection & sets)`                    | `{:tag :intersection, :sets sets}`                                 | set intersection                           |
| `set1-set2-...`               | `(set- & sets)`                            | `{:tag :difference, :sets sets}`                                   | set difference                             | <!-- improve naming -->
| `elem:set`                    | `(member? elem set)`                       | `{:tag :member?, :elem elem, :set set}`                            | element of                                 |
| `elem:set`                    | `(: elem set)`                             | `{:tag :member?, :elem elem, :set set}`                            | sugar                                      | <!-- implement -->
| `elem:set`                    | `(contains? set elem)`                     | `{:tag :member?, :elem elem, :set set}`                            | clojure                                    | <!-- contains-all? -->
| `elem/:set`                   | `(not (member? set elem))`                 | ``                                                                 | not element of                             | 
| `subset<:set`                 | `(subset? subset set)`                     | `{:tag :subset?, :subset subset, :set set}`                        | subset of                                  |
| `set<<:superset`              | `(superset? superset set)`                 | `{:tag :subset?, :subset set, :set superset}`                      | sugar                                      |
| `subset/<:set`                | `(not (subset? subset set))`               | ``                                                                 | not subset of                              |
| `strict-subset<<:set`         | `(strict-subset? strict-subset set)`       | `{:tag :strict-subset?, :strict-subset strict-subset, :set set}`   | strict subset of                           |
| `set<<:strict-superset`       | `(strict-superset? strict-superset set)`   | `{:tag :strict-subset?, :strict-subset set, :set strict-superset}` | sugar                                      |
| `strict-subset/<<:set`        | `(not (strict-subset? strict-subset set))` | ``                                                                 | not strict subset of                       |
| `union(set-of-sets)`          | `(unite-sets set-of-sets)`                 | `{:tag :general-union, :set-of-sets set-of-sets}`                  | generalised union over sets of sets        | <!-- improve naming -->
| `inter(set-of-sets)`          | `(intersect-sets set-of-sets)`             | `{:tag :general-intersection, :set-of-sets set-of-sets}`           | generalised intersection over sets of sets | <!-- improve naming -->
| `UNION(ids).(pred&#124;expr)` | `(union-pe ids pred expr)`                 | `{:tag :union-pe, :ids ids, :pred pred, :expr expr}`               | generalised union with predicate           | <!-- see examples -->
| `INTER(ids).(pred&#124;expr)` | `(intersection-pe ids pred expr)`          | `{:tag :intersection-pe, :ids ids, :pred pred, :expr expr}`        | generalised intersection with predicate    | <!-- see examples -->

##Numbers
| B                       | Lisb                 | IR                                                | Description                                                          |
|-------------------------|----------------------|---------------------------------------------------|----------------------------------------------------------------------|
| `INTEGER`               | `integer-set`        | `{:tag :integer-set}`                             | set of integers                                                      |
| `NATURAL`               | `natural-set`        | `{:tag :natural-set}`                             | set of natural numbers                                               |
| `NATURAL1`              | `natural1-set`       | `{:tag :natural1-set}`                            | set of non-zero natural numbers                                      |
| `INT`                   | `int-set`            | `{:tag :int-set}`                                 | set of implementable integers (MININT..MAXINT)                       |
| `NAT`                   | `nat-set`            | `{:tag :nat-set}`                                 | set of implementable natural numbers                                 |
| `NAT1`                  | `nat1-set`           | `{:tag :nat1-set}`                                | set of non-zero implementable natural numbers                        |
| `from..to`              | `(interval from to)` | `{:tag :interval, :from from, :to to}`            | set of numbers from n to m                                           |
|                         | `(range from to)`    | `{:tag :interval, :from from, :to (- to 1)}`      | clojure                                                              |
| `MININT`                | `min-int`            | `{:tag :min-int}`                                 | the minimum implementable integer                                    |
| `MAXINT`                | `max-int`            | `{:tag :max-int}`                                 | the maximum implementable integer                                    |
| `num1>num2>...`         | `(> & nums)`         | `{:tag :>, :nums nums}`                           | greater than                                                         |
| `num1<num2<...`         | `(< & nums)`         | `{:tag :<, :nums nums}`                           | less than                                                            |
| `num1=>num2=>...`       | `(>= & nums)`        | `{:tag :>=, :nums nums}`                          | greater than or equal                                                |
| `nums1<=num2<=...`      | `(<= & nums)`        | `{:tag :<=, :nums nums}`                          | less than or equal                                                   |
| `max(S)`                | `(max S)`            | `{:tag :max, :set set}`                           | maximum of a set of numbers                                          |
| `max({m,n,o})`          | `(max m n o)`        | `{:tag :max, :set set}`                           | sugar                                                                |
| `min(S)`                | `(min S)`            | `{:tag :min, :set set}`                           | minimum of a set of numbers                                          |
| `min({m,n,o})`          | `(min m n o)`        | `{:tag :min, :set set}`                           | sugar                                                                |
| `num1+num2+...`         | `(+ & nums)`         | `{:tag :+, :nums nums}`                           | addition                                                             |
| `num1-num2-...`         | `(- & nums)`         | `{:tag :-, :nums nums}`                           | difference                                                           |
| `num1*num2*...`         | `(* & nums)`         | `{:tag :*, :nums nums}`                           | multiplication                                                       |
| `num1/num2/...`         | `(div & nums)`       | `{:tag :div, :nums nums}`                         | division                                                             || `num1/num2/...`         | `(/ & nums)`         | `{:tag :div, :nums nums}                         | sugar                                                                |
| `num1**num2**...`       | `(** & nums)`        | `{:tag :**, :nums nums}`                          | power                                                                |
| `num1 mod num2 mod ...` | `(mod & nums)`       | `{:tag :mod, :nums nums}`                         | remainder of division                                                |
| `PI(z).(P&#124;E)`      | `(pi #{z} P E)`      | `{:tag :pi, :ids ids, :pred pred, :expr expr}`    | Set product                                                          | <!-- see examples -->
| `SIGMA(z).(P&#124;E)`   | `(sigma #{z} P E)`   | `{:tag :sigma, :ids ids, :pred pred, :expr expr}` | Set summation                                                        | <!-- see examples -->
| `succ(n)`               | `(succ n)`           | `{:tag :succ, :num num}`                          | successor (n+1)                                                      |
| `succ(n)`               | `(inc n)`            | `{:tag :succ, :num num}`                          | clojure                                                              |
| `pred(n)`               | `(pred n)`           | `{:tag :pred, :num num}`                          | predecessor (n-1)                                                    |
| `pred(n)`               | `(dec n)`            | `{:tag :pred, :num num}`                          | clojure                                                              |
| `0xH`                   | ``                   | ``                                                | hexadecimal literal, where H is a sequence of letters in [0-9A-Fa-f] | <!-- see examples -->

##Relations
| B                                         | Lisb                    | IR                                             | Description                                                      |
|-------------------------------------------|-------------------------|------------------------------------------------|------------------------------------------------------------------|
| `set1<->set2<->...`                       | `(<-> & sets)`          | `{:tag :<->, :sets sets}`                      | relation                                                         |
| `set1<<->set2<<->...`                     | `(<<-> & sets)`         | `{:tag :<<->, :sets sets}`                     | total relation                                                   |
| `set1<->>set2<<->...`                     | `(<->> & sets)`         | `{:tag :<->> , :sets sets}`                    | surjective relation                                              |
| `set1<<->>set2<<->...`                    | `(<<->> & sets)`        | `{:tag :<<->>, :sets sets}`                    | total surjective relation                                        |
| `leftP&#124;->right`                      | `(couple left right)`   | `{:tag couple, :left left, :right right}`      | maplet                                                           | <!-- improve naming? -->
| `dom(rel)`                                | `(dom rel)`             | `{:tag :dom, :rel rel}`                        | domain of relation                                               |
| `ran(rel)`                                | `(ran rel)`             | `{:tag :ran, :rel rel}`                        | range of relation                                                |
| `id(set)`                                 | `(id set)`              | `{:tag :id, :set set}`                         | identity relation                                                |
| `set<&#124;rel`                           | `(<&#124; set rel)`     | `{:tag :dom-restriction, :rel rel, :set set}`  | domain restriction                                               | <!-- improve clearance -->
| `set<<&#124;rel`                          | `(<<&#124; set rel)`    | `{:tag :dom-substraction, :rel rel, :set set}` | domain subtraction                                               | <!-- improve clearance -->
| `rel&#124;>set`                           | `(&#124;> rel set)`     | `{:tag :ran-restriction, :rel rel, :set set}`  | range restriction                                                | <!-- improve clearance -->
| `rel&#124;>>set`                          | `(&#124;>> rel set)`    | `{:tag :ran-substraction, :rel rel, :set set}` | range subtraction                                                | <!-- improve clearance -->
| `rel~`                                    | `(inverse rel)`         | `{:tag :inverse, :rel rel}`                    | inverse of relation                                              |
| `rel[set]`                                | `(image rel set)`       | `{:tag :image, :rel rel, :set set}`            | relational image                                                 |
| `rel1<+rel2<+...`                         | `(<+ & rels)`           | `{:tag :<+, :rels rels}`                       | relational overriding (r2 overrides r1)                          |
| `rel1><rel2><...`                         | `(>< & rels)`           | `{:tag :><, :rels rels}`                       | direct product {x,(y,z) &#124; x,y:r1 & x,z:r2}                  |
| `((rel1;rel2);...)`                       | `(comp & rels)`         | `{:tag :comp, :rels rels}`                     | relational composition {x,y&#124; x&#124;->z:r1 & z&#124;->y:r2} |
| `((rel1&#124;&#124;rel2)&#124;&#124;...)` | `(&#124;&#124; & rels)` | `{:tag :parallel-product, :rels rels}`         | parallel product {((x,v),(y,w)) &#124; x,y:r1 & v,w:r2}          | <!-- collision with parallel-sub -->
| `prj1(set1, set2)`                        | `(prj1 set1 set2)`      | `{:tag :prj1, :set1 set1, :set2 set2}`         | projection function (usage prj1(Dom,Ran)(Pair))                  | <!-- see examples -->
| `prj2(set1, set2)`                        | `(prj2 set1 set2)`      | `{:tag :prj2, :set1 set1, :set2 set2}`         | projection function (usage prj2(Dom,Ran)(Pair))                  | <!-- see examples -->
| `clojure1(rel)`                           | `(clojure1 rel)`        | `{:tag :clojure1, :rel rel}`                   | transitive closure                                               |
| `clojure(rel)`                            | `(clojure rel)`         | `{:tag :clojure, :rel rel}`                    | reflexive & transitive closure                                   |
| `iterate(rel,num)`                        | `(iterate rel num)`     | `{:tag :iterate, :rel rel, :num num}           | iteration of r with n>=0                                         |
| `fnc(rel)`                                | `(fnc rel)`             | `{:tag :fnc, :rel rel}`                        | translate relation A<->B into function A+->POW(B)                |
| `rel(rel)`                                | `(rel rel)`             | `{:tag :rel, :rel rel}`                        | translate relation A<->POW(B) into relation A<->B                |

##Functions
| B                                                    | Lisb                     | IR                                                 | Description          |
|------------------------------------------------------|--------------------------|----------------------------------------------------|----------------------|
| `set1+->set2+->...`                                  | `(+-> & sets)`           | `{:tag :+->, :sets sets}`                          | partial function     |
| `set1-->set2-->...`                                  | `(--> & sets)`           | `{:tag :-->, :sets sets}`                          | total function       |
| `set1+->>set2+->>...`                                | `(+->> & sets)`          | `{:tag :+->>, :sets sets}`                         | partial surjection   |
| `set1-->>set2-->>...`                                | `(-->> & sets)`          | `{:tag :-->>, :sets sets}`                         | total surjection     |
| `set1>+>set2>+>...`                                  | `(>+> & sets)`           | `{:tag :>+>, :sets sets}`                          | partial injection    |
| `set1>->set2>->...`                                  | `(>-> & sets)`           | `{:tag :>->, :sets sets}`                          | total injection      |
| `set1>+>>set2>+>>...`                                | `(>+>> & sets)`          | `{:tag :>+>>, :sets sets}`                         | partial bijection    |
| `set1>->>set2>->>...`                                | `(>->> & sets)`          | `{:tag :>->>, :sets sets}`                         | total bijection      |
| `%id1,id2,... .(pred&#124;expr)`                     | `(lambda ids pred expr)` | `{:tag :lambda, :ids ids, :pred pred, :expr expr}` | lambda abstraction   |
| `%id1,id2,... .(id1:type1&id2:type2&... &#124;expr)` | `(fn [id-types] expr)`   | `{:tag :fn, :id-types id-types, :expr expr}`       | sugar                |
| `f(arg1,arg2,...)`                                   | `(apply f & args)`       | `{:tag :apply, :f f, :args args}`                  | function application |

##Sequences
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
| `elem->seq`              | `(-> elem seq)`       | `{:tag :->, :elem elem, :set seq}`       | prepend element                           |
| `(seq<-elem1)<-elem2...` | `(<- seq elems)`      | `{:tag :<-, :set seq, :elem elems}`      | append element                            |
| `rev(seq)`               | `(rev seq)`           | `{:tag :rev, :seq seq}`                  | reverse of sequence                       |
| `first(seq)`             | `(first seq)`         | `{:tag :first, :seq seq}`                | first element                             |
| `last(seq)`              | `(last seq)`          | `{:tag :last, :seq seq}`                 | last element                              |
| `front(seq)`             | `(front seq)`         | `{:tag :front, :seq seq}`                | front of sequence (all but last element)  |
| `front(seq)`             | `(drop-last seq)`     | `{:tag :front, :seq seq}`                | clojure                                   |
| `tail(seq)`              | `(tail seq)`          | `{:tag :tail, :seq seq}`                 | tail of sequence (all but first element)  |
| `tail(seq)`              | `(rest seq)`          | `{:tag :tail, :seq seq}`                 | clojure                                   |
| `conc(seq-of-seqs)`      | `(conc seq-of-seqs)`  | `{:tag :conc, :seq-of-seqs seq-of-seqs}` | concatenation of sequence of sequences    |
| `seq/&#124;\num`         | `(take num seq)`      | `{:tag :take, :num num, :seq seq}`       | take first n elements of sequence         |
| `seq\&#124;/num`         | `(drop num seq)`      | `{:tag :drop, :num num, :seq seq}`       | drop first n elements from sequence       |

##Records
| B                                 | Lisb                  | IR                                   | Description                                          |
|-----------------------------------|-----------------------|--------------------------------------|------------------------------------------------------|
| `struct(id1:type1,id2:type2,...)` | `(struct & id-types)` | `{:tag :struct, :id-types id-types}` | set of records with given fields and field types     |
| `rec(id1:val1,id2:val2,...)`      | `(rec & id-vals)`     | `{:tag :rec, :id-vals id-vals}`      | construct a record with given field names and values |
| `rec'id`                          | `(get rec id)`        | `{:tag :get, :rec rec, :id id}`      | get value of field with name ID                      |

##Strings
| B               | Lisb           | IR                   | Description                                                           |
|-----------------|----------------|----------------------|-----------------------------------------------------------------------|
| `"astring"`     | `"astring"`    | `"astring"`          | a specific (single-line) string value                                 |
| `'''astring'''` | ``             | ``                   | an alternate way of writing (multi-line) strings, no need to escape " |
| `STRING`        | `string-set`   | `{:tag :string-set}` | the set of all strings                                                |

##Reals

##Trees

##Let and If-Then-Else
| B                                                                | Lisb                                        | IR                                                           | Description                 |
|------------------------------------------------------------------|---------------------------------------------|--------------------------------------------------------------|-----------------------------|
| `IF cond THEN then ELSE else END`                                | `(if-expr cond then else)`                  | `{:tag :if-expr, :cond cond, :then then, :else else}`        | conditional for expressions |
| `IF cond THEN then ELSE else END`                                | `(and (=> cond then) (=> (not cond) else))` | ``                                                           | conditional for predicates  | <!-- detect and rewrite to if -->
| `LET id1,id2,... BE id1=val1 & id2=val2 ... IN expr-or-pred END` | `(let id-vals expr-or-pred)`                | `{:tag :let, :id-vals id-vals, :expr-or-pred :expr-or-pred}` |                             |

##Substitutions
| B                                                          | Lisb                                         | IR                                                   | Description                            |
|------------------------------------------------------------|----------------------------------------------|------------------------------------------------------|----------------------------------------|
| `skip`                                                     | `skip`                                       | `{:tag :skip}`                                       | no operation                           |
| `id1,id2,... := val1,val2,...`                             | `(assign & id-vals)`                         | `{:tag :assign, :id-vals id-vals}`                   | assignment                             |
| `f(x) := E`                                                | ``                                           | ``                                                   | functional override                    | <!-- TODO -->
| `ids :: set`                                               | `(becomes-element-of ids set)`               | `{:tag :becomes-element-of, :ids ids, :set set}`     | choice from set                        | <!-- improve naming -->
| `ids : (pred)`                                             | `(becomes-such ids set)`                     | `{:tag :becomes-such, :ids ids, :pred pred}`         | choice by pred (constraining ids)      | <!-- improve naming -->
| `id1,id2,... <-- op(arg1,arg2,...)`                        | `(op-call ids op args)`                      | `{:tag :op-call, :ids ids, :op op, :args args}`      | call operation and assign return value | <!-- rename ids to returns? -->
| `sub1&#124;&#124;sub2&#124;...`                            | `(parallel-sub & subs)`                      | `{:tag :parallel-sub, :subs subs}`                   | parallel substitution                  |
| `sub1&#124;&#124;sub2&#124;...`                            | `(&#124;&#124; & subs)`                      | `{:tag :parallel-sub, :subs subs}`                   | sugar                                  | <!-- collision with parallel product -->
| `sub1;sub2;...`                                            | `(sequential-sub & subs)`                    | `{:tag :sequential-sub, :subs subs}`                 | sequential composition                 |
| `ANY id1,id2,... WHERE where THEN then END`                | `(any ids where then)`                       | `{:tag :any, :ids ids, :where where, :then then}`    | non deterministic choice               |
| `LET id1,id2,... BE id1=val1&id2=val2... IN sub END`       | `(let-sub [id1 val1 id2 val2 ...] sub)`      | `{:tag :let-sub, :id-vals id-vals, :sub sub}`        |                                        |
| `VAR x,... IN G END`                                       | `(var ids sub`                               | `{:tag :var, :ids ids, :sub sub}`                    | generate local variables               |
| `PRE pred THEN sub END`                                    | `(pre pred sub)`                             | `{:tag :pre, :pred pred, :sub sub}`                  |                                        |
| `CHOICE G OR H END`                                        | `(choice & subs)`                            | `{:tag :choice, :subs subs}`                         |                                        |
| `IF cond THEN then END`                                    | `(if-sub cond then)`                         | `{:tag :if-sub, :cond cond, :then then}`             |                                        |
| `IF cond THEN then ELSE else END`                          | `(if-sub cond then else)`                    | `{:tag :if-sub, :cond cond, :then then, :else else}` |                                        |
| `IF P1 THEN G1 ELSIF P2 THEN G2 ... END`                   | `(cond cond1 then1 cond2 then2 ...)`         | `{:tag :cond, :clauses clauses}`                     |                                        |
| `IF P1 THEN G1 ESLIF P2 THEN G2 ... ELSE Gn END`           | `(cond cond1 then1 cond2 then2 ... else)`    | `{:tag :cond, :clauses clauses}`                     |                                        |
| `SELECT P THEN G WHEN ... WHEN Q THEN H END`               | `(select cond1 then1 cond2 then2 ...)`       | `{:tag :select, :clauses clauses}`                   |                                        |
| `SELECT P THEN G WHEN ... WHEN Q THEN H ELSE I END`        | `(select cond1 then1 cond 2 then2 ... else)` | `{:tag :select, :clauses clauses}`                   |                                        |
| `CASE E OF EITHER m THEN G OR n THEN H ... END END`        | ``                                           | ``                                                   |                                        | <!-- TODO -->
| `CASE E OF EITHER m THEN G OR n THEN H ... ELSE I END END` | ``                                           | ``                                                   |                                        | <!-- TODO -->
| ``                                                         | ``                                           | ``                                                   |                                        | 
| `WHEN P THEN G END  is a synonym for SELECT P THEN G END`  | ``                                           | ``                                                   |                                        | <!-- TODO -->

##Machine clauses
###Machine inclusion
| B                                | Lisb                    | IR                                  | Description                          |
|----------------------------------|-------------------------|-------------------------------------|--------------------------------------|
| `USES mch-name1,mch-name2,...`   | `(uses & mch-names)`    | `{:tag :uses :values mch-names}`    |                                      |
| `INDLUDES mch-ref1,mch-ref2,...` | `(includes & mch-refs)` | `{:tag :includes :values mch-refs}` |                                      |
| `SEES mch-name1,mch-name2,...`   | `(sees & mch-names)`    | `{:tag :sees :values mch-names}`    |                                      |
| `EXTENDS mch-ref1,mch-ref2,...`  | `(extends & mch-refs)`  | `{:tag :extends :values mch-refs}`  |                                      |
| `PROMOTES op1,op2,...`           | `(promotes & ops)`      | `{:tag :promotes :values ops}`      |                                      |
####Machine reference
| B                         | Lisb              | IR                                             | Description                    |
|---------------------------|-------------------|------------------------------------------------|--------------------------------|
| `name`                    | `name`            | `{:tag :mch-ref, :name name}`                  | `machine name`                 |
| `name(param1,param2,...)` | `[name & params]` | `{:tag :mch-ref, :name name, :params :params}` | `machine name with parameters` |
###Machine section
| B                                  | Lisb                     | IR                                    | Description                          |
|------------------------------------|--------------------------|---------------------------------------|--------------------------------------|
| `CONSTRAINTS pred1 & pred2 & ...`  | `(constraints & preds)`  | `{:tag :constraints, :values preds}`  |                                      |
| `SETS set-def1;set-def2;...`       | `(sets & set-defs)`      | `{:tag :sets, :values set-defs}`      |                                      |
| `CONSTANTS id1,id2,...`            | `(constants & ids)`      | `{:tag :constants, :values ids}`      |                                      |
| `CONCRETE_CONSTANTS cx,cy,..`      | ``                       | ``                                    |                                      | <!-- TODO -->
| `PROPERTIES pred1 & pred2 & ...`   | `(properties preds)`     | `{:tag :properties, :values preds}`   |                                      |
| `DEFINITIONS m(x,...) == BODY;...` | ``                       | ``                                    |                                      | <!-- TODO -->
| `VARIABLES id1,id2,...`            | `(variables & ids)`      | `{:tag :variables, :values ids}`      |                                      |
| `CONCRETE_VARIABLES cv,cw,...`     | ``                       | ``                                    |                                      | <!-- TODO -->
| `INVARIANT pred1 & pred2 & ...`    | `(invariants & preds)`   | `{:tag :invariants, :values preds}`   |                                      |
| `ASSERTIONS pred1;pred2;...`       | `(assertions & preds)`   | `{:tag :assertions, :values preds}`   |                                      |
| `INITIALISATION subs1;subs2;...`   | `(init & subs)`          | `{:tag :init, :values subs}`          |                                      |
| `OPERATIONS op-def1;op-def2;...`   | `(operations & op-defs)` | `{:tag :operations, :values op-defs}` |                                      |
####Set definitions
| B                      | Lisb                          | IR                                             | Description    |
|------------------------|-------------------------------|------------------------------------------------|----------------|
| `id`                   | `(deferred-set id)`           | `{:tag :deferred-set, :id id}`                 | deferred set   | <!-- improve lisb -->
| `id={elem1,elem2,...}` | `(enumerated-set id & elems)` | `{:tag :enumerated-set, :id id, :elems elems}` | enumerated set | <!-- improve lisb -->
####Operation definitions
| B                                                    | Lisb                          | IR                                                                 | Description    |
|------------------------------------------------------|-------------------------------|--------------------------------------------------------------------|----------------|
| `return1,return2,... <-- name(arg1,arg2,...) = body` | `(name [arg1,arg2,...] body)` | `{:tag :op, :return-vals return-vals, :name name, :args args, :body body}` |                |  <!-- TODO -->


##Machine
| B                                                   | Lisb                                        | IR                                                                          | Description         |
|-----------------------------------------------------|---------------------------------------------|-----------------------------------------------------------------------------|---------------------|
| `MACHINE header clauses END`                        | `(machine header & clauses)`                | `(merge {:tag :machine, :clauses clauses} header)`                          |                     |
| `MODEL header clauses END`                          | `(model header & clauses)`                  | `(merge {:tag :model, :clauses clauses} header)`                            | synonym for MACHINE |
| `SYSTEM header clauses END`                         | `(system header & clauses)`                 | `(merge {:tag :system, :clauses clauses} header)`                           | synonym for MACHINE |
| `REFINEMENT header REFINES ref-mch clauses END`     | `(refinement header ref-mch & clauses)`     | `(merge {:tag :refinement, :ref-mch ref-mch, :clauses clauses} header)`     |                     |
| `IMPLEMENTATION header REFINES ref-mch clauses END` | `(implementation header ref-mch & clauses)` | `(merge {:tag :implementation, :ref-mch ref-mch, :clauses clauses} header)` |                     |
###Machine header
| B                         | Lisb              | IR                             | Description                  |
|---------------------------|-------------------|--------------------------------|------------------------------|
| `name`                    | `name`            | `{:name name}`                 | machine name                 |
| `name(arg,arg,...)`       | `[name & args]`   | `{:name name, :args args}`     | machine name with parameters |
