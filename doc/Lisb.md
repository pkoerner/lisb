# Work in progress lisb api doc
##Logical predicates
| B                         | Lisb                     | IR                                                  | Description                |
|---------------------------|--------------------------|-----------------------------------------------------|----------------------------|
| `pred1 & pred2 & ...`     | `(and & preds)`          | {:tag :and, :preds preds}                           | conjunction                |
| `pred1 or pred2 or ...`   | `(or & preds)`           | {:tag :or, :preds preds}                            | disjunction                |
| `pred1 => pred2 => ...`   | `(=> & preds)`           | {:tag :implication, :preds preds}                   | implication                |
| `pred1 <=> pred2 <=> ...` | `(<=> & preds)`          | {:tag :equivalence, :preds preds}                   | equivalence                |
| `not(pred)`               | `(not pred)`             | {:tag :not, :pred pred}                             | not                        |
| `!(x).(P=>Q)`             | `(for-all [:x] (=> P Q)` | {:tag :for-all, :ids ids, :implication implication} | universal quantification   |
| `#(x).(P&Q)`              | `(exists [:x] (and P Q)` | {:tag :exists, :ids ids, :pred pred}                | existential quantification |
##Equality
| B                                                         | Lisb                     | IR                                          | Description |
|-----------------------------------------------------------|--------------------------|---------------------------------------------|-------------|
| `left = right`                                            | `(= left right)`         | {:tag :equal, :left left, :right right}     | equality    |
| `left /= right`                                           | `(not= left right)`      | {:tag :not-equal, :left left, :right right} | disequality |
| `elem1/=elem2 & elem1/=elem3 & ... & elem2/=elem3 & ... ` | `(distinct? & elements)` | {:tag :distinct, :elements elements}        | distinct    |
##Booleans
| B            | Lisb                | IR                             | Description                          |
|--------------|---------------------|--------------------------------|--------------------------------------|
| `TRUE`       | `true`              | true                           |                                      |
| `FALSE`      | `false`             | false                          |                                      |
| `BOOL`       | `bool-set`          | {:tag :bool-set}               | set of boolean values ({TRUE,FALSE}) |
| `bool(pred)` | `(pred->bool pred)` | {:tag :pred->bool, :pred pred} | convert predicate into BOOL value    |
##Sets
| B                     | Lisb                         | IR                                                     | Description                                |
|-----------------------|------------------------------|--------------------------------------------------------|--------------------------------------------|
| `{}`                  | `#{}`                        | #{}                                                    | empty set                                  |
| `{E}`                 | `#{E}`                       | #{E}                                                   | singleton set                              |
| `{E,F}`               | `#{E F}`                     | #{E,F}                                                 | set enumeration                            |
| `{x&#124;P}`          | `(comp-set #{:x} P)`         | {:tag :comp-set, :ids ids, :pred pred                  | comprehension set                          |
| `POW(S)`              | `(pow S)`                    | {:tag :power-set, :set set}                            | power set                                  |
| `POW1(S)`             | `(pow1 S)`                   | {:tag :power1-set, :set set}                           | set of non-empty subsets                   |
| `FIN(S)`              | `(fin S)`                    | {:tag :fin, :set set}                                  | set of all finite subsets                  |
| `FIN1(S)`             | `(fin1 S)`                   | {:tag :fin1, :set set}                                 | set of all non-empty finite subsets        |
| `card(S)`             | `(count s)`                  | {:tag :card, :set set}                                 | cardinality                                |
| `S*T`                 | `(* S T)`                    | {:tag :mult-or-cart, :nums nums}                       | cartesian product                          |
| `S\/T`                | `(union S T)`                | {:tag :union, :sets sets}                              | set union                                  |
| `S/\T`                | `(intersection S T)`         | {:tag :intersection, :sets sets}                       | set intersection                           |
| `S-T`                 | `(set- S T)`                 | {:tag :difference, :sets sets}                         | set difference                             |
| `E:S`                 | `(member? S E)`              | {:tag :member, :element element, :set set}             | element of                                 |
| `E:S`                 | `(contains? E S)`            | {:tag :member, :element element, :set set}             | element of                                 |
| `E/:S`                | `(not (contains? E S))`      |                                                        | not element of                             |
| `S<:T`                | `(subset? S T)`              | {:tag :subset, :subset subset, :set set}               | subset of                                  |
| `S/<:T`               | `(not (subset? S T))`        |                                                        | not subset of                              |
| `S<<:T`               | `(subset-strict? S T)`       | {:tag :subset-strict, :subset subset, :set set}        | strict subset of                           |
| `S/<<:T `             | `(not (subset-strict? S T))` |                                                        | not strict subset of                       |
| `union(S)`            | `(unite-sets S)`             | {:tag :general-union, :set-of-sets set-of-sets}        | generalised union over sets of sets        |
| `inter(S)`            | `(intersect-sets S)`         | {:tag :general-intersection, :set-of-sets set-of-sets} | generalised intersection over sets of sets |
| `UNION(z).(P&#124;E)` | `(union-pe #{z} P E)`        | {:tag :union-pe, :ids ids, :pred pred, :expr expr      | generalised union with predicate           |
| `INTER(z).(P&#124;E)` | `(inter-pe #{z} P E)`        | {:tag :inter-pe, :ids ids, :pred pred, :expr expr      | generalised intersection with predicate    |
##Numbers
| B                       | Lisb                 | IR                                              | Description                                                          |
|-------------------------|----------------------|-------------------------------------------------|----------------------------------------------------------------------|
| `INTEGER`               | `integer-set`        | {:tag :integer-set}                             | set of integers                                                      |
| `NATURAL`               | `natural-set`        | {:tag :natural-set}                             | set of natural numbers                                               |
| `NATURAL1`              | `natural1-set`       | {:tag :natural1-set}                            | set of non-zero natural numbers                                      |
| `INT`                   | `int-set`            | {:tag :int-set}                                 | set of implementable integers (MININT..MAXINT)                       |
| `NAT`                   | `nat-set`            | {:tag :nat-set}                                 | set of implementable natural numbers                                 |
| `NAT1`                  | `nat1-set`           | {:tag :nat1-set}                                | set of non-zero implementable natural numbers                        |
| `from..to`              | `(interval from to)` | {:tag :interval, :from from, :to to}            | set of numbers from n to m                                           |
| `from..(to+1)`          | `(range from to)`    |                                                 |                                                                      |
| `MININT`                | `min-int`            | {:tag :min-int}                                 | the minimum implementable integer                                    |
| `MAXINT`                | `max-int`            | {:tag :max-int}                                 | the maximum implementable integer                                    |
| `num1>num2>...`         | `(> & nums)`         | {:tag :greater, :nums nums}                     | greater than                                                         |
| `num1<num2<...`         | `(< & nums)`         | {:tag :less, :nums nums}                        | less than                                                            |
| `num1=>num2=>...`       | `(>= & nums)`        | {:tag :greater-eq, :nums nums}                  | greater than or equal                                                |
| `nums1<=num2<=...`      | `(<= & nums)`        | {:tag :less-eq, :nums nums}                     | less than or equal                                                   |
| `max(S)`                | `(max S)`            | {:tag :max, :set set}                           | maximum of a set of numbers                                          |
| `max({m,n,o})`          | `(max m n o)`        | {:tag :max, :set set}                           |                                                                      |
| `min(S)`                | `(min S)`            | {:tag :min, :set set}                           | minimum of a set of numbers                                          |
| `min({m,n,o})`          | `(min m n o)`        | {:tag :min, :set set}                           |                                                                      |
| `num1+num2+...`         | `(+ & nums)`         | {:tag :plus, :nums nums}                        | addition                                                             |
| `-num`                  | `(- num)`            | {:tag :unary-minus, :num num}                   | unary-minus                                                          |
| `num1-num2-...`         | `(- & nums)`         | {:tag :minus, :nums nums}                       | difference                                                           |
| `num1*num2*...`         | `(* & nums)`         | {:tag :mult-or-cart, :nums nums}                | multiplication                                                       |
| `num1/num2/...`         | `(/ & nums)`         | {:tag :div, :nums nums}                         | division                                                             |
| `num1**num2**...`       | `(** & nums)`        | {:tag :pow, :nums nums}                         | power                                                                |
| `num1 mod num2 mod ...` | `(mod & nums)`       | {:tag :mod, :nums nums}                         | remainder of division                                                |
| `PI(z).(P&#124;E)`      | `(pi #{z} P E)`      | {:tag :pi, :ids ids, :pred pred, :expr expr}    | Set product                                                          |
| `SIGMA(z).(P&#124;E)`   | `(sigma #{z} P E)`   | {:tag :sigma, :ids ids, :pred pred, :expr expr} | Set summation                                                        |
| `succ(n)`               | `(inc n)`            | {:tag :inc, :num num}                           | successor (n+1)                                                      |
| `pred(n)`               | `(dec n)`            | {:tag :dec, :num num}                           | predecessor (n-1)                                                    |
| `0xH`                   | ``                   |                                                 | hexadecimal literal, where H is a sequence of letters in [0-9A-Fa-f] |
##Relations
| B                                         | Lisb                                 | IR                                             | Description                                                      |
|-------------------------------------------|--------------------------------------|------------------------------------------------|------------------------------------------------------------------|
| `set1<->set2<->...`                       | `(<-> & sets)`                       | {:tag :rel, :sets sets}                        | relation                                                         |
| `set1<<->set2<<->...`                     | `(total-relation & sets)`            | {:tag :total-rel, :sets sets}                  | total relation                                                   |
| `set1<->>set2<<->...`                     | `(surjective-relation & sets)`       | {:tag :surj-rel, :sets sets}                   | surjective relation                                              |
| `set1<<->>set2<<->...`                    | `(total-surjective-relation & sets)` | {:tag :total-surj-rel, :sets sets}             | total surjective relation                                        |
| `leftP&#124;->right`                      | `(couple left right)`                | {:tag couple, :left left, :right right}        | maplet                                                           |
| `dom(rel)`                                | `(dom rel)`                          | {:tag :dom, :rel rel}                          | domain of relation                                               |
| `ran(rel)`                                | `(ran rel)`                          | {:tag :ran, :rel rel}                          | range of relation                                                |
| `id(set)`                                 | `(identity set)`                     | {:tag :id, :set set}                           | identity relation                                                |
| `set<&#124;rel`                           | `(<&#124; set rel)`                  | {:tag :dom-restriction, :rel rel, :set set}    | domain restriction                                               |
| `set<<&#124;rel`                          | `(<<&#124; set rel)`                 | {:tag :dom-substraction, :rel rel, :set set}   | domain subtraction                                               |
| `rel&#124;>set`                           | `(&#124;> rel set)`                  | {:tag :range-restriction, :rel rel, :set set}  | range restriction                                                |
| `rel&#124;>>set`                          | `(&#124;>> rel set)`                 | {:tag :range-substraction, :rel rel, :set set} | range subtraction                                                |
| `rel~`                                    | `(inverse rel)`                      | {:tag :inverse, :rel rel}                      | inverse of relation                                              |
| `rel[set]`                                | `(image rel set)`                    | {:tag :image, :rel rel, :set set}              | relational image                                                 |
| `rel1<+rel2<+...`                         | `(<+ & rels)`                        | {:tag :override, :rels rels}                   | relational overriding (r2 overrides r1)                          |
| `rel1><rel2><...`                         | `(>< & rels)`                        | {:tag direct-product, :rels rels}              | direct product {x,(y,z) &#124; x,y:r1 & x,z:r2}                  |
| `((rel1;rel2);...)`                       | `(comp & rels`                       | {:tag comp, :rels rels}                        | relational composition {x,y&#124; x&#124;->z:r1 & z&#124;->y:r2} |
| `((rel1&#124;&#124;rel2)&#124;&#124;...)` | `(&#124;&#124; & rels)`              | {:tag parallel-product, :rels rels}            | parallel product {((x,v),(y,w)) &#124; x,y:r1 & v,w:r2}          |
| `prj1(set1, set2)`                        | `(prj1 set1 set2)`                   | {:tag :prj1, :set1 set1, :set2 set2}           | projection function (usage prj1(Dom,Ran)(Pair))                  |
| `prj2(set1, set2)`                        | `(prj2 set1 set2)`                   | {:tag :prj2, :set1 set1, :set2 set2}           | projection function (usage prj2(Dom,Ran)(Pair))                  |
| `clojure1(rel)`                           | `(clojure1 rel)`                     | {:tag :clojure1, :rel rel}                     | transitive closure                                               |
| `clojure(rel)`                            | `(clojure rel)`                      | {:tag :clojure, :rel rel}                      | reflexive & transitive closure                                   |
| `iterate(rel,num)`                        | `(iterate rel num)`                  | {:tag :iterate, :rel rel, :num num}            | iteration of r with n>=0                                         |
| `fnc(rel)`                                | `(fnc rel)`                          | {:tag :functionise, :rel rel}                  | translate relation A<->B into function A+->POW(B)                |
| `rel(rel)`                                | `(rel rel)`                          | {:tag :relationise, :rel rel}                  | translate relation A<->POW(B) into relation A<->B                |
##Functions
| B                       | Lisb                     | IR                                               | Description          |
|-------------------------|--------------------------|--------------------------------------------------|----------------------|
| `set1+->set2+->...`     | `(+-> & sets)`           | {:tag :partial-fn, :sets sets}                   | partial function     |
| `set1-->set2-->...`     | `(--> & sets)`           | {:tag :total-fn, :sets sets}                     | total function       |
| `set1+->>set2+->>...`   | `(+->> & sets)`          | {:tag :partial-surjection, :sets sets}           | partial surjection   |
| `set1-->>set2-->>...`   | `(-->> & sets)`          | {:tag :total-surjection, :sets sets}             | total surjection     |
| `set1>+>set2>+>...`     | `(>+> & sets)`           | {:tag :partial-injection, :sets sets}            | partial injection    |
| `set1>->set2>->...`     | `(>-> & sets)`           | {:tag :total-injection, :sets sets}              | total injection      |
| `set1>+>>set2>+>>...`   | `(>+>> & sets)`          | {:tag :partial-bijection, :sets sets}            | partial bijection    |
| `set1>->>set2>->>...`   | `(>->> & sets)`          | {:tag :total-bijection, :sets sets}              | total bijection      |
| `%ids.(pred&#124;expr)` | `(lambda ids pred expr)` | {:tag :lambda, :ids ids, :pred pred, :expr expr} | lambda abstraction   |
|                         | `(fn [id-types] expr)`   | {:tag :fn, :id-types id-types, :expr expr}       | lambda abstraction   |
| `f(arg1,arg2,...)`      | `(apply f & args)`       | {:tag :apply, :f f, :args args}                  | function application |
##Sequences
| B            | Lisb             | IR | Description                          |
|--------------|------------------|----|--------------------------------------|
| `<> or []`   | `(sequence)`     | {:tag :empty-sequence} |
| `[E]`        | `(sequence E)`   | {:tag :sequence, :elements elements} |
| `[E,F]`      | `(sequence E F)` |  |
| `seq(S)`     | `(seq S)`        | {:tag :seq, :set set} |
| `seq1(S)`    | `(seq1 S)`       | {:tag :seq1, :set set} |
| `iseq(S)`    | `(iseq S)`       | {:tag :iseq, :set set} |
| `iseq1(S)`   | `(iseq1 S)`      | {:tag :iseq1, :set set} |
| `perm(S)`    | `(perm S)`       | {:tag :perm, :set set} |
| `size(S)`    | `(size S)`       |  |
| `s^t`        | `(concat s t)`   | {:tag :concat, :seqs seqs} |
| `E->s`       | `(cons s E)`     | {:tag :insert-front |
| `s<-E`       | `(append s E)`   | {:tag |
| `rev(S)`     | `(reverse S)`    | {:tag :rev, :seq seq} |
| `first(S)`   | `(first S)`      | {:tag :first, :seq seq} |
| `last(S)`    | `(last S)`       | {:tag :last, :seq seq} |
| `front(S)`   | `(drop-last S)`  | {:tag :drop-last, :seq seq} |
| `tail(S)`    | `(rest S)`       | {:tag :rest, :seq seq} |
| `conc(S)`    | `(conc S)`       | {:tag :conc, :seq-of-seqs seq-of-seqs} |
| `s/&#124;\n` | `(take n S)`     | {:tag :take, :seq seq, :n n} |
| `s\&#124;/n` | `(drop n S)`     | {:tag :drop, :seq seq, :n n} |
##Records
| B                                 | Lisb                  | IR                                 | Description                                          |
|-----------------------------------|-----------------------|------------------------------------|------------------------------------------------------|
| `struct(id1:type1,id2:type2,...)` | `(struct & id-types)` | {:tag :struct, :id-types id-types} | set of records with given fields and field types     |
| `rec(id1:value1,id2:value2,...)`  | `(rec & id-vals)`     | {:tag :rec, :id-vals id-vals}      | construct a record with given field names and values |
| `rec'id`                          | `(get rec id)`        | {:tag :get, :rec record, :id id}   | get value of field with name ID                      |
##Strings
| B               | Lisb                       | IR | Description                          |
|-----------------|----------------------------|----|--------------------------------------|
| `"astring"`     | `"astring"`     | "astring" | a specific (single-line) string value |
| `'''astring'''` | ``              |  | an alternate way of writing (multi-line) strings, no need to escape " |
| `STRING`        | `string-set`    | {:tag :string-set} | the set of all strings |
| `size(s)`       | `(count-seq s)` |  |  |
| `rev(s)`        | `(reverse s)`   |  |  |
| `s^t`           | `(concat s t)`  |  |  |
| `conc(ss)`      | `(conc ss)`      |  |  |
##Reals
##Trees
##Let and If-Then-Else
| B                                                                | Lisb                                        | IR                                                         | Description                 |
|------------------------------------------------------------------|---------------------------------------------|------------------------------------------------------------|-----------------------------|
| `IP cond THEN then ELSE else END`                                | `(if-expr cond then else)`                  | {:tag :if-expr, :cond cond, :then then, :else else}        | conditional for expressions |
| `IP cond THEN then ELSE else END`                                | `(and (=> cond then) (=> (not cond) else))` |                                                            | conditional for predicates  |
| `LET id1,id2,... BE id1=val1 & id2=val2 ... IN expr-or-pred END` | `(let id-vals expr-or-pred)`                | {:tag :let, :id-vals id-vals, :expr-or-pred :expr-or-pred} |                             |
##Substitutions
| B                                                          | Lisb                            | IR | Description                          |
|------------------------------------------------------------|---------------------------------|--- |--------------------------------------|
| `skip`                                                     | `skip`                          | {:tag :skip} |  |
| `x := E`                                                   | `(assign x E)`                  | {:tag :assign, :id-vals id-vals} |  |
| `f(x) := E`                                                | ``                              |  |  |
| `x :: S`                                                   | `(becomes-element-of #{x} S)`   | {:tag :becomes-element-of, :ids ids, :set set}  |  |
| `x : (P)`                                                  | `(becomes-such #{x} P)`         | {:tag :becomes-such, :ids ids, :pred pred} |  |
| `x <-- OP(y)`                                              | `(operatian-call #{x} OP #{y}`  | {:tag :op-call, :ids ids, :op op, :args args} |  |
| `G&#124;&#124;H`                                           | `(parallel-substitution G H)`   | {:tag :parallel-subst, :substs substs} |  |
| `G;H`                                                      | `(sequential-substitution G H)` | {:tag :sequential-subst, :substs substs} |  |
| `ANY x,... WHERE P THEN G END`                             | `(any #{x} P G)`                | {:tag :any, :ids ids, :where where, :then then} |  |
| `LET x,... BE x=E & ... IN G END`                          | `(let-sub #{x} (= :x 1) G`      | {:tag :let-sub, :ids ids, :pred pred, :subst subst} |  |
| `VAR x,... IN G END`                                       | `(var #{x} skip`                | {:tag :var, :ids ids, :subst subst} |  |
| `PRE P THEN G END`                                         | `(pre P G)`                     | {:tag :pre, :pred pred, :subst subst} |  |
| `CHOICE G OR H END`                                        | `(choice G H)`                  | {:tag :choice, :substs substs} |  |
| `IF P THEN G END`                                          | `(if-sub P G)`                  | {:tag :if-sub, :cond cond, :then then} |  |
| `IF P THEN G ELSE H END`                                   | `(if-sub P G H)`                | {:tag :if-sub, :cond cond, :then then, :else else} |  |
| `IF P1 THEN G1 ELSIF P2 THEN G2 ... END`                   | `(cond P1 G1 P2 G2 ...)`        | {:tag :cond, :clauses clauses} |  |
| `IF P1 THEN G1 ESLIF P2 THEN G2 ... ELSE Gn END`           | `(cond P1 G1 P2 G2 ... Gn)`     | {:tag :cond, :clauses clauses} |  |
| `SELECT P THEN G WHEN ... WHEN Q THEN H END`               | `(select P G ... Q H)`          | {:tag :select, :clauses clauses} |  |
| `SELECT P THEN G WHEN ... WHEN Q THEN H ELSE I END`        | `(select P G ... Q H I)`        | {:tag :select, :clauses clauses} |  |
| `CASE E OF EITHER m THEN G OR n THEN H ... END END`        | ``  |  |  |
| `CASE E OF EITHER m THEN G OR n THEN H ... ELSE I END END` | ``  |  |  |
|   |   |  |  |
| `WHEN P THEN G END  is a synonym for SELECT P THEN G END`  | ``  |  |  |

##Machine clauses
###Machine reference
| B                 | Lisb            | IR                                         | Description                    |
|-------------------|-----------------|--------------------------------------------|--------------------------------|
| `name`            | `name`          | `{:tag :mch-ref, :name name}`              | `machine name`                 |
| `name(a1,a2,...)` | `[name & args]` | `{:tag :mch-ref, :name name, :args :args}` | `machine name with parameters` |
###Machine inclusion
| B                                | Lisb                    | IR                               | Description                          |
|----------------------------------|-------------------------|----------------------------------|--------------------------------------|
| `USES mch-name1,mch-name2,...`   | `(uses & mch-names)`    | `{:tag :uses :values mch-names}` |                                      |
| `INDLUDES mch-ref1,mch-ref2,...` | `(includes & mch-refs)` | `{:tag :sees :values mch-refs}`  |                                      |
| `SEES mch-name1,mch-name2,...`   | `(sees & mch-names)`    | `{:tag :sees :values mch-names}` |                                      |
| `EXTENDS mch-ref1,mch-ref2,...`  | `(extends & mch-refs)`  | `{:tag :sees :values mch-refs}`  |                                      |
| `PROMOTES op1,op2,...`           | `(promotes & ops)`      | `{:tag :promotes :values ops}`   |                                      |
###Machine section
| B                                  | Lisb                    | IR                                   | Description                          |
|------------------------------------|-------------------------|--------------------------------------|--------------------------------------|
| `CONSTRAINTS pred1 & pred2 & ...`  | `(constraints & preds)` | `{:tag :constraints, :values preds}` |                                      |
| `SETS set-def1;set-def2;...`       | `(sets & set-defs)`     | `{:tag :sets, :values set-defs}`     |                                      |
| `CONSTANTS id1,id2,...`            | `(constants & ids)`     | `{:tag :constants, :values ids}`     |                                      |
| `CONCRETE_CONSTANTS cx,cy,..`      | ``                      | ``                                   |                                      |
| `PROPERTIES pred1 & pred2 & ...`   | `(properties preds)`    | `{:tag :properties, :values preds}`  |                                      |
| `DEFINITIONS m(x,...) == BODY;...` | ``                      | ``                                   |                                      |
| `VARIABLES id1,id2,...`            | `(variables & ids)`     | `{:tag :variables, :values ids}`     |                                      |
| `CONCRETE_VARIABLES cv,cw,...`     | ``                      | ``                                   |                                      |
| `INVARIANT pred1 & pred2 & ...`    | `(invariants & preds)`  | `{:tag :invariants, :values preds}`  |                                      |
| `ASSERTIONS pred1;pred2;...`       | `(assertions & preds)`  | `{:tag :assertions, :values preds}`  |                                      |
| `INITIALISATION subst1;subst2;...` | `(init & substs)`       | `{:tag :init, :values substs}`       |                                      |
| `OPERATIONS ops`                   | `(operations & ops)`    | `{:tag :operations, :values ops}`    |                                      |

##Machine header
| B                 | Lisb            | IR                          | Description                  |
|-------------------|-----------------|-----------------------------|------------------------------|
| `name`            | `name`          | `{:name name}`              | machine name                 |
| `name(a1,a2,...)` | `[name & args]` | `{:name name, :args :args}` | machine name with parameters |

##Machine
| B                                                   | Lisb                                        | IR                                                                          | Description         |
|-----------------------------------------------------|---------------------------------------------|-----------------------------------------------------------------------------|---------------------|
| `MACHINE header clauses END`                        | `(machine header & clauses)`                | `(merge {:tag :machine, :clauses clauses} header)`                          |                     |
| `MODEL header clauses END`                          | `(model header & clauses)`                  | `(merge {:tag :model, :clauses clauses} header)`                            | synonym for MACHINE |
| `SYSTEM header clauses END`                         | `(system header & clauses)`                 | `(merge {:tag :system, :clauses clauses} header)`                           | synonym for MACHINE |
| `REFINEMENT header REFINES ref-mch clauses END`     | `(refinement header ref-mch & clauses)`     | `(merge {:tag :refinement, :ref-mch ref-mch, clauses clauses} header)`      |                     |
| `IMPLEMENTATION header REFINES ref-mch clauses END` | `(implementation header ref-mch & clauses)` | `(merge {:tag :implementation, :ref-mch ref-mch, :clauses clauses} header)` |                     |
