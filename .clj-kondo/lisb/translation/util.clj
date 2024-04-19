(ns lisb.translation.util)

(defmacro b 
  [lisb]
  (let [pre-processed-lisb lisb]
    `(let [; parse units
           ~'machine bmachine
           ~'model bmodel
           ~'system bsystem
           ~'refinement brefinement
           ~'implementation bimplementation

           ; machine clauses
           ; machine inclusions
           ~'uses buses
           ~'includes bincludes
           ~'sees bsees
           ~'extends bextends
           ~'promotes bpromotes
           ; machine sections
           ~'constraints bconstraints
           ~'sets bsets
           ~'deferred-set bdeferred-set
           ~'enumerated-set benumerated-set
           ~'constants bconstants
           ~'properties bproperties
           ~'definitions bdefinitions
           ~'expression-definition bexpression-definition
           ~'predicate-definition bpredicate-definition
           ~'substitution-definition bsubstitution-definition
           ~'file-definition bfile-definition
           ~'freetypes bfreetypes
           ~'freetype bfreetype
           ~'constructor bconstructor
           ~'variables bvariables
           ~'invariants binvariants
           ~'assertions bassertions
           ~'init binit
           ~'operations boperations
           ~'op bop

           ; substitutions
           ~'skip bskip
           ~'block bblock
           ~'assign bassign
           ~'set! bassign                                   ; sugar
           ~'becomes-element-of bbecomes-element-of
           ~'becomes-member bbecomes-element-of             ; sugar
           ~'becomes-such bbecomes-such
           ~'op-call bop-call
           ~'op-call-with-returns bop-call-with-returns
           ~'parallel-sub bparallel-sub
           ~'|| bparallel-sub                               ; sugar
           ~'sequential-sub bsequential-sub
           ~'any bany
           ~'let-sub blet-sub
           ~'var-sub bvar
           ~'pre bprecondition
           ~'assert bassert
           ~'choice bchoice
           ~'if-sub bif-sub
           ~'cond bcond
           ~'select bselect
           ~'case bcase

           ; if
           ~'if bif ; this does not work because "if" is a special form, handled in pre-process-lisb
           ~'if-expr bif
           ~'if-pred bif

           ; let
           ~'let blet ; this DOES work because "let*" is the special form, "let" is just a macro
           ~'let-expr blet
           ~'let-pred blet

           ; trees

           ; reals - (alpha - besser nicht verwenden)
           ~'real 'bto-real
           ~'real-set 'breal-set
           ~'floor 'bfloor
           ~'ceil 'bceil

           ; strings
           ~'string-set bstring-set

           ; records
           ~'struct bstruct
           ~'record brecord
           ~'record-get brecord-get

           ; sequences
           ~'sequence bsequence
           ~'seq bseq
           ~'seq1 bseq1
           ~'iseq biseq
           ~'iseq1 biseq1
           ~'perm bperm
           ~'size bsize
           ~'concat bconcat
           ~'prepend bprepend
           ~'-> bprepend                                    ; sugar
           ~'append bappend
           ~'<- bappend                                     ; sugar
           ~'reverse breverse
           ~'first bfirst
           ~'last blast
           ~'front bfront
           ~'drop-last bfront                               ; clojure
           ~'tail btail
           ~'rest btail                                     ; clojure
           ~'conc bconc
           ~'take btake
           ~'drop bdrop

           ; functions
           ~'partial-function bpartial-function
           ~'+-> bpartial-function                          ; sugar
           ~'total-function btotal-function
           ~'--> btotal-function
           ~'partial-surjection bpartial-surjection
           ~'+->> bpartial-surjection
           ~'total-surjection btotal-surjection
           ~'-->> btotal-surjection
           ~'partial-injection bpartial-injection
           ~'>+> bpartial-injection
           ~'total-injection btotal-injection
           ~'>-> btotal-injection
           ~'partial-bijection bpartial-bijection
           ~'>+>> bpartial-bijection
           ~'total-bijection btotal-bijection
           ~'>->> btotal-bijection
           ~'lambda blambda
           ;~'fn bfn  ; sugar
           ~'fn-call bfn-call

           ; relations
           ~'relation brelation
           ~'<-> brelation                                  ; sugar
           ~'total-relation btotal-relation
           ~'<<-> btotal-relation                           ; sugar
           ~'surjective-relation bsurjective-relation
           ~'<->> bsurjective-relation                      ; sugar
           ~'total-surjective-relation btotal-surjective-relation
           ~'<<->> btotal-surjective-relation               ; sugar
           ~'maplet bmaplet
           ~'|-> bmaplet                                    ; sugar
           ~'dom bdom
           ~'ran bran
           ~'id bid
           ~'domain-restriction bdomain-restriction
           ~'<| bdomain-restriction                         ; sugar
           ~'domain-subtraction bdomain-subtraction
           ~'<<| bdomain-subtraction                        ; sugar
           ~'range-restriction brange-restriction
           ~'|> brange-restriction                          ; sugar
           ~'range-subtraction brange-subtraction
           ~'|>> brange-subtraction                         ; sugar
           ~'inverse binverse
           ~'image bimage
           ~'override boverride
           ~'<+ boverride                                   ; sugar
           ~'direct-product bdirect-product
           ~'>< bdirect-product                             ; sugar
           ~'composition bcomposition
           ~'parallel-product bparallel-product
           ~'prj1 bprj1
           ~'prj2 bprj2
           ~'closure1 bclosure1
           ~'closure bclosure
           ~'iterate biterate
           ~'fnc bfnc
           ~'rel brel

           ; numbers
           ~'integer-set binteger-set
           ~'natural-set bnatural-set
           ~'natural1-set bnatural1-set
           ~'int-set bint-set
           ~'nat-set bnat-set
           ~'nat1-set bnat1-set
           ~'interval binterval
           ~'range brange
           ~'min-int bmin-int
           ~'max-int bmax-int
           ~'> b>
           ~'< b<
           ~'>= b>=
           ~'<= b<=
           ~'max bmax
           ~'min bmin
           ~'+ b+
           ~'- b-
           ~'cart-or-mult bcart-or-mult
           ~'* b*                                           ; added separat multiplication
           ~'div bdiv
           ~'/ bdiv
           ~'** b**
           ~'mod bmod
           ~'pi bpi
           ~'π bpi                                          ; sugar
           ~'sigma bsigma
           ~'Σ bsigma                                       ; sugar
           ~'successor bsuccessor
           ~'inc bsuccessor                                 ; sugar
           ~'predecessor bpredecessor
           ~'dec bpredecessor                               ; sugar

           ;;; sets
           ~'comprehension-set bcomprehension-set
           ~'pow bpow
           ~'pow1 bpow1
           ~'fin bfin
           ~'fin1 bfin1
           ~'card bcard
           ~'cartesian-product bcartesian-product           ; added separat cartesian-product
           ; TODO: rebind ids in def-pred
           ;~'x bcartesian-product                             ; sugar
           ~'union bunion
           ~'intersection bintersection
           ~'set- bset-
           ~'member? bmember?
           ~'in bmember?                                    ; sugar
           ~'contains? bcontains?                           ; sugar
           ~'subset? bsubset?
           ~'strict-subset? bstrict-subset?
           ~'superset? bsuperset?                           ; sugar
           ~'strict-superset? bstrict-superset?             ; sugar
           ~'unite-sets bunite-sets
           ~'intersect-sets bintersect-sets
           ~'union-pe bunion-pe
           ~'intersection-pe bintersection-pe

           ;;; booleans
           ~'bool-set bbool-set
           ~'pred->bool bpred->bool

           ;;; equality predicates
           ~'= b=
           ~'not= bnot=
           ~'distinct? bdistinct?                           ; added funcionality

           ;;; logical predicates
           ~'and band
           ~'or bor
           ~'equivalence bequivalence
           ~'<=> bequivalence                               ; sugar
           ~'implication bimplication
           ~'=> bimplication                                ; sugar
           ~'not bnot
           ~'for-all bfor-all
           ~'exists bexists]
       ~pre-processed-lisb)))




(defn wrap [ctx node]
  (cond
    (keyword? node) `(~node ~ctx)
    (map? node) (into {} (map (fn f [[k v]]  [k (wrap ctx v)]) node))
    (set? node) (set (map (partial wrap ctx) node))
    (list? node) (apply list (map (partial wrap ctx) node))
    (vector? node) (vec  (map (partial wrap ctx) node))
    :otherwise node))


(defn almost-flatten [x]
  (remove coll? (rest (tree-seq coll? seq x))))


(defmacro pred [name & args]
  (let [body (last args)
        params (drop-last args)
        ctx (gensym 'lisb_ctx_)
        wrapped-body (wrap ctx body)
        keywords (set (filter keyword? (almost-flatten body)))]
    `(fn ~name ~@params
       (let [~ctx (into {} (mapv (fn [x#] [x# (keyword (gensym "lisb_"))]) ~keywords))]
         (do (b ~wrapped-body))))))


(defmacro defpred
  [name & args]
  `(def ~name (pred ~name ~@args)))