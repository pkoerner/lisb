(ns lisb.translation.lisb2ir
  (:require [clojure.walk :as walk]))

(defmacro b [lisb]
  (walk/postwalk
   (fn [form]
     (get {'min-int 'lisb.translation.lisb2ir/bmin-int
           'max-int 'lisb.translation.lisb2ir/bmax-int
           'integer-set 'lisb.translation.lisb2ir/binteger-set
           'natural-set 'lisb.translation.lisb2ir/bnatural-set
           'natural1-set 'lisb.translation.lisb2ir/bnatural1-set
           'int-set 'lisb.translation.lisb2ir/bint-set
           'nat-set 'lisb.translation.lisb2ir/bnat-set
           'nat1-set 'lisb.translation.lisb2ir/bnat1-set
           'skip 'lisb.translation.lisb2ir/bskip
           'bool-set 'lisb.translation.lisb2ir/bbool-set
           'string-set 'lisb.translation.lisb2ir/bstring-set
           '| :lisb-internal/any-value
           '<-- 'lisb.translation.lisb2ir/bop-call-with-returns
           'machine 'lisb.translation.lisb2ir/bmachine
           'model 'lisb.translation.lisb2ir/bmodel
           'system 'lisb.translation.lisb2ir/bsystem
           'refinement 'lisb.translation.lisb2ir/brefinement
           'implementation 'lisb.translation.lisb2ir/bimplementation
           'uses 'lisb.translation.lisb2ir/buses
           'includes 'lisb.translation.lisb2ir/bincludes
           'sees 'lisb.translation.lisb2ir/bsees
           'extends 'lisb.translation.lisb2ir/bextends
           'promotes 'lisb.translation.lisb2ir/bpromotes
           'constraints 'lisb.translation.lisb2ir/bconstraints
           'sets 'lisb.translation.lisb2ir/bsets
           'deferred-set 'lisb.translation.lisb2ir/bdeferred-set
           'enumerated-set 'lisb.translation.lisb2ir/benumerated-set
           'constants 'lisb.translation.lisb2ir/bconstants
           'properties 'lisb.translation.lisb2ir/bproperties
           'definitions 'lisb.translation.lisb2ir/bdefinitions
           'expression-definition 'lisb.translation.lisb2ir/bexpression-definition
           'predicate-definition 'lisb.translation.lisb2ir/bpredicate-definition
           'substitution-definition 'lisb.translation.lisb2ir/bsubstitution-definition
           'file-definition 'lisb.translation.lisb2ir/bfile-definition
           'freetypes 'lisb.translation.lisb2ir/bfreetypes
           'freetype 'lisb.translation.lisb2ir/bfreetype
           'constructor 'lisb.translation.lisb2ir/bconstructor
           'variables 'lisb.translation.lisb2ir/bvariables
           'invariants 'lisb.translation.lisb2ir/binvariants
           'assertions 'lisb.translation.lisb2ir/bassertions
           'init 'lisb.translation.lisb2ir/binit
           'operations 'lisb.translation.lisb2ir/boperations
           'op 'lisb.translation.lisb2ir/bop

           'block 'lisb.translation.lisb2ir/bblock
           'assign 'lisb.translation.lisb2ir/bassign
           'set! 'lisb.translation.lisb2ir/bassign                                   ; sugar
           'becomes-element-of 'lisb.translation.lisb2ir/bbecomes-element-of
           'becomes-member 'lisb.translation.lisb2ir/bbecomes-element-of             ; sugar
           'becomes-such 'lisb.translation.lisb2ir/bbecomes-such
           'op-call 'lisb.translation.lisb2ir/bop-call
           'op-call-with-returns 'lisb.translation.lisb2ir/bop-call-with-returns
           'parallel-sub 'lisb.translation.lisb2ir/bparallel-sub
           '|| 'lisb.translation.lisb2ir/bparallel-sub                               ; sugar
           'sequential-sub 'lisb.translation.lisb2ir/bsequential-sub
           'any 'lisb.translation.lisb2ir/bany
           'let-sub 'lisb.translation.lisb2ir/blet-sub
           'var-sub 'lisb.translation.lisb2ir/bvar
           'pre 'lisb.translation.lisb2ir/bprecondition
           'assert 'lisb.translation.lisb2ir/bassert
           'choice 'lisb.translation.lisb2ir/bchoice
           'if-sub 'lisb.translation.lisb2ir/bif-sub
           'cond 'lisb.translation.lisb2ir/bcond
           'select 'lisb.translation.lisb2ir/bselect
           'case 'lisb.translation.lisb2ir/bcase
           'if 'lisb.translation.lisb2ir/bif ; this does not work 'lisb.translation.lisb2ir/because "if" is a special form, handled in pre-process-lisb
           'if-expr 'lisb.translation.lisb2ir/bif
           'if-pred 'lisb.translation.lisb2ir/bif
           'let 'lisb.translation.lisb2ir/blet ; this DOES work 'lisb.translation.lisb2ir/because "let*" is the special form, "let" is just a macro
           'let-expr 'lisb.translation.lisb2ir/blet
           'let-pred 'lisb.translation.lisb2ir/blet
           'real 'bto-real
           'real-set 'breal-set
           'floor 'bfloor
           'ceil 'bceil

           'struct 'lisb.translation.lisb2ir/bstruct
           'record 'lisb.translation.lisb2ir/brecord
           'record-get 'lisb.translation.lisb2ir/brecord-get
           'sequence 'lisb.translation.lisb2ir/bsequence
           'seq 'lisb.translation.lisb2ir/bseq
           'seq1 'lisb.translation.lisb2ir/bseq1
           'iseq 'lisb.translation.lisb2ir/biseq
           'iseq1 'lisb.translation.lisb2ir/biseq1
           'perm 'lisb.translation.lisb2ir/bperm
           'size 'lisb.translation.lisb2ir/bsize
           'concat 'lisb.translation.lisb2ir/bconcat
           'prepend 'lisb.translation.lisb2ir/bprepend
           '-> 'lisb.translation.lisb2ir/bprepend                                    ; sugar
           'append 'lisb.translation.lisb2ir/bappend
           '<- 'lisb.translation.lisb2ir/bappend                                     ; sugar
           'reverse 'lisb.translation.lisb2ir/breverse
           'first 'lisb.translation.lisb2ir/bfirst
           'last 'lisb.translation.lisb2ir/blast
           'front 'lisb.translation.lisb2ir/bfront
           'drop-last 'lisb.translation.lisb2ir/bfront                               ; clojure
           'tail 'lisb.translation.lisb2ir/btail
           'rest 'lisb.translation.lisb2ir/btail                                     ; clojure
           'conc 'lisb.translation.lisb2ir/bconc
           'take 'lisb.translation.lisb2ir/btake
           'drop 'lisb.translation.lisb2ir/bdrop
           'partial-function 'lisb.translation.lisb2ir/bpartial-function
           '+-> 'lisb.translation.lisb2ir/bpartial-function                          ; sugar
           'total-function 'lisb.translation.lisb2ir/btotal-function
           '--> 'lisb.translation.lisb2ir/btotal-function
           'partial-surjection 'lisb.translation.lisb2ir/bpartial-surjection
           '+->> 'lisb.translation.lisb2ir/bpartial-surjection
           'total-surjection 'lisb.translation.lisb2ir/btotal-surjection
           '-->> 'lisb.translation.lisb2ir/btotal-surjection
           'partial-injection 'lisb.translation.lisb2ir/bpartial-injection
           '>+> 'lisb.translation.lisb2ir/bpartial-injection
           'total-injection 'lisb.translation.lisb2ir/btotal-injection
           '>-> 'lisb.translation.lisb2ir/btotal-injection
           'partial-bijection 'lisb.translation.lisb2ir/bpartial-bijection
           '>+>> 'lisb.translation.lisb2ir/bpartial-bijection
           'total-bijection 'lisb.translation.lisb2ir/btotal-bijection
           '>->> 'lisb.translation.lisb2ir/btotal-bijection
           'lambda 'lisb.translation.lisb2ir/blambda
           'fn 'lisb.translation.lisb2ir/bfn  ; sugar
           'fn-call 'lisb.translation.lisb2ir/bfn-call
           'relation 'lisb.translation.lisb2ir/brelation
           '<-> 'lisb.translation.lisb2ir/brelation                                  ; sugar
           'total-relation 'lisb.translation.lisb2ir/btotal-relation
           '<<-> 'lisb.translation.lisb2ir/btotal-relation                           ; sugar
           'surjective-relation 'lisb.translation.lisb2ir/bsurjective-relation
           '<->> 'lisb.translation.lisb2ir/bsurjective-relation                      ; sugar
           'total-surjective-relation 'lisb.translation.lisb2ir/btotal-surjective-relation
           '<<->> 'lisb.translation.lisb2ir/btotal-surjective-relation               ; sugar
           'maplet 'lisb.translation.lisb2ir/bmaplet
           '|-> 'lisb.translation.lisb2ir/bmaplet                                    ; sugar
           'dom 'lisb.translation.lisb2ir/bdom
           'ran 'lisb.translation.lisb2ir/bran
           'id 'lisb.translation.lisb2ir/bid
           'domain-restriction 'lisb.translation.lisb2ir/bdomain-restriction
           '<| 'lisb.translation.lisb2ir/bdomain-restriction                         ; sugar
           'domain-subtraction 'lisb.translation.lisb2ir/bdomain-subtraction
           '<<| 'lisb.translation.lisb2ir/bdomain-subtraction                        ; sugar
           'range-restriction 'lisb.translation.lisb2ir/brange-restriction
           '|> 'lisb.translation.lisb2ir/brange-restriction                          ; sugar
           'range-subtraction 'lisb.translation.lisb2ir/brange-subtraction
           '|>> 'lisb.translation.lisb2ir/brange-subtraction                         ; sugar
           'inverse 'lisb.translation.lisb2ir/binverse
           'image 'lisb.translation.lisb2ir/bimage
           'override 'lisb.translation.lisb2ir/boverride
           '<+ 'lisb.translation.lisb2ir/boverride                                   ; sugar
           'direct-product 'lisb.translation.lisb2ir/bdirect-product
           '>< 'lisb.translation.lisb2ir/bdirect-product                             ; sugar
           'composition 'lisb.translation.lisb2ir/bcomposition
           'parallel-product 'lisb.translation.lisb2ir/bparallel-product
           'prj1 'lisb.translation.lisb2ir/bprj1
           'prj2 'lisb.translation.lisb2ir/bprj2
           'eventb-prj1 'lisb.translation.lisb2ir/beventb-prj1
           'eventb-prj2 'lisb.translation.lisb2ir/beventb-prj2
           'closure1 'lisb.translation.lisb2ir/bclosure1
           'closure 'lisb.translation.lisb2ir/bclosure
           'iterate 'lisb.translation.lisb2ir/biterate
           'fnc 'lisb.translation.lisb2ir/bfnc
           'rel 'lisb.translation.lisb2ir/brel

           'interval 'lisb.translation.lisb2ir/binterval
           'range 'lisb.translation.lisb2ir/brange

           '> 'lisb.translation.lisb2ir/b>
           '< 'lisb.translation.lisb2ir/b<
           '>= 'lisb.translation.lisb2ir/b>=
           '<= 'lisb.translation.lisb2ir/b<=
           'max 'lisb.translation.lisb2ir/bmax
           'min 'lisb.translation.lisb2ir/bmin
           '+ 'lisb.translation.lisb2ir/b+
           '- 'lisb.translation.lisb2ir/b-
           'cart-or-mult 'lisb.translation.lisb2ir/bcart-or-mult
           '* 'lisb.translation.lisb2ir/b*                                           ; added separat multiplication
           'div 'lisb.translation.lisb2ir/bdiv
           '/ 'lisb.translation.lisb2ir/bdiv
           '** 'lisb.translation.lisb2ir/b**
           'mod 'lisb.translation.lisb2ir/bmod
           'pi 'lisb.translation.lisb2ir/bpi
           'π 'lisb.translation.lisb2ir/bpi                                          ; sugar
           'sigma 'lisb.translation.lisb2ir/bsigma
           'Σ 'lisb.translation.lisb2ir/bsigma                                       ; sugar
           'successor 'lisb.translation.lisb2ir/bsuccessor
           'inc 'lisb.translation.lisb2ir/bsuccessor                                 ; sugar
           'predecessor 'lisb.translation.lisb2ir/bpredecessor
           'dec 'lisb.translation.lisb2ir/bpredecessor                               ; sugar
           'comprehension-set 'lisb.translation.lisb2ir/bcomprehension-set
           'pow 'lisb.translation.lisb2ir/bpow
           'pow1 'lisb.translation.lisb2ir/bpow1
           'fin 'lisb.translation.lisb2ir/bfin
           'fin1 'lisb.translation.lisb2ir/bfin1
           'card 'lisb.translation.lisb2ir/bcard
           'cartesian-product 'lisb.translation.lisb2ir/bcartesian-product           ; added separat cartesian-product
           'union 'lisb.translation.lisb2ir/bunion
           'intersection 'lisb.translation.lisb2ir/bintersection
           'set- 'lisb.translation.lisb2ir/bset-
           'member? 'lisb.translation.lisb2ir/bmember?
           'in 'lisb.translation.lisb2ir/bmember?                                    ; sugar
           'contains? 'lisb.translation.lisb2ir/bcontains?                           ; sugar
           'subset? 'lisb.translation.lisb2ir/bsubset?
           'strict-subset? 'lisb.translation.lisb2ir/bstrict-subset?
           'superset? 'lisb.translation.lisb2ir/bsuperset?                           ; sugar
           'strict-superset? 'lisb.translation.lisb2ir/bstrict-superset?             ; sugar
           'unite-sets 'lisb.translation.lisb2ir/bunite-sets
           'intersect-sets 'lisb.translation.lisb2ir/bintersect-sets
           'union-pe 'lisb.translation.lisb2ir/bunion-pe
           'intersection-pe 'lisb.translation.lisb2ir/bintersection-pe
           'pred->bool 'lisb.translation.lisb2ir/bpred->bool
           '= 'lisb.translation.lisb2ir/b=
           'not= 'lisb.translation.lisb2ir/bnot=
           'distinct? 'lisb.translation.lisb2ir/bdistinct?                           ; added funcionality
           'and 'lisb.translation.lisb2ir/band
           'or 'lisb.translation.lisb2ir/bor
           'equivalence 'lisb.translation.lisb2ir/bequivalence
           '<=> 'lisb.translation.lisb2ir/bequivalence                               ; sugar
           'implication 'lisb.translation.lisb2ir/bimplication
           '=> 'lisb.translation.lisb2ir/bimplication                                ; sugar
           'not 'lisb.translation.lisb2ir/bnot
           'for-all 'lisb.translation.lisb2ir/bfor-all
           'exists 'lisb.translation.lisb2ir/bexists}
          form
          form))
   lisb))

(defmacro pred [name & args]
  (let [body (last args)
        params (drop-last args)]
    `(fn ~name ~@params
       (b ~body))))


(defmacro defpred
  [name & args]
  `(def ~name (pred ~name ~@args)))