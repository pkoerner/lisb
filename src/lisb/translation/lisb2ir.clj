(ns lisb.translation.lisb2ir)


(declare b=)
(declare band)
(declare bnot)
(declare bpred)

(defn bif
  ([node cond then] (assoc node :cond cond :then then))
  ([node cond then else]
   (if else
     (assoc node :cond cond :then then :else else)
     (bif node cond then))))

#_(defn blet [node kvs]
  (let [kv-pairs (partition 2 kvs)
        identifiers (map first kv-pairs)
        assignment (reduce band (map (partial apply b=) kv-pairs))]
    (assoc node :identifiers identifiers :assignment assignment)))


;;; parse units

(defn process-header [m header]
  (if (seqable? header)
    (let [[name & args] header]
      (assoc m :name name :args args))
    (assoc m :name header)))

(defn bmachine [header & clauses]
  (process-header {:tag :machine
                   :clauses clauses}
                  header))

(defn bmodel [header & clauses]
  (process-header {:tag :model
                   :clauses clauses}
                  header))

(defn bsystem [header & clauses]
  (process-header {:tag :system
                   :clauses clauses}
                  header))

(defn brefinement [header ref-mch & clauses]
  (process-header {:tag :refinement
                   :ref-mch ref-mch
                   :clauses clauses}
                  header))

(defn bimplementation [header ref-mch & clauses]
  (process-header {:tag :implementation
                   :ref-mch ref-mch
                   :clauses clauses}
                  header))


;;; machine clauses
;; machine inclusions

(defn process-mch-ref [mch-ref]
  (let [m {:tag :mch-ref}]
    (if (seqable? mch-ref)
      (let [[name & args] mch-ref]
        (assoc m :name name :args args))
      (assoc m :name mch-ref :args []))))

(defn bmch-ref [name & args]
  {:tag :mch-ref
   :name name
   :args args})

(defn buses [& mch-names]
  {:tag :uses
   :values mch-names})

(defn bincludes [& mch-refs]
  {:tag :includes
   :values (map process-mch-ref mch-refs)})

(defn bsees [& mch-names]
  {:tag :sees
   :values mch-names})

(defn bextends [& mch-refs]
  {:tag    :extends
   :values (map process-mch-ref mch-refs)})

(defn bpromotes [& ops]
  {:tag :promotes
   :values ops})

;; machine sections

(defn bconstraints [& preds]
  {:tag :contraints
   :values preds})

(defn bsets [& set-defs]
  {:tag :sets
   :values set-defs})
(defn bdeferred-set [id]
  {:tag :deferred-set
   :id id})
(defn benumerated-set [id & elems]
  {:tag :enumerated-set
   :id id
   :elems elems})

(defn bconstants [& ids]
  {:tag :constants
   :values ids})

(defn bproperties [& preds]
  {:tag :properties
   :values preds})

(defn bdefinitions [& defs]
  {:tag :definitions
   :values defs})

(defn bvariables [& ids]
  {:tag :variables
   :values ids})

(defn binvariants [& preds]
  {:tag :invariants
   :values preds})

(defn bassertions [& preds]
  {:tag :assertions
   :values preds})

(defn binit [& subs]
  {:tag :init
   :values subs})

(defn boperations [& op-defs]
  {:tag :operations
   :values op-defs})
(defn boperation [return-vals name args body]
  {:tag :operation
   :return-vals return-vals
   :name name
   :args args
   :body body})


;;; substitutions

(def bskip {:tag :skip})

(defn bblock [sub]
  {:tag :block
   :sub sub})

(defn bassign [& id-vals]
  {:tag :assign
   :id-vals id-vals})

(defn bbecomes-element-of [ids set]
  {:tag :becomes-element-of
   :ids ids
   :set set})

(defn bbecomes-such [ids pred]
  {:tag :becomes-such
   :ids ids
   :pred pred})

(defn bop-call [ids op args]
  {:tag :op-call
   :ids ids
   :op op
   :args args})

(defn bparallel-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :parallel-sub
     :subs subs}))

(defn bsequential-sub [& subs]
  (if (= 1 (count subs))
    (first subs)
    {:tag           :sequential-sub
     :subs subs}))

(defn bany [ids where then]
  {:tag :any
   :ids ids
   :where where
   :then then})

(defn blet-sub [id-vals sub]
  {:tag :let-sub
   :id-vals id-vals
   :sub sub})

(defn bvar [ids sub]
  {:tag :var
   :ids ids
   :sub sub})

(defn bprecondition [pred sub]
  {:tag :pre
   :pred pred
   :sub sub})

(defn bassert [pred sub]
  {:tag :assert
   :pred pred
   :sub sub})

(defn bchoice [& subs]
  {:tag :choice
   :subs subs})

(defn bif-sub
  ([cond then] (bif {:tag :if-sub} cond then))
  ([cond then else] (bif {:tag :if-sub} cond then else)))
(defn bcond [& clauses]
    {:tag :cond
     :clauses clauses})

(defn bselect [& clauses]
  {:tag :select
   :clauses clauses})

(defn bop-sub [op & args]
  {:tag :op-sub
   :op op
   :args args})


;;; if

(defn bif-expr
  ([cond then else] (bif {:tag :if-expr} cond then else))) ; else is always present


;;; let

(defn blet [id-vals expr-or-pred]
  {:tag :let
   :id-vals id-vals
   :expr-or-pred expr-or-pred})

;;; trees


;;; reals - (alpha - besser nicht verwenden)


;;; strings

(def bstring-set
  {:tag :string-set})


;;; records

(defn bstruct [& id-types]
  {:tag :struct
   :id-types id-types})

(defn brec [& id-vals]
  {:tag :rec
   :id-vals id-vals})

(defn bget [rec id]
  {:tag :get
   :rec rec
   :id id})


;;; sequences

(defn bsequence
  ([] {:tag :empty-sequence})
  ([& elems]
   {:tag    :sequence
    :elems elems}))

(defn bseq [set]
  {:tag :seq
   :set set})

(defn bseq1 [set]
  {:tag :seq1
   :set set})

(defn biseq [set]
  {:tag :iseq
   :set set})

(defn biseq1 [set]
  {:tag :iseq1
   :set set})

(defn bperm [set]
  {:tag :perm
   :set set})

(defn bsize [seq]
  {:tag :size
   :seq seq})

(defn bconcat [& seqs]
  {:tag :concat
   :seqs seqs})

(defn b-> [elem seq]
  {:tag :->
   :elem elem
   :seq seq})

(defn b<- [seq & elems]
  {:tag :<-
   :seq seq
   :elems elems})

(defn brev [seq]
  {:tag :rev
   :seq seq})

(defn bfirst [seq]
  {:tag :first
   :seq seq})

(defn blast [seq]
  {:tag :last
   :seq seq})

(defn bfront [seq]
  {:tag :front
   :seq seq})

(defn btail [seq]
  {:tag :tail
   :seq seq})

(defn bconc [seq-of-seqs]
  {:tag :conc
   :seq-of-seqs seq-of-seqs})

(defn btake [num seq]
  {:tag :take
   :seq seq
   :num num})

(defn bdrop [num seq]
  {:tag :drop
   :seq seq
   :num num})


;;; functions

(defn b+-> [& sets]
  {:tag :+->
   :sets sets})

(defn b--> [& sets]
  {:tag :-->
   :sets sets})

(defn b+->> [& sets]
  {:tag :+->>
   :sets sets})

(defn b-->> [& sets]
  {:tag :-->>
   :sets sets})

(defn b>+> [& sets]
  {:tag :>+>
   :sets sets})

(defn b>-> [& sets]
  {:tag :>->
   :sets sets})

(defn b>+>> [& sets]
  {:tag :>+>>
   :sets sets})

(defn b>->> [& sets]
  {:tag :>->>
   :sets sets})

(defn blambda [ids pred expr]
  {:tag :lambda
   :ids ids
   :pred pred
   :expr expr})
; sugar
(defn bfn [id-types expr]
  {:tag :fn
   :id-types id-types
   :expr expr})

(defn bapply [f & args]
  {:tag :apply
   :f f
   :args args})


;;; relations

(defn b<-> [& sets]
  {:tag :<->
   :sets sets})

(defn b<<-> [& sets]
  {:tag :<<->
   :sets sets})

(defn b<->> [& sets]
  {:tag :<->>
   :sets sets})

(defn b<<->> [& sets]
  {:tag :<<->>
   :sets sets})

(defn bcouple [left right]
  {:tag :couple
   :left left
   :right right})

(defn bdom [rel]
  {:tag :dom
   :rel rel})

(defn bran [rel]
  {:tag :ran
   :rel rel})

(defn bid [set]
  {:tag :id
   :set set})

(defn b<| [set rel]
  {:tag :domain-restriction
   :set set
   :rel rel})

(defn b<<| [set rel]
  {:tag :domain-subtraction
   :set set
   :rel rel})

(defn b|> [rel set]
  {:tag :range-restriction
   :rel rel
   :set set})

(defn b|>> [rel set]
  {:tag :range-subtraction
   :rel rel
   :set set})

(defn binverse [rel]
  {:tag :inverse
   :rel rel})

(defn bimage [rel set]
  {:tag :image
   :rel rel
   :set set})

(defn b<+ [& rels]
  {:tag :<+
   :rels rels})

(defn b>< [& rels]
  {:tag :><
   :rels rels})

(defn bcomp [& rels]
  {:tag :comp
   :rels rels})

(defn b|| [& rels]
  {:tag :parallel-product
   :rels rels})

(defn bprj1 [set1 set2]
  {:tag :prj1
   :set1 set1
   :set2 set2})

(defn bprj2 [set1 set2]
  {:tag :prj2
   :set1 set1
   :set2 set2})

(defn bclosure1 [rel]
  {:tag :closure1
   :rel rel})

(defn bclosure [rel]
  {:tag :closure
   :rel rel})

(defn biterate [rel num]
  {:tag :iterate
   :rel rel
   :num num})

(defn bfnc [rel]
  {:tag :fnc
   :rel rel})

(defn brel [rel]
  {:tag :rel
   :rel rel})


;;; numbers

(def binteger-set {:tag :integer-set})

(def bnatural-set {:tag :natural-set})

(def bnatural1-set {:tag :natural1-set})

(def bint-set {:tag :int-set})

(def bnat-set {:tag :nat-set})

(def bnat1-set {:tag :nat1-set})

(defn binterval [from to]
  {:tag :interval
   :from from
   :to to})

(defn brange [from to]
  (cond
    (number? to) (binterval from (dec to))
    (= :succ (:tag to)) (binterval from (:number to))
    :else (binterval from (bpred to))))

(def bmin-int
  {:tag :min-int})

(def bmax-int
  {:tag :max-int})

(defn bmax
  ([set]
   {:tag :max, :set set})
  ; adds enumeration to max
  ([num & nums]
   (let [nums (into #{num} nums)]
     (bmax nums))))

(defn bmin
  ([set]
   {:tag :min, :set set})
  ; adds enumeration to min
  ([num & nums]
   (let [nums (into #{num} nums)]
     (bmin nums))))

(defn b+ [& nums]
  {:tag :+
   :nums nums})

(defn b- [& nums]
  (if (= 1 (count nums))
    {:tag :unary-minus, :num (first nums)}
    {:tag :-, :nums nums}))

(defn b* [& nums]
  {:tag :*
   :nums nums})

(defn bdiv [& nums]
  {:tag :div
   :nums nums})

(defn b** [& nums]
  {:tag :**
   :nums nums})

(defn bmod [& nums]
  {:tag :mod
   :nums nums})

(defn bpi [ids pred expr]
  {:tag :pi
   :ids ids
   :pred pred
   :expr expr})

(defn bsigma [ids pred expr]
  {:tag :sigma
   :ids ids
   :pred pred
   :expr expr})

(defn bsucc [num]
  {:tag :succ
   :num num})

(defn bpred [num]
  {:tag :pred
   :num num})

; number predicates

(defn b< [& nums]
  {:tag :<
   :nums nums})

(defn b> [& nums]
  {:tag :>
   :nums nums})

(defn b<= [& nums]
  {:tag :<=
   :nums nums})

(defn b>= [& nums]
  {:tag :>=
   :nums nums})


;;; sets

(defn bcomp-set [ids pred]
  {:tag :comp-set
   :ids ids
   :pred pred})

(defn bpow [set]
  {:tag :power-set
   :set set})

(defn bpow1 [set]
  {:tag :power1-set
   :set set})

(defn bfin [set]
  {:tag :fin
   :set set})

(defn bfin1 [set]
  {:tag :fin1
   :set set})

(defn bcard [set]
  {:tag :card
   :set set})

(defn bunion [& sets]
  {:tag :union
   :sets sets})

(defn bintersection [& sets]
  {:tag :intersection
   :sets sets})

(defn bdifference [& sets]
  {:tag :difference
   :sets sets})

(defn bmember? [elem set]
  {:tag :member?
   :elem elem
   :set set})

; TODO: (bcontains-all? set & elements)
(defn bcontains? [set elem]
 (bmember? elem set))

(defn bsubset? [subset set]
  {:tag :subset?
   :subset subset
   :set set})

(defn bstrict-subset? [strict-subset set]
  {:tag :strict-subset?
   :strict-subset strict-subset
   :set set})

; adds superset to lisb
(defn bsuperset? [superset set]
  (bsubset? set superset))

; adds superset-strict to lisb
(defn bstrict-superset? [strict-superset set]
  (bstrict-subset? set strict-superset))

(defn bunite-sets [set-of-sets]
  {:tag :general-union
   :set-of-sets set-of-sets})

(defn bintersect-sets [set-of-sets]
  {:tag :general-intersection
   :set-of-sets set-of-sets})

(defn bunion-pe [ids pred expr]
  {:tag :union-pe
   :ids ids
   :pred pred
   :expr expr})

(defn bintersection-pe [ids pred expr]
  {:tag :intersection-pe
   :ids ids
   :pred pred
   :expr expr})


;;; booleans

(def bbool-set {:tag :bool-set})

(defn bpred->bool [pred]
  {:tag :pred->bool
   :pred pred})


;;; equality predicates

(defn b= [left right]
  {:tag :=
   :left left
   :right right})

(defn bnot= [left right]
  {:tag :not=
   :left left
   :right right})
; syntactic sugar
(defn bdistinct? [& elems]
  {:tag :distinct?
   :elements elems})


;;; logical predicates

(defn band [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :and
     :preds preds}))

(defn bor [& preds]
  (if (= 1 (count preds))
    (first preds)
    {:tag :or
     :preds preds}))

(defn b=> [& preds]
  {:tag :=>
   :preds preds})

(defn b<=> [& preds]
  {:tag :<=>
   :preds preds})

(defn bnot [pred]
  {:tag :not
   :pred pred})

(defn bfor-all
  ([ids =>-left =>-right]
   {:tag      :bfor-all
    :ids      ids
    :=>-left  =>-left
    :=>-right =>-right})
  ; syntactic sugar for most cases
  ([id-types pred]
   {:tag :for-all
    :id-types id-types
    :pred pred})
  )

(defn bexists [ids pred]
  {:tag :exists
   :ids ids
   :pred pred})


;;; misc

(defn bset-enum [& elements]
  {:tag :set-enum
   :elements elements})

(defn bmap-set [p s]
  (bran (blambda [:x] (bmember? :x s) (p :x))))


(defmacro b [lisb]
  `(let [
         ; parse units
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
         ~'variables bvariables
         ~'invariants binvariants
         ~'assertions bassertions
         ~'init binit
         ~'operations boperations
         ~'operation boperation

         ; substitutions
         ~'skip bskip
         ~'block bblock
         ~'assign bassign
         ~'becomes-element-of bbecomes-element-of
         ~'becomes-such bbecomes-such
         ~'op-call bop-call
         ~'parallel-sub bparallel-sub
         ~'|| bparallel-sub                                 ; sugar
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
         ~'op-sub bop-sub
         ;~'case bcase

         ; if
         ~'if-expr bif-expr

         ; let
         ~'let blet

         ; trees

         ; reals - (alpha - besser nicht verwenden)

         ; strings
         ~'string-set bstring-set

         ; records
         ~'struct bstruct
         ~'rec brec
         ~'get bget

         ; sequences
         ~'sequence bsequence
         ~'seq bseq
         ~'seq1 bseq1
         ~'iseq biseq
         ~'iseq1 biseq1
         ~'perm bperm
         ~'size bsize
         ~'concat bconcat
         ~'-> b->
         ~'<- b<-
         ~'rev brev
         ~'first bfirst
         ~'last blast
         ~'front bfront
         ~'drop-last bfront                                 ; clojure
         ~'tail btail
         ~'rest btail                                       ; clojure
         ~'conc bconc
         ~'take btake
         ~'drop bdrop

         ; functions
         ~'+-> b+->
         ~'--> b-->
         ~'+->> b+->>
         ~'-->> b-->>
         ~'>+> b>+>
         ~'>-> b>->
         ~'>+>> b>+>>
         ~'>->> b>->>

         ; functions altnames
         ~'partial-fn b+->
         ~'total-fn b-->
         ~'partial-surjection b+->>
         ~'total-surjection b-->>
         ~'partial-injection b>+>
         ~'total-injection b>->
         ~'partial-bijection b>+>>
         ~'total-bijection b>->>

         ~'lambda blambda
         ~'fn bfn  ; sugar
         ~'apply bapply

         ; relations
         ~'<-> b<->
         ~'<<-> b<<->
         ~'<->> b<->>
         ~'<<->> b<<->>
         ~'couple bcouple
         ~'dom bdom
         ~'ran bran
         ~'id bid
         ~'<| b<|
         ~'<<| b<<|
         ~'|> b|>
         ~'|>> b|>>
         ~'inverse binverse
         ~'image bimage
         ~'<+ b<+
         ~'>< b><
         ~'comp bcomp
         ~'|| b||
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
         ~'< b<
         ~'> b>
         ~'<= b<=
         ~'>= b>=
         ~'max bmax
         ~'min bmin
         ~'+ b+
         ~'- b-
         ~'* b*
         ~'/ bdiv
         ~'** b**
         ~'mod bmod
         ~'sigma bsigma
         ~'pi bpi
         ~'succ bsucc
         ~'inc bsucc                                        ; clojure
         ~'pred bpred
         ~'dec bpred                                        ; clojure

         ;;; sets
         ~'comp-set bcomp-set
         ~'pow bpow
         ~'pow1 bpow1
         ~'fin bfin
         ~'fin1 bfin1
         ~'card bcard
         ~'union bunion
         ~'intersection bintersection
         ~'difference bdifference
         ~'unite-sets bunite-sets
         ~'intersect-sets bintersect-sets
         ~'union-pe bunion-pe
         ~'intersection-pe bintersection-pe
         ~'member? bmember?
         ~'contains? bcontains?                             ; clojure
         ~'subset? bsubset?
         ~'strict-subset? bstrict-subset?
         ~'superset? bsuperset?                             ; sugar
         ~'strict-superset? bstrict-superset?               ; sugar

         ;;; booleans
         ~'bool-set bbool-set
         ~'pred->bool bpred->bool

         ;;; equality predicates
         ~'= b=
         ~'not= bnot=
         ; added funcionality
         ~'distinct? bdistinct?

         ;;; logical predicates
         ~'and band
         ~'or bor
         ~'<=> b<=>
         ~'=> b=>
         ~'not bnot
         ~'for-all bfor-all
         ~'exists bexists]
     ~lisb
    ))

(defn lisb->ir [lisb]
  (eval `(b ~lisb)))


(def bempty-machine (bmachine :Empty))


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


(defmacro defpred [name & args]
  `(def ~name (pred ~name ~@args)))
