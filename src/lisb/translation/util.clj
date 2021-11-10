(ns lisb.translation.util
  (:require [potemkin :refer [import-vars]]
            [lisb.translation ast2lisb ir2ast lisb2ir data-conversion])
  (:import
    (de.be4.classicalb.core.parser BParser)
    (de.be4.classicalb.core.parser.util PrettyPrinter)))

(import-vars [lisb.translation.ast2lisb ast->lisb])
(import-vars [lisb.translation.ir2ast ir->ast])
(import-vars [lisb.translation.data-conversion ensure-list convert])
(import-vars [lisb.translation.lisb2ir lisb->ir b
              ;;; parse units
              bmachine bmodel bsystem brefinement bimplementation
              ;;; machine clauses
              ;; machine inclusions
              buses bincludes bsees bextends bpromotes
              ;; machine sections
              bconstraints bsets bdeferred-set benumerated-set bconstants bproperties bdefinitions bvariables
              binvariants bassertions binit boperations
              ;;; substitutions
              bskip bblock bassign bbecomes-element-of bbecomes-such bop-call bparallel-sub bsequential-sub bany
              blet-sub bvar bprecondition bassert bchoice bif-sub bselect
              ;;; if
              bif-expr
              ;;; let
              blet
              ;;; trees
              ;;; reals
              ;;; string
              bstring-set
              ;;; records
              bstruct brecord brecord-get
              ;;; sequences
              bsequence bseq bseq1 biseq biseq1 bperm bsize bconcat bprepend bappend breverse bfirst blast bfront btail
              bconc btake bdrop
              ;;; functions
              bpartial-function btotal-function bpartial-surjection btotal-surjection bpartial-injection btotal-injection
              bpartial-bijection btotal-bijection blambda bfn-call
              ;;; relations
              brelation btotal-relation bsurjective-relation btotal-surjective-relation bmaplet bdom bran bid
              bdomain-restriction bdomain-subtraction brange-restriction brange-subtraction binverse bimage boverride
              bdirect-product bcomposition bparallel-product bprj1 bprj2 bclosure1 bclosure biterate bfnc brel
              ;;; numbers
              binteger-set bnatural-set bnatural1-set bint-set bnat-set bnat1-set binterval brange bmin-int bmax-int
              b< b> b<= b>= bmax bmin b+ b- b* bdiv b** bmod bpi bsigma bsuccessor bpredecessor
              ;;; sets
              bcomprehension-set bpow bpow1 bfin bfin1 bcard bunion bintersection bset- bmember? bcontains? bsubset?
              bstrict-subset? bsuperset? bstrict-superset? bunite-sets bunion-pe bintersect-sets bintersection-pe
              ;;; booleans
              bbool-set bpred->bool
              ;;; equality predicates
              b= bnot= bdistinct?
              ;;; logical predicates
              band bor bimplication bequivalence bnot bfor-all bexists
              ;;; misc
              bset-enum bmap-set defpred pred almost-flatten wrap bempty-machine])

(defn ast->b [ast]
  (let [pprinter (PrettyPrinter.)]
    (.apply ast pprinter)
    (.getPrettyPrint pprinter)))

(defn ast->ir [ast]
  (lisb->ir (ast->lisb ast)))

(defn b->ast [b] (.parse (BParser.) b false))
(defn b-formula->ast [b-formula] (.parseFormula (BParser.) b-formula))
(defn b-expression->ast [b-expression] (.parseExpression (BParser.) b-expression))
(defn b-substitution->ast [b-substitution] (.parseSubstitution (BParser.) b-substitution))
(defn b-predicate->ast [b-predicate] (.parsePredicate (BParser.) b-predicate))
(defn b-operation->ast [b-operation] (.parseTransition (BParser.) b-operation))
(defn b-machine-clause->ast [b-machine-clause] (b->ast (str "#MACHINECLAUSE" b-machine-clause)))

(defn b->lisb [b] (ast->lisb (b->ast b)))
(defn b-predicate->lisb [b-predicate] (ast->lisb (b-predicate->ast b-predicate)))
(defn b-expression->lisb [b-expression] (ast->lisb (b-expression->ast b-expression)))
(defn b-formula->lisb [b-formula] (ast->lisb (b-formula->ast b-formula)))
(defn b-substitution->lisb [b-substitution] (ast->lisb (b-substitution->ast b-substitution)))
(defn b-operation->lisb [b-operation] (ast->lisb (b-operation->ast b-operation)))
(defn b-machine-clause->lisb [b-machine-clause] (ast->lisb (b-machine-clause->ast b-machine-clause)))

(defn b->ir [b] (lisb->ir (b->lisb b)))
(defn b-predicate->ir [b-predicate] (lisb->ir (b-predicate->lisb b-predicate)))
(defn b-expression->ir [b-expression] (lisb->ir (b-expression->lisb b-expression)))
(defn b-formula->ir [b-formula] (lisb->ir (b-formula->lisb b-formula)))
(defn b-substitution->ir [b-substitution] (lisb->ir (b-substitution->lisb b-substitution)))
(defn b-operation->ir [b-operation] (lisb->ir (b-operation->lisb b-operation)))
(defn b-machine-clause->ir [b-machine-clause] (lisb->ir (b-machine-clause->lisb b-machine-clause)))

(defn lisb->ast [lisb] (ir->ast (lisb->ir lisb)))

(defn lisb->b [lisb] (ast->b (lisb->ast lisb)))

(defn ir->b [ir]
  (ast->b (ir->ast ir)))
