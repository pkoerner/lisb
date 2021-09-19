(ns lisb.translation.util
  (:require [potemkin :refer [import-vars]]
            [lisb.translation ast2lisb ir2ast lisb2ir data-conversion])
  (:import
    (de.be4.classicalb.core.parser BParser)
    (de.be4.classicalb.core.parser.util PrettyPrinter)))

(import-vars [lisb.translation.ast2lisb ast->lisb])
(import-vars [lisb.translation.ir2ast ir->ast])
(import-vars [lisb.translation.data-conversion ensure-list convert])
(import-vars [lisb.translation.lisb2ir defpred pred almost-flatten wrap bempty-machine lisb->ir b bcall bexists bfor-all
              bnot b=> b<=> bor band bdistinct? bnot= b= bpred->bool bbool-set bsuperset-strict? bsuperset?
              bsubset-strict? bsubset? bmember? bcontains? bintersection-pe bunion-pe bintersect-sets bunite-sets
              bdifference bintersection bunion bcount bfin1 bfin bpow1 bpow bcomp-set bdec binc bpi bsigma bmod b** bdiv
              b* b- b+ bmin bmax b>= b<= b> b< bmax-int bmin-int brange binterval bnat1-set bnat-set bint-set
              bnatural1-set bnatural-set binteger-set brel bfnc biterate bclosure bclosure1 bprj2 bprj1 b|| bcomp b><
              b<+ bimage binverse b|>> b|> b<<| b<| bid bran bdom bcouple btotal-surjective-relation
              bsurjective-relation btotal-relation b<-> bapply blambda b>->> b>+>> b>-> b>+> b-->> b+->> b--> b+-> bdrop
              btake bconc brest bdrop-last blast bfirst breverse bconj bcons bconcat bsize bperm biseq1 biseq bseq1 bseq
              bsequence brec-get brecord bstruct bstring-set blet-pred blet-expr bif-expr bselect bcond bif-sub bchoice
              bassert bprecondition bvar blet-sub bany bsequence-substitution bparallel-substitution boperation-call
              bbecomes-such bbecomes-element-of bassign bblock bskip boperation boperations binit bassertions
              binvariants bvariables bdefinitions bproperties bconstants benumerated-set bdeferred-set bsets
              bconstraints bmachine-header bmachine-variant bmachine bmap-set bset-enum])

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

(defn b->lisb [b] (ast->lisb (b->ast b)))
(defn b-predicate->lisb [b-predicate] (ast->lisb (b-predicate->ast b-predicate)))
(defn b-expression->lisb [b-expression] (ast->lisb (b-expression->ast b-expression)))
(defn b-formula->lisb [b-formula] (ast->lisb (b-formula->ast b-formula)))
(defn b-substitution->lisb [b-substitution] (ast->lisb (b-substitution->ast b-substitution)))
(defn b-operation->lisb [b-operation] (ast->lisb (b-operation->ast b-operation)))

(defn b->ir [b] (lisb->ir (b->lisb b)))
(defn b-predicate->ir [b-predicate] (lisb->ir (b-predicate->lisb b-predicate)))
(defn b-expression->ir [b-expression] (lisb->ir (b-expression->lisb b-expression)))
(defn b-formula->ir [b-formula] (lisb->ir (b-formula->lisb b-formula)))
(defn b-substitution->ir [b-substitution] (lisb->ir (b-substitution->lisb b-substitution)))
(defn b-operation->ir [b-operation] (lisb->ir (b-operation->lisb b-operation)))

(defn lisb->ast [lisb] (ir->ast (lisb->ir lisb)))

(defn lisb->b [lisb] (ast->b (lisb->ast lisb)))

(defn ir->b [ir]
  (ast->b (ir->ast ir)))
