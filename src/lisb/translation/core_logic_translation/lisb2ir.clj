(ns lisb.translation.core-logic-translation.lisb2ir
  (:require [clojure.core.logic.pldb :as pldb])
  (:require [lisb.translation.core-logic-translation.database :as db])
  (:require [clojure.core.logic :refer :all])
  (:require [clojure.core.logic.protocols :refer [IWalkTerm walk-term]]))

(extend-protocol IWalkTerm
  clojure.lang.IPersistentSet
  (walk-term [v f] (with-meta (set (walk-term (seq v) f)) (meta v))))


(def primitive? 
  "predicate that returns true if argument is not a seq or a symbol." 
  (some-fn number? boolean? string? keyword?))

(declare new-translato)

; (defn list-same-counto-zip [l1 l2 zipped]
;   (conde [(emptyo l1) (emptyo l2) (emptyo zipped)]
;          [(fresh [h1 h2 t1 t2 hz tz]
;                  (conso h1 t1 l1)
;                  (conso h2 t2 l2)
;                  (== hz [h1 h2])
;                  (conso hz tz zipped)
;                  (list-same-counto-zip t1 t2 tz))]))

; (defn secondo [l e]
;   (fresh [tmp]
;          (resto l tmp)
;          (firsto tmp e)))

(defn maplisto
  ([g l1]
   (conde [(emptyo l1)]
          [(fresh [h t] (conso h t l1) (g h) (maplisto g t))]))
  ([g l1 l2]
   (conde [(emptyo l1) (emptyo l2)]
          [(fresh [h1 h2 t1 t2] (conso h1 t1 l1) (conso h2 t2 l2) (g h1 h2) (maplisto g t1 t2))])))

(defn match-values-with-keys [ks vs res]
  (conde [(fresh [tag] (== ks [[tag]])
                       (== [[tag vs]] res))]
         [(emptyo vs) (emptyo ks) (emptyo res)]
         [(fresh [ks-head ks-tail vs-head vs-tail res-head res-tail]
                 (conso ks-head ks-tail ks)
                 (conso vs-head vs-tail vs)
                 (conso res-head res-tail res)
                 (== res-head [ks-head vs-head])
                 (match-values-with-keys ks-tail vs-tail res-tail))]))

; (defn try-pairs-mappo [pairs mappo]
;   (conda [(nonlvaro mappo)
;           (fresh [tmp-pairs tag smaller-tmp-pairs]
;                  (project [mappo] (== tmp-pairs (seq mappo)))
;                  (rembero [:tag tag] tmp-pairs smaller-tmp-pairs)
;                  (conso [:tag tag] smaller-tmp-pairs pairs))]
;          [(lvaro mappo)
;           s#]))

(defna extract-mappo-auxo [ks m kv-pairs]
  ([[] _ []])
  ([[[h-ks] . t-ks] m [[h-ks v-ks] . t-pairs]]
   ;; we know: nonlvaro m
   (project [m h-ks] (== v-ks (get m h-ks)))
   (extract-mappo-auxo t-ks m t-pairs)) 
  ([[h-ks . t-ks] m [[h-ks v-ks] . t-pairs]]
   ;; we know: nonlvaro m
   (project [m h-ks] (== v-ks (get m h-ks)))
   (extract-mappo-auxo t-ks m t-pairs)))

(defn try-extract-mappo 
  [ks m kv-pairs]
  (conda [(emptyo ks) (emptyo kv-pairs)]
         [(nonlvaro m)
          (extract-mappo-auxo ks m kv-pairs) ]
         [(lvaro m)
          s#]))

(defn pairs-mappo [pairs mappo]
  (project [pairs] (== mappo (into {} pairs))))

(defn translate-name [lisb ir]
  (conde [(== lisb ir) (pred ir keyword?)]
         [(fresh [namey args]
                 (conso namey args lisb)
                 (== ir {:name namey, :args args}))]))

(defne wrap-machine-ref [translated wrapped]
  ([{:name namey :args :args} {:tag :machine-reference :name namey :args args}])
  ([namey {:tag :machine-reference :name namey}]))

(defnu translate-opo [lisb ir]
  ([[opname args body] {:tag :op, :name translated-name :returns [], :args args, :body translated-body}]
    (translate-name opname translated-name)
    (new-translato body translated-body))
  ([['<-- returns [opname args body]] {:tag :op, :returns returns, :name translated-name, :args args, :body translated-body}]
    (translate-name opname translated-name)
    (new-translato body translated-body)))


(defne treat-specialo [lisb ir]
  ([_ _]
   (fresh [_1 _2]
          (conso _1 _2 lisb) ; ensure list-parens ;; TODO: incomplete
          (matche [lisb]
                  ([[mch-decl mchname . clauses]]
                   (fresh [ir-tag translated-clauses translated-name]
                          (membero [mch-decl ir-tag] '[[machine :machine] [model :model] [system :system] ])
                          (== ir {:tag ir-tag :name translated-name :machine-clauses translated-clauses})
                          (maplisto new-translato clauses translated-clauses)
                          (translate-name mchname translated-name)))
                  ([[ref-decl mchname abstr-name . clauses]]
                   (fresh [ir-tag translated-clauses translated-name translated-abstr-name mch-ref]
                          (membero [ref-decl ir-tag] '[[refinement :refinement] [implementation :implementation]])
                          (== ir {:tag ir-tag, :name translated-name, :abstract-machine-name mch-ref :machine-clauses translated-clauses})
                          (maplisto new-translato clauses translated-clauses)
                          (translate-name mchname translated-name)  
                          (wrap-machine-ref translated-abstr-name mch-ref)  
                          (translate-name abstr-name translated-abstr-name)))
                  ([['operations . ops]]
                   (fresh [translated-ops]
                          (== ir {:tag :operations, :values translated-ops})
                          (maplisto translate-opo ops translated-ops)))
                  ;; TODO: try generate syntactic sugar only if nonlvaro lisb
                  ([['<-- returns ['op-call . opname-args]]]
                   (fresh [tmplisb newlisb]
                          (conso returns opname-args tmplisb)
                          (conso 'op-call tmplisb newlisb)
                          (new-translato newlisb ir)))
                  ([['for-all ids lhs rhs]]
                   (nonlvaro ids)
                   (new-translato `(~'for-all ~ids (~'implication ~lhs ~rhs)) ir))))))

;; TODO: ensure IR allows seqs for identifier lists, but DSL does not (?) 
(defn new-translato [lisb ir]
  (conda [(treat-specialo lisb ir)]
         [(conde
           [(== lisb ir)
            (pred lisb primitive?)]
           [(nonlvaro lisb) 
            (pred lisb set?)
            (fresh [lisb' ir']
                   (project [lisb] (== lisb' (or (seq lisb) [])))
                   (maplisto new-translato lisb' ir')
                   (project [ir'] (== ir (set ir')))) ]
           [(nonlvaro ir) 
            (pred ir set?)
            (fresh [lisb' ir']
                   (project [ir] (== ir' (or (seq ir) [])))
                   (maplisto new-translato lisb' ir')
                   (project [lisb'] (== lisb (set lisb'))))]
           [(== lisb ir)
            (pred lisb vector?)
            ] ;; NOTE: assuming vectors are only valid for collections of identifiers
           [(fresh [ir-tag ir-pairs-with-tag]
                   (featurec ir {:tag ir-tag})
                   (db/rules ir-tag lisb [])
                   (== ir-pairs-with-tag [[:tag ir-tag]])
                   (pairs-mappo ir-pairs-with-tag ir))]
           [(fresh [operator args ir-tag more-tags all-tags translatod-args _zippo ir-pairs ir-pairs-with-tag _1 _2]
                   (conso operator args lisb) 
                   (featurec ir {:tag ir-tag})
                   (db/rules ir-tag operator more-tags)
                   (conso _1 _2 more-tags) ;; NOTE: this is necessary to avoid backtracking from the branch above.
                                           ;;       otherwise, one could generate DSL code such as (nat-set)
                   (try-extract-mappo more-tags ir ir-pairs)
                   (match-values-with-keys more-tags translatod-args ir-pairs)
                   (conso [:tag ir-tag] ir-pairs ir-pairs-with-tag)
                   (maplisto new-translato args translatod-args)
                   (pairs-mappo ir-pairs-with-tag ir))]
           )]
         
         [(lvaro lisb) (nonlvaro ir) (project [ir] (throw (IllegalArgumentException. (str ir))))]
         [(nonlvaro lisb) (lvaro ir) (project [lisb] (throw (IllegalArgumentException. (str (list* lisb)))))]
         ))

(def translato new-translato)


(defn lisb->ir [lisb]
  (first (pldb/with-dbs [db/rules-tag-sym-args]
           (run 1 [q] (new-translato lisb q)))))

(defn ir->lisb [ir]
  (first (pldb/with-dbs [db/rules-tag-sym-args]
           (run 1 [q] (new-translato q ir)))))

(defn op->ir [lisb]
  (first (pldb/with-dbs [db/rules-tag-sym-args]
           (run 1 [q] (translate-opo lisb q)))))

(defn ir->op [ir]
  (first (pldb/with-dbs [db/rules-tag-sym-args]
           (run 1 [q] (translate-opo q ir)))))

#_(defn counto [l n]
  (conde [(emptyo l) (== 0 n)]
         [(fresh [_ tmptail tmpcnt]
                 (conso _ tmptail l)
                 (counto tmptail tmpcnt)
                 (== n (inc tmpcnt)))]))

(comment

  (ir->lisb (lisb->ir #{1 '(+ 1 1) 3}))
(run 1 [q] (== q #{1 2})
           (project [q] (== nil (println q)))
     )


(lisb->ir '(<-- [:a :b] (op-call :someop [:c :d])))
(lisb->ir '(<-- [:a :b] (op-call :someop [])))
(ir->lisb {:tag :op-call, :returns :res, :op :someop, :args :bla})
(op->ir '(<-- :res (:somename :args (< 1 2))))
(ir->lisb '{:tag :op, :returns :res, :name :somename, :args :args, :body {:tag :less, :nums (1 2)}})
(ir->lisb (lisb->ir '(op-call :res :someop :bla)))
(ir->lisb (lisb->ir '(=> (+ 1 2 3) :bar)))
(ir->lisb (lisb->ir '(for-all [:x] (member? :x nat-set) (<= :x 0))))
(ir->lisb (lisb->ir '(=> :foo :bar)))
(lisb->ir '(+ :foo :bar))
(lisb->ir '(assign :foo 42 :bar 43))
(lisb->ir '(+ "a" "b"))
(lisb->ir '["a" "b"])
(lisb->ir '(pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                (assign (fn-call :curDeadlines :timer) :deadline)))

(lisb->ir '(and (contains? :TIMERS :timer) (contains? natural-set :deadline)))
(lisb->ir '(contains? :TIMERS :timer))
(lisb->ir '(member? :TIMERS :timer))

(ir->lisb (lisb->ir '(machine :foo
                    (constants :bar)
                    (operations (:AbsoluteSetDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign (fn-call :curDeadlines :timer) :deadline)))
                                (<-- returns (:foo [] (assign :bla 42)))
                                )
                    )))


(ir->lisb (lisb->ir '(operations (:AbsoluteSetDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign (fn-call :curDeadlines :timer) :deadline)))
                                (<-- returns (:foo [] (assign :bla 42)))
                                )
                    ))


  (pldb/with-dbs [db/rules-tag-sym-args]
    (run 3 [q] (new-translato q {:tag :member, :elem :timer :set :TIMERS}))) 
  (pldb/with-dbs [db/rules-tag-sym-args]
    (run 1 [q] (new-translato '(assign :bla 42) q))) 

  (pldb/with-dbs [db/rules-tag-sym-args]
    (run 1 [q] (new-translato q '{:tag :assignment, :id-vals (:bla 42)}))) 

  (lisb->ir :x)
  (lisb->ir '(nat-set))
  (lisb->ir 'nat-set)
  (ir->lisb :x)
  (ir->lisb {:preds [:foo :bar], :tag :implication})
  (ir->lisb {:tag :implication :preds [:foo :bar]})
(lisb->ir '(=> :foo :bar))
(ir->lisb (lisb->ir '(=> (+ 1 2 3) (* (+ 1 4) 5) (* 6 7 8 ))))
(ir->lisb (lisb->ir '(implication (implication :quux :foo) :bar)))
(lisb->ir '(+ 1 4))

; (run 1 [x] 
;      (fresh [e y]
;             (== x y)
;             (== y [[:foo e]])
;             (everyg
;               #(fresh [e1 e2] 
;                       (firsto % e1) (secondo % e2)
;                       (new-translato e1 e2))
;               y)))

(run 1 [p q]
     (== q {:tag :sub, :nums [1 2]})
     (matche [p q]
             ([['+ . args] {:tag :add, :nums args'}]
              (maplisto == args args'))
             ([['- . args] {:tag :sub, :nums args'}]
              (maplisto == args args'))
             )
     )

(run 1 [n] (fresh [x] (== x 42) (== n [:foo x])))
(run 1 [n] (fresh [h t] (conso h t :a)))

(run 1 [n] (== n {1 2 3 4})) ;; okay
; (run 1 [n] (== n #{1 2 3 4})) ;; not okay


(run 3 [n] (match-values-with-keys [:foo :bar :bar] [1 2 3] n))
; (run 1 [n] (secondo [:foo :bar :bar] n))
(run 1 [n] (counto [:foo :bar :bar] n))
; (run 2 [n z] (list-same-counto-zip n [:foo :bar :bar] z))
  (pldb/with-dbs [db/rules-tag-sym-args]
    (run 1 [q p v] (featurec p {:foo q})
                   (== q {:foo v})
                   (== v 42)
         )
    
    ) 
(into {} [[:foo 0]])
    (run* [q v] (== q (apply hash-map [v 42]))
          (== v :foo)
          )

    (run* [q v] (== q (apply hash-map [:foo 42]))
          (project [q] (== v (:foo q)))
          )

    (run* [q] (mappo [:foo :bar] {:foo 42, :bar 43})
          )
    (run* [q] (mappo [:foo :bar] q)
          )
    (run* [q] (mappo [:foo :bar] q)
          (featurec q {:foo 42})
          (featurec q {:bar 43})
          )

    (run* [q r] 
          (== r [:foo :bar])
          (== q (zipmap r [1 2])))
    
  
  

     (run* [q] (== q :x) (pred keyword? q))
     (run* [q v x] (== q {:tag x, v 42})
                   (== x :foo)
                   (== v :bar)
                   )
     (run* [q v] (featurec q {:tag 3})
                 (== v :foo)
           )
   (pldb/with-dbs [db/rules-tag-sym-args
                   db/facts_args_ir db/facts_ops facts_special_lisb facts_special_ir]
     #_(run 1 [q] (db/has-args :op-call q))
     #_(run 1 [q] (db/has-args :op-call q))
    #_ (run 1 [q p r s] (db/rules :op-call q p) (conso r s p)
          
          ))
(= (quote (<-- (1 2) (op-call :someop []))) (first (translato {:tag :op-call, :returns [1 2], :op :someop, :args []})))
)

