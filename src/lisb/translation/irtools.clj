(ns lisb.translation.irtools
  (:require [com.rpl.specter :as s]))


(defn TAG [t] (s/path #(= (:tag %) t)))
(def CLAUSES (s/if-path (s/must :ir) [:ir :clauses] [:machine-clauses]))
(defn CLAUSE [^clojure.lang.Keyword name] (s/path [CLAUSES s/ALL (TAG name)]))
(def ALL-KEYWORDS (s/walker keyword?))
(defn IR-NODE [x] (s/walker #(= (:tag %) x)))
(defn OPERATION [opname]
  [(CLAUSE :operations) :values s/ALL (s/path #(= (:name %) opname))])

(defn get-definitions 
  "Selects all definitions from the DEFINITION clause.
  Returns a map which maps the definition name to a map 
  containing the definition's arguments and
  the body the definition should be replaced by."
  [ir] 
  (into {} (map (fn [m] [(get m :name), {:args (get m :args), :replace-by (or (get m :expr) (get m :pred))}])
                (s/select [(CLAUSE :definitions) :values s/ALL] ir))))

(defn transform-single-definition-call
  "Transforms a single definition call based on a map of all definitions.
   This map can be obtained by calling (get-definitions ir).
   Does not take any scoping rules into account."
  [all-definitions definition-call]
  (let [definition (get all-definitions (:name definition-call))
                       args-replacement (zipmap (get definition :args) (get definition-call :args))]
                   (s/transform ALL-KEYWORDS
                                (fn [x] (args-replacement x x)) 
                                (get definition :replace-by))))

(defn replace-all-definitions 
  "Expands all definition calls in the provided IR.
  Does not take any scoping rules into account."
  [ir]
  (let [defs (get-definitions ir)]
    (s/transform (IR-NODE :definition-call)
                 (partial transform-single-definition-call defs)
                 ir)))

(defn get-operation 
  "Retrieves the operation named opname from the IR provided."
  [ir opname]
  (s/select-one (OPERATION (keyword opname)) ir))

; (meta (with-meta [:foo] {:bar :baz}))
 ;(def IDENTIFIERS
 ;  (s/recursive-path [] node
 ;                  (cond-pathes (fn [node] (#{:and :or :implication :equivalence} (:tag node))) [:preds s/ALL node]
 ;                               (fn [node] (#{:pred->bool :not} (:tag node))) [:pred node]
 ;                               (fn [node] (#{:power-set :power1-set :fin :fin1 :cardinality :max :min :id :seq :seq1 :iseq :iseq1 :perm} (:tag node))) [:set node]
 ;                               (fn [node] (#{:cartesian-product :union :intersection :difference :subset? :strict-subset? :relation :total-relation :surjective-relation :total-surjective-relation :partial-fn :total-fn :partial-surjection :total-surjection :partial-injection :partial-bijection :total-bijection} (:tag node))) [:sets s/ALL node]
 ;                               (fn [node] (#{:assignment} (:tag node))) [:id-vals s/ALL node]
 ;                               (fn [node] (#{:greater :less :greater-equals :less-equals :add :sub :div :pow :mod} (:tag node))) [:nums s/ALL node]
 ;                               (fn [node] (#{:member} (:tag node))) [(s/multi-path :elem :set) s/ALL node]
 ;                               (fn [node] (#{:nat-set :nat1-set :int-set :integer-set :natural-set :natural1-set} (:tag node))) [s/STOP]
 ;                               :otherwise s/STAY
 ;                           )
 ;                  )
 ;  )
; 
(def IDENTIFIERS
  (s/recursive-path [] p
                    (s/cond-path
                      (fn [node] (#{:nat-set :nat1-set :int-set :integer-set :natural-set :natural1-set} (:tag node))) s/STOP
                      (fn [node] (and (map? node) (contains? node :ids)))
                      [(s/view #(remove (set (:ids %)) 
                                       (s/select (s/multi-path
                                             [(s/must :subs) s/ALL p]
                                             [(s/must :pred) p]
                                             [(s/must :expr) p]
                                             [(s/must :set) p]
                                             [(s/must :implication) p]) %))
                              ) s/ALL]
                      #(or (set? %) (sequential? %)) [s/ALL p]
                      keyword? s/STAY
                      boolean? s/STOP
                      number? s/STOP
                      :otherwise (s/multi-path
                                   [(s/must :preds) s/ALL p]
                                   [(s/must :sets) s/ALL p]
                                   [(s/must :id-vals) s/ALL p]
                                   [(s/must :nums) s/ALL p]
                                   [(s/must :pred) p]
                                   [(s/must :set) p]
                                   [(s/must :elem) p]
                                   [(s/must :f) p]
                                   [(s/must :left) p]
                                   [(s/must :right) p]
                                   [(s/must :from) p]
                                   [(s/must :to) p]
                                   [(s/must :args) s/ALL p]
                                   ))
                    )
  )

(defn find-identifiers [ir]
  (set (s/select IDENTIFIERS ir)))

 (comment
(find-identifiers (binterval :x :y))
(s/select [s/ALL s/STAY] (s/select (s/multi-path (s/must :from) (s/must :to)) (binterval :x :y)))

; (mapcat #(s/select IDENTIFIERS %) (s/select [(CLAUSE :operations) :values s/ALL :body (s/multi-path :pred :subs)] ir))
(s/select [s/ALL IDENTIFIERS] (s/select [(CLAUSE :operations) :values s/ALL :body (s/multi-path :pred :subs)] ir))
(s/select IDENTIFIERS (first (s/select [(CLAUSE :operations) :values s/ALL :body (s/multi-path :pred :subs)] ir)))
(s/select [(s/multi-path :elem :set)] (first (:preds (first (s/select [(CLAUSE :operations) :values s/ALL :body (s/multi-path :pred :subs)] ir)))))
(s/select IDENTIFIERS :foo)
(s/select [s/STAY] :foo)
(use 'lisb.translation.lisb2ir)
(s/select IDENTIFIERS (bexists [:z] (bfor-all [:x :y] (band (b< :x :a :z) (b= :y :z) ))))
(s/select IDENTIFIERS (band :z (bexists [:z] (bfor-all [:x :y] (band (b< :x :a :z) (b= :y :z) )))))
(s/select (s/multi-path [(s/must :ids)] [(s/must :implication) IDENTIFIERS]) (bfor-all [:x] (b= 1 :x)))
(s/select IDENTIFIERS (b= 1 :x)))

(comment
  (def ir 
  '{:tag :machine,
   :machine-clauses ({:tag :constants, :values (:Coins)} 
                     {:tag :properties, :values ({:tag :subset, :sets (:Coins {:tag :nat1-set})} {:tag :equals, :left :Coins, :right #{1 2}})}
                     {:tag :definitions, :values ({:tag :expression-definition, :name :total, :args [:pp], :expr {:tag :sigma, :ids [:cc :nn], :pred {:tag :and, :preds ({:tag :member, :elem :cc, :set :Coins} {:tag :member, :elem :nn, :set {:tag :nat1-set}} {:tag :member, :elem {:tag :maplet, :left :cc, :right :nn}, :set :pp})}, :expr {:tag :cartesian-product-or-multiplication, :nums-or-sets (:cc :nn)}}}
                                                  {:tag :expression-definition, :name :diff, :args [:pp :qq], :expr {:tag :comprehension-set, :ids [:cc :nn], :pred {:tag :and, :preds ({:tag :member, :elem :cc, :set :Coins} {:tag :member, :elem :nn, :set {:tag :nat-set}} {:tag :equals, :left :nn, :right {:tag :sub, :nums ({:tag :fn-call, :f :pp, :args (:cc)} {:tag :fn-call, :f :qq, :args (:cc)})}})}}}
                                                  {:tag :predicate-definition, :name :less, :args [:pp :qq], :pred {:tag :for-all, :ids [:cc], :implication {:tag :implication, :preds ({:tag :member, :elem :cc, :set :Coins} {:tag :less-equals, :nums ({:tag :fn-call, :f :pp, :args (:cc)} {:tag :fn-call, :f :qq, :args (:cc)})})}}}
                                                  {:tag :expression-definition, :name :ASSERT_LTL, :args [], :expr "G F ([AddCoin] or [Empty] or [Total])"} {:tag :expression-definition, :name :SET_PREF_MAXINT, :args [], :expr 4})}
                     {:tag :variables, :values (:purse)} 
                     {:tag :invariants, :values ({:tag :member, :elem :purse, :set {:tag :total-fn, :sets (:Coins {:tag :nat-set})}})}
                     {:tag :init, :values ({:tag :assignment, :id-vals (:purse {:tag :cartesian-product-or-multiplication, :nums-or-sets (:Coins #{0})})})}
                     {:tag :operations, :values ({:tag :op, :returns [], :name :AddCoin, :args [:cc], :body {:tag :precondition, :pred {:tag :and, :preds ({:tag :member, :elem :cc, :set :Coins} {:tag :less, :nums ({:tag :fn-call, :f :purse, :args (:cc)} {:tag :max-int})})}, :subs ({:tag :assignment, :id-vals ({:tag :fn-call, :f :purse, :args (:cc)} {:tag :add, :nums ({:tag :fn-call, :f :purse, :args (:cc)} 1)})})}}
                                                 {:tag :op, :returns [:rr], :name :Pay, :args [:amount], :body {:tag :precondition, :pred {:tag :member, :elem :amount, :set {:tag :nat1-set}}, :subs ({:tag :any, :ids [:p2], :pred {:tag :and, :preds ({:tag :member, :elem :p2, :set {:tag :total-fn, :sets (:Coins {:tag :nat-set})}} {:tag :definition-call, :name :less, :args [:p2 :purse]} {:tag :equals, :left {:tag :definition-call, :name :total, :args [{:tag :definition-call, :name :diff, :args [:purse :p2]}]}, :right :amount})}, :subs ({:tag :parallel-sub, :subs ({:tag :assignment, :id-vals (:purse :p2)} {:tag :assignment, :id-vals (:rr {:tag :definition-call, :name :diff, :args [:purse :p2]})})})})}}
                                                 {:tag :op, :returns [:tt], :name :Total, :args [], :body {:tag :assignment, :id-vals (:tt {:tag :definition-call, :name :total, :args [:purse]})}}
                                                 {:tag :op, :returns [], :name :Empty, :args [], :body {:tag :select, :clauses ({:tag :equals, :left {:tag :definition-call, :name :total, :args [:purse]}, :right 0} {:tag :skip})}})}), :name :Purse, :args []}) 


  (replace-all-definitions ir)
  )


