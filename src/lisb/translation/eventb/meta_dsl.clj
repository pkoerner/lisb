(ns lisb.translation.eventb.meta-dsl
  (:require [lisb.translation.eventb.dsl :refer [eventb]]
            [com.rpl.specter :as s]
            [lisb.translation.irtools :as irt]
            [lisb.translation.lisb2ir :refer [band b= bexists bmember? bcomprehension-set bor bmaplet]]
            ))

(defn keyword-addstr [kw suffix]
  (keyword (str (name kw) suffix)))

(defn primed [id]
  (keyword-addstr id "_primed"))

(defn bappify-subst [action]
  (case (:tag action)
    :skip {:written [], :pred true}
    :assignment (let [kvs (partition 2 (:id-vals action))]
                  {:written (map first kvs)
                   :pred (apply band (map (fn [[k v]] (b= (primed k) v)) kvs))}) ;; TODO: LHS might be function call
    :becomes-such {:written (:ids action)
                   :pred (clojure.walk/postwalk (fn [form] (get (into {} (for [x (:ids action)] [(keyword-addstr x "'") (primed x)])) form form))
                                                (:pred action))} ;; todo: replace all m'
    :becomes-element-of {:written [(first (:ids action))]
                         :pred (bmember? (primed (first (:ids action)))
                                  (:set action))}))

(defn predify-event [evt-id variables evt-ir]
  (let [namey (:name evt-ir)
        clauses (:clauses evt-ir)
        params (:values (first (filter #(= (:tag %) :args) clauses)))
        guards (:values (first (filter #(= (:tag %) :guards) clauses)))
        actions (:values (first (filter #(= (:tag %) :actions) clauses)))
        bappified (map bappify-subst actions)
        bap (apply band (b= evt-id namey)
                   (concat (map :pred bappified)
                           (map #(b= (primed %) %) (clojure.set/difference (set variables) (set (mapcat :written bappified)))) 
                           ))]
    {:guard (apply band guards)
     :bap bap}))

;(def evt-ir '{:tag :event, :name :tick_min, :clauses ({:tag
;:status, :value :convergent} {:tag :args, :values (:prm1)} {:tag :guards, :values ({:tag :and, :preds ({:tag :less, :nums (:m 59)} {:tag :equals, :left :prm1, :right 1})})} {:tag :actions, :values ({:tag :becomes-such, :ids [:m], :pred {:tag :equals, :left :m', :right {:tag :add, :nums (:m 1)}}})})})
;(predify-event :t [:m :h] evt-ir)
;
;(clojure.walk/postwalk #(if (keyword? %) (get {:m' :mp} % % ) %)  (:values (nth (:clauses evt-ir) 3)))
;(bappify-subst (first (:values (nth (:clauses evt-ir) 3))))

(defn transform [evtb-mch-ir]
  (let [event-name :t
        variant-name :v
        mch-name (keyword-addstr (:name evtb-mch-ir) "_auto")
        variables (:values (s/select-one (irt/CLAUSE :variables) evtb-mch-ir))
        events-clause (:values (s/select-one (irt/CLAUSE :events) evtb-mch-ir))
        evt-preds (map (partial predify-event event-name variables) events-clause)
        event-names (map :name events-clause)
        init-clause (:values (s/select-one (irt/CLAUSE :init) evtb-mch-ir))
        init-bap (apply band (map (comp :pred bappify-subst) init-clause))
        ap (bcomprehension-set (map primed variables) init-bap) ;; TODO review comprehension sets, I think there is an extra level of maplet ;; TODO: primed not necessary; but variables are primed internally
        types [(eventb natural-set) (eventb natural-set)] ;; TODO: extract types, in order of variable names
        convergent (set (map :name (filter (fn [evt] (some #(= {:tag :status, :value :convergent} %) (:clauses evt))) events-clause)))
        ordinary (conj (clojure.set/difference (set event-names) convergent) :init)
        variant (bcomprehension-set (concat variables [variant-name])
                                    (b= variant-name (:expr (s/select-one (irt/CLAUSE :variant) evtb-mch-ir))))
        grd (bcomprehension-set (cons event-name variables)
                                (apply bor (map :guard evt-preds))
                                (bmaplet event-name (apply bmaplet variables)))
        bap (bcomprehension-set (cons event-name variables)  
                                (apply bor (map :bap evt-preds))
                                (bmaplet event-name (bmaplet (apply bmaplet variables) (apply bmaplet (map primed variables))))
                                )
        inv (bcomprehension-set variables #_(apply bmaplet variables) (apply band (remove #(= (:tag %) :theorem) (:values (s/select-one (irt/CLAUSE :invariants) evtb-mch-ir)))))
        thm (bcomprehension-set variables #_(apply bmaplet variables) (apply band (map :pred (filter #(= (:tag %) :theorem) (:values (s/select-one (irt/CLAUSE :invariants) evtb-mch-ir))))))
        ]
    grd
    `(eventb
      (~'context ~mch-name
               (~'sets :Ev)
               (~'constants ~@(conj event-names mch-name :init))
               (~'axioms 
                 (~'partition :Ev ~@(map hash-set (conj event-names :init)))
                 (~'member? ~mch-name (~'extended-expr :Machine [(~'cartesian-product ~@types) :Ev] []))
                 (~'= (~'extended-expr :Event [~mch-name] []) :Ev)
                 (~'= (~'extended-expr :Init  [~mch-name] []) :init)
                 (~'= (~'extended-expr :State [~mch-name] []) (~'cartesian-product ~@types))
                 (~'= (~'extended-expr :Thm   [~mch-name] []) '~thm)
                 (~'= (~'extended-expr :Inv   [~mch-name] []) '~inv)
                 (~'= (~'extended-expr :AP    [~mch-name] []) '~ap)
                 (~'= (~'extended-expr :BAP   [~mch-name] []) '~bap)
                 (~'= (~'extended-expr :Grd   [~mch-name] []) '~grd)
                 (~'= (~'extended-expr :Convergent [~mch-name] []) ~convergent)
                 (~'= (~'extended-expr :Ordinary   [~mch-name] []) ~ordinary)
                 (~'= (~'extended-expr :Variant    [~mch-name] []) '~variant)

                 )
               )
    ))
  )

(defmacro event-b-meta
  [form]
  (transform (eval `(eventb ~form))))

(comment
(clojure.repl/pst)
(def evtb-mch-ir (eventb (machine :Clock
           (variables :h :m)
           (invariants (and (member? :m natural-set) (member? :h natural-set)) (and (< :m 60) (< :h 24)) (theorem (or (< :m 59) (or (and (= :m 59) (< :h 23)) (and (= :m 59) (= :h 23))))))
           (variant (- (* 24 60) 1 (+ :m (* :h 60))))
           (init (becomes-such [:m :h] (and (= :m' 0) (= :h' 0))))
           (events (event :tick_min (status :convergent) (when (< :m 59)) (then (becomes-such [:m] (= :m' (+ :m 1)))))
                   (event :tick_hour (status :convergent) (when (and (= :m 59) (< :h 23))) (then (becomes-such [:m :h] (and (= :m' 0) (= :h' (+ :h 1))))))
                   (event :tick_midnight (when (and (= :m 59) (= :h 23))) (then (becomes-such [:m :h] (and (= :m' 0) (= :h' 0)))))))))
(def ir 
(event-b-meta 
  (machine :Clock
           (variables :h :m)
           (invariants (and (member? :m natural-set) (member? :h natural-set)) (and (< :m 60) (< :h 24)) (or (< :m 59) (or (and (= :m 59) (< :h 23)) (and (= :m 59) (= :h 23)))))
           (variant (- (* 24 60) 1 (+ :m (* :h 60))))
           (init (becomes-such [:m :h] (and (= :m' 0) (= :h' 0))))
           (events (event :tick_min (status :convergent) (when (< :m 59)) (then (becomes-such [:m] (= :m' (+ :m 1)))))
                   (event :tick_hour (status :convergent) (when (and (= :m 59) (< :h 23))) (then (becomes-such [:m :h] (and (= :m' 0) (= :h' (+ :h 1))))))
                   (event :tick_midnight (when (and (= :m 59) (= :h 23))) (then (becomes-such [:m :h] (and (= :m' 0) (= :h' 0)))))))))
(:machine-clauses evtb-mch-ir)
({:tag :variables, :values (:h :m)}
 {:tag :invariants, :values ({:tag :and, :preds ({:tag :member, :elem :m, :set {:tag :natural-set}} {:tag :member, :elem :h, :set {:tag :natural-set}})} {:tag :and, :preds ({:tag :less, :nums (:m 60)} {:tag :less, :nums (:h 24)})} {:tag :or, :preds ({:tag :less, :nums (:m 59)} {:tag :or, :preds ({:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :less, :nums (:h 23)})} {:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :equals, :left :h, :right 23})})})})}
 {:tag :variant, :expr {:tag :sub, :nums ({:tag :mul, :nums (24 60)} 1 {:tag :add, :nums (:m {:tag :mul, :nums (:h 60)})})}}
 {:tag :init, :values ({:tag :becomes-such, :ids [:m :h], :pred {:tag :and, :preds ({:tag :equals, :left :m', :right 0} {:tag :equals, :left :h', :right 0})}})}
 {:tag :events, :values ({:tag :event, :name :tick_min, :clauses ({:tag :status, :value :convergent} {:tag :guards, :values ({:tag :less, :nums (:m 59)})} {:tag :actions, :values ({:tag :becomes-such, :ids [:m], :pred {:tag :equals, :left :m', :right {:tag :add, :nums (:m 1)}}})})}
                         {:tag :event, :name :tick_hour, :clauses ({:tag :status, :value :convergent} {:tag :guards, :values ({:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :less, :nums (:h 23)})})} {:tag :actions, :values ({:tag :becomes-such, :ids [:m :h], :pred {:tag :and, :preds ({:tag :equals, :left :m', :right 0} {:tag :equals, :left :h', :right {:tag :add, :nums (:h 1)}})}})})}
                         {:tag :event, :name :tick_midnight, :clauses ({:tag :guards, :values ({:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :equals, :left :h, :right 23})})} {:tag :actions, :values ({:tag :becomes-such, :ids [:m :h], :pred {:tag :and, :preds ({:tag :equals, :left :m', :right 0} {:tag :equals, :left :h', :right 0})}})})})})
(:values (first (filter #(= (:tag %) :events) (:machine-clauses evtb-mch-ir))))
(:values (first (filter #(= (:tag %) :events) (:machine-clauses evtb-mch-ir))))
(apply band (:values (s/select-one (irt/CLAUSE :invariants) evtb-mch-ir))))

(comment (require '[lisb.translation.eventb.util :as util])
  (util/prob-model->rodin (util/ir->prob-model ir) "MyModel" "/home/philipp/tmp/")



(util/ir->prob-model 
'{:tag :context, :name :Clock, :machine-clauses ({:tag :properties, :values 
                                                  (
                                                   {:tag :equals, :left 42, :right {:tag :comprehension-set,
                                                                                    ;:ids [{:tag :maplet, :elems (:t {:tag :maplet, :elems (:h :m)})}],
                                                                                    :ids [:t {:tag :maplet, :elems (:h :m)}],
                                                                                    :pred {:tag :equals :left 1 :right 1}}} )})}))
