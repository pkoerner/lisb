(ns lisb.translation.eventb.meta-dsl
  (:require [lisb.translation.eventb.dsl :refer [eventb]]
            [com.rpl.specter :as s]
            [lisb.translation.irtools :as irt]
            [lisb.translation.lisb2ir :refer [band]]
            ))

(defn transform [evtb-mch-ir]
  (let [mch-name (:name evtb-mch-ir)
        events-clause (:values (s/select-one (irt/CLAUSE :events) evtb-mch-ir))
        event-names (map :name events-clause)
        types [(eventb natural-set) (eventb natural-set)] ;; TODO: extract types, in order of variable names
        convergent (set (map :name (filter (fn [evt] (some #(= {:tag :status, :value :convergent} %) (:clauses evt))) events-clause)))
        ordinary (conj (clojure.set/difference (set event-names) convergent) :init)
        variant (:expr (s/select-one (irt/CLAUSE :variant) evtb-mch-ir))
        inv (apply band (:values (s/select-one (irt/CLAUSE :invariants) evtb-mch-ir)))
        ]
    `(eventb
      (~'context ~mch-name
               (~'sets :Ev)
               (~'constants ~@(conj event-names mch-name :init))
               (~'axioms 
                 (~'partition :Ev ~@(map hash-set (conj event-names :init)))
                 (~'member? ~mch-name (~'extended-pred :Machine [(~'cartesian-product ~@types) :Ev] []))
                 (~'= (~'extended-expr :Event [~mch-name] []) :Ev)
                 (~'= (~'extended-expr :Init  [~mch-name] []) :init)
                 (~'= (~'extended-expr :State [~mch-name] []) (~'cartesian-product ~@types))
                 (~'= (~'extended-expr :Thm   [~mch-name] []) nil)
                 (~'= (~'extended-expr :Inv   [~mch-name] []) '~inv)
                 (~'= (~'extended-expr :AP    [~mch-name] []) nil)
                 (~'= (~'extended-expr :BAP   [~mch-name] []) nil)
                 (~'= (~'extended-expr :Grd   [~mch-name] []) nil)
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

(clojure.repl/pst)
(def xx 
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
(apply band (:values (s/select-one (irt/CLAUSE :invariants) evtb-mch-ir)))
