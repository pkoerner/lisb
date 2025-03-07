(ns lisb.translation.eventb.irtools
  (:require [com.rpl.specter :as s] 
            [lisb.translation.irtools :as irt]))


(defn extract-labels [ir]
  (let [events (:values (s/select-one (irt/CLAUSE :events) ir))
        init (:values (s/select-one (irt/CLAUSE :init) ir))
        invariants (:values (s/select-one (irt/CLAUSE :invariants) ir))]
    (into {"INVARIANT" (into (sorted-map) (for [inv invariants] [(:label inv) (dissoc inv :label)]))}
     ;; TODO: not to sure about parameters in initialisation
     ;;"INITIALISATION" (into (sorted-map) (for [inv invariants] [(:label inv) (dissoc inv :label)]))
     (for [e events]
       [(name (:name e))
        (let [guards (:values (first (filter #(= (:tag %) :guards) (:clauses e))))
             actions (:values (first (filter #(= (:tag %) :guards) (:clauses e))))]
         (into (sorted-map) (for [el (concat guards actions)] [(:label el) (dissoc el :label)])))]))))

(comment
  (def ir '{:tag :machine, :name :Clock, :machine-clauses ({:tag :variables, :values (:h :m)}
                                                           {:tag :invariants, :values ({:tag :and, :preds ({:tag :member, :elem :m, :set {:tag :natural-set}} {:tag :member, :elem :h, :set {:tag :natural-set}}),
:label "inv1"} {:tag :and, :preds ({:tag :less, :nums (:m 60)} {:tag :less, :nums (:h 24)}), :label "inv2"} {:tag :theorem, :pred {:tag :or, :preds ({:tag :less, :nums (:m 59)} {:tag :or, :preds ({:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :less, :nums (:h 23)})} {:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :equals, :left :h, :right 23})})})}, :label "inv3"})}
                                                           {:tag :variant, :expr {:tag :sub, :nums ({:tag :mul, :nums (24 60)} 1 {:tag :add, :nums (:m {:tag :mul, :nums (:h 60)})})}}
                                                           {:tag :init, :values ({:tag :becomes-such, :ids [:m :h], :pred {:tag :and, :preds ({:tag :equals, :left :m', :right 0} {:tag :equals, :left :h', :right 0})}, :label "act1"})}
                                                           {:tag :events, :values ({:tag :event, :name :tick_min, :clauses ({:tag :status, :value :convergent} {:tag :guards, :values ({:tag :less, :nums (:m 59), :label "grd1"})} {:tag :actions, :values ({:tag :becomes-such, :ids [:m], :pred {:tag :equals, :left :m', :right {:tag :add, :nums (:m 1)}}, :label "act1"})})} {:tag :event, :name :tick_hour, :clauses ({:tag :status, :value :convergent} {:tag :guards, :values ({:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :less, :nums (:h 23)}), :label "grd1"})} {:tag :actions, :values ({:tag :becomes-such, :ids [:m :h], :pred {:tag :and, :preds ({:tag :equals, :left :m', :right 0} {:tag :equals, :left :h', :right {:tag :add, :nums (:h 1)}})}, :label "act1"})})} {:tag :event, :name :tick_midnight, :clauses ({:tag :guards, :values ({:tag :and, :preds ({:tag :equals, :left :m, :right 59} {:tag :equals, :left :h, :right 23}), :label "grd1"})} {:tag :actions, :values ({:tag :becomes-such, :ids [:m :h], :pred {:tag :and, :preds ({:tag :equals, :left :m', :right 0} {:tag :equals, :left :h', :right 0})}, :label "act1"})})})})})
  

(extract-labels ir)
)
