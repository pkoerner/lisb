(ns lisb.fset.examples
  (:require
   [lisb.translation.lisb2ir :refer [lisb->ir]]
   [lisb.fset.core :refer [print-transform!]]))

(def scheduler
  '(machine
    (machine-variant)
    (machine-header :scheduler [])
    (sets (deferred-set :PID))
    (variables :active :ready :waiting)
    (invariant (and
                (member? :active (pow :PID))
                (member? :ready (pow :PID))
                (member? :waiting (pow :PID))
                (subset? :active :PID)
                (subset? :ready :PID)
                (subset? :waiting :PID)))
    (init (assign :active #{} :ready #{} :waiting #{}))))


(comment

  (print-transform! (lisb->ir scheduler))

)
