(ns lisb.examples.eventb.vendingmachine
  (:require [lisb.translation.eventb.util :refer [eventb]]))

(def vendingmachine (eventb
                     (machine
                      :BeverageVendingMachine
                      (sees :BeverageVendingMachine_ctx)
                      (variables :paid :nsoda)
                      (invariants
                       (member? :paid bool-set)
                       (member? :nsoda (interval 0 :nmax)))
                      (init (assign :nsoda :nmax :paid false))
                      (events
                       (event :insert_coin (when (= :paid false)) (assign :paid true))
                       (event :ret_coin (when (= :paid true)) (assign :paid false))
                       (event :refill (when (= :paid false)) (assign :nsoda :nmax))
                       (event :sget
                        (when (> :nsoda 0) (= :paid true))
                        (assign :nsoda (- :nsoda 1))
                        (assign :paid false))))))

(def vendingmachine_ctx (eventb
                         (context
                           :BeverageVendingMachine_ctx
                           (constants :nmax)
                           (axioms (member? :nmax natural-set) (= :nmax 5)))))

