(ns lisb.translation.eventb.cycle-test
  (:require [lisb.translation.eventb.util :refer [eventb lisb->ir ir->prob prob->lisb]]
            [clojure.test :refer [deftest are]]))

(deftest machine-context-test
  (are [ir] (= ir (-> ir ir->prob prob->lisb lisb->ir))
    (eventb (machine :Empty))
    (eventb (context :Empty_ctx)))
  )

(deftest machine-test
  (are [ir] (= ir (-> ir ir->prob prob->lisb lisb->ir))
    (eventb (machine :foo
                     (variables :x :y :z)
                     (init
                      (assign :x 1 :y 2)
                      (assign :z :nat))
                     (events
                      (event :magic (any :t) (when (> :x 0)) (then (assign :x :t))))))))

(deftest event-test
  (are [ir] (= ir (-> ir ir->prob prob->lisb lisb->ir))
    (eventb (event :empty))
    (eventb (event :event1 (then (assign :x 1 :y 2))))
    (eventb (event :event1 (when (> :y 2))))
    (eventb (event :event1 (any :t)))
    ))

(comment
  (-> (eventb (event :event1 (any :t)))
      ir->prob
      prob->lisb
      )
  )
