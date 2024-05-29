(ns lisb.examples.snake
  (:require [lisb.translation.lisb2ir :refer [b]]
            [lisb.translation.util :refer [ir->b]]
            [lisb.high-level :refer :all]
            [lisb.examples.simple :as simple]))

(def smalltrace
  (let [m (load-initialized-machine-trace (ir->b simple/bakery0))]
    (-> m
        (perform :enter1)
        (perform :try1)
        (perform :leave1))))

(def smalltrace-next-steps
  (possible-ops smalltrace))

(comment
  ; this wants to be a snake game example when grown up.
  (def snek (b (machine
                 :Snek
                 (variables :board :direction)
                 (invariants
                   ;types
                   (member? :board (total-function
                                    (* (range 1 9) (range 1 9))
                                    {0 1}))
                   (member? :direction (* (range -1 1) (range -1 1))))
                 (init (assign :board )) ; oh. how do i do set comprehension?
                 ))))

(comment

  (perform smalltrace (.getName (first smalltrace-next-steps)))

  (create-ns 'b)

  #_(intern 'b
          'take
          btake)
  )
