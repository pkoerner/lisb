{:name       :ACounter,
 :variables  #{:ii :jj},
 :invariants (and
               (member? :ii (interval 0 10))
               (member? :jj (interval 0 10))
               (< :ii 11)
               (>= :jj 0)),
 :init       (assign :ii 2 :jj 10),
 :operations #{(operation () :inc (select (> :jj 0) (parallel-substitution
                                                      (assign :ii (+ :ii 1))
                                                      (assign :jj (- :jj 1)))))
               (operation (:result) :res (assign :result :ii))}}
(machine
  (machine-variant)
  (machine-header :ACounter ())
  (variables :ii :jj)
  (invariants (and (member :ii (interval 0 10))
                     (member :jj (interval 0 10))
                     (< :ii 11)
                     (>= :jj 0)))
  (init (assign '(:ii :jj) '(2 10)))                        ; change assign
  (operations
    (operation () :inc () (select (> :jj 0) (parallel-substitution
                                                 (assign '(:ii) (list (+ :ii 1)))
                                                 (assign '(:jj) (list (- :jj 1))))))
    (operation '(:result) :res () (assign '(:result) '(:ii)))))
