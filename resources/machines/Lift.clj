{:name       :Lift,
 :variables  #{:etage},
 :invariants (member? :etage (interval 0 99)),
 :init       (assign :etage 4),
 :operations #{(operation () :inc (pre (< :etage 99) (assign :etage (inc :etage))))
               (operation () :dec (pre (> :etage 0) (assign :etage (dec :etage))))}}

(machine
  (machine-variant)
  (machine-header :Lift ())
  (variables :etage)
  (invariants (member :etage (interval 0 99)))
  (init (block (assign '(:etage) '(4))))
  (operations
    (operation () :inc () (pre (< :etage 99) (assign '(:etage) (list (+ :etage 1)))))
    (operation () :dec () (pre (> :etage 0) (assign '(:etage) (list (- :etage 1)))))))