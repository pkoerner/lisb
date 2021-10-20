(ns lisb.examples.marriages
  (:require [lisb.translation.lisb2ir :refer [lisb->ir b]]))

(def life (b (machine
               :Life
               (sets
                 (deferred-set :PERSON)
                 (enumerated-set :SEX :boy :girl))
               (variables :male :female)
               (invariants
                 (subset? :male :PERSON)
                 (subset? :female :PERSON)
                 (= (intersection :male :female) #{}))
               (init
                 (parallel-sub
                   (assign :male #{})
                   (assign :female #{})))
               (operations
                 (operation [] :born [:nn :ss] (pre
                                                 (and
                                                   (contains? :PERSON :nn)
                                                   (not (contains? (union :male :female) :nn))
                                                   (contains? :SEX :ss))
                                                 (if-sub (= :ss :boy)
                                                         (assign :male (union :male #{:nn}))
                                                         (assign :female (union :female #{:nn})))))
                 (operation [] :die [:nn] (pre
                                            (and
                                              (contains? :PERSON :nn)
                                              (contains? (union :male :female) :nn))
                                            (if-sub (contains? :male :nn)
                                                    (assign :male (- :male #{:nn}))
                                                    (assign :female (- :female #{:nn})))))))))

(def marriage (b (machine
                   :Marriage
                   (uses :Life)
                   (variables :marriage)
                   (invariants
                     (contains? (>+> :male :female) :marriage))
                   (init
                     (assign :marriage #{}))
                   (operations
                     (operation [] :wed [:mm :ff]
                                (pre
                                  (and
                                    (contains? :male :mm)
                                    (not (contains? (dom :marriage) :mm))
                                    (contains? :female :ff)
                                    (not (contains? (ran :marriage) :ff)))
                                  (assign (apply :marriage :mm) :ff)))
                     (operation [] :part [:mm :ff]
                                (pre
                                  (and
                                    (contains? :male :mm)
                                    (contains? :female :ff)
                                    (contains? :marriage [:mm :ff]))
                                  (assign :marriage (- :marriage #{[:mm :ff]}))))
                     (operation [:pp] :partner [:nn]
                                (pre
                                  (and
                                    (contains? :PERSON :nn)
                                    (contains? (union (dom :marriage) (ran :marriage)) :nn))
                                  (if-sub (contains? (dom :marriage) :nn)
                                          (assign :pp (apply :marriage :nn))
                                          (assign :pp (apply (inverse :marriage) :nn)))))))))

(def registrar (b (machine
                    :Registrar
                    (extends :Marriage)
                    (includes :Life)
                    (promotes :born)
                    (operations
                      (operation [] :dies [:nn] (pre
                                                  (and
                                                    (contains? :PERSON :nn)
                                                    (contains? (union :male :female) :nn))
                                                  (parallel-sub
                                                    (op-sub :die :nn)
                                                    (cond
                                                      (contains? (dom :marriage) :nn) (op-sub :part :nn (apply :marriage :nn))
                                                      (contains? (ran :marriage) :nn) (op-sub :part (apply (inverse :marriage) :nn) :nn)))))))))
