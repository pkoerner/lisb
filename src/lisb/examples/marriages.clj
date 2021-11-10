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
                 (:born [:nn :ss]
                   (pre
                     (and
                       (contains? :PERSON :nn)
                       (not (contains? (union :male :female) :nn))
                       (contains? :SEX :ss))
                     (if-sub (= :ss :boy)
                             (assign :male (union :male #{:nn}))
                             (assign :female (union :female #{:nn})))))
                 (:die [:nn]
                   (pre
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
                     (:wed [:mm :ff]
                       (pre
                         (and
                           (contains? :male :mm)
                           (not (contains? (dom :marriage) :mm))
                           (contains? :female :ff)
                           (not (contains? (ran :marriage) :ff)))
                         (assign (fn-call :marriage :mm) :ff)))
                     (:part [:mm :ff]
                       (pre
                         (and
                           (contains? :male :mm)
                           (contains? :female :ff)
                           (contains? :marriage [:mm :ff]))
                         (assign :marriage (- :marriage #{[:mm :ff]}))))
                     (<-- [:pp] (:partner [:nn]
                                  (pre
                                    (and
                                      (contains? :PERSON :nn)
                                      (contains? (union (dom :marriage) (ran :marriage)) :nn))
                                    (if-sub (contains? (dom :marriage) :nn)
                                            (assign :pp (fn-call :marriage :nn))
                                            (assign :pp (fn-call (inverse :marriage) :nn))))))))))

(def registrar (b (machine
                    :Registrar
                    (extends :Marriage)
                    (includes :Life)
                    (promotes :born)
                    (operations
                      (:dies [:nn] (pre
                                     (and
                                       (contains? :PERSON :nn)
                                       (contains? (union :male :female) :nn))
                                     (parallel-sub
                                       (op-call :die :nn)
                                       (cond
                                         (contains? (dom :marriage) :nn) (op-call :part :nn (fn-call :marriage :nn))
                                         (contains? (ran :marriage) :nn) (op-call :part (fn-call (inverse :marriage) :nn) :nn)))))))))
