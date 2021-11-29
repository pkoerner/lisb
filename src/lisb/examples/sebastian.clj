(ns lisb.examples.sebastian
  (:require [lisb.translation.lisb2ir :refer [lisb->ir b]]))

(def generic-timer-mc (b (machine
                           [:GenericTimersMC :TIMERS]
                           (variables :curDeadlines)
                           (invariants (contains? (+-> :TIMERS natural-set) :curDeadlines))
                           (init (assign :curDeadlines #{}))
                           (operations
                             (:AbsoluteSetDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign (fn-call :curDeadlines :timer) :deadline)))
                             (:AddDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign (fn-call :curDeadlines :timer) :deadline)))
                             (:TimeoutDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign :curDeadlines (set- :curDeadlines #{(|-> :timer :deadline)}))))
                             (:IncreaseTime [:delta]
                                        (select (and (contains? natural-set :delta) (=> (not= :curDeadlines #{}) (<= :delta (min (ran :curDeadlines)))))
                                                (assign :curDeadlines (lambda [:x] (contains? (dom :curDeadlines) :x) (- (fn-call :curDeadlines :x) :delta)))))
                             (:IncreaseTimeUntilDeadline [:timer :delta]
                                        (select (and (contains? natural-set :delta) (contains? (dom :curDeadlines) :timer) (= :delta (min (ran :curDeadlines))) (= :delta (fn-call :curDeadlines :timer)))
                                                (assign :curDeadlines (lambda [:x] (contains? (set- (dom :curDeadlines) #{:timer}) :x) (- (fn-call :curDeadlines :x) :delta)))))))))

(def traffic-light2 (b (machine :TrafficLight2
                                (sets
                                  (enumerated-set :colors :red :redyellow :yellow :green)
                                  (enumerated-set :COMMANDS :cmd_cars_redyellow :cmd_cars_yellow :cmd_cars_green :cmd_cars_red :cmd_peds_red :cmd_peds_green))
                                (variables :tl_cars :tl_peds)
                                (invariants
                                  (contains? :colors :tl_cars)
                                  (contains? #{:green :red} :tl_peds)
                                  (or (= :tl_peds :red) (= :tl_cars :red)))
                                (init (parallel-sub (assign :tl_cars :red) (assign :tl_peds :red)))
                                (operations
                                  (:Send_command_cars_ry [] skip)
                                  (:Send_command_cars_y [] skip)
                                  (:Send_command_cars_g [] skip)
                                  (:Send_command_cars_r [] skip)
                                  (:Send_command_peds_r [] skip)
                                  (:Send_command_peds_g [] skip)
                                  (:Timeout [:cmd] (select (contains? :COMMANDS :cmd) skip))
                                  (:RTIME_Passes [:delta] (select (contains? natural-set :delta) skip))
                                  (:cars_ry [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (assign :tl_cars :redyellow)))
                                  (:cars_y [] (select (= :tl_cars :green) (assign :tl_cars :yellow)))
                                  (:cars_g [] (select (= :tl_cars :redyellow) (assign :tl_cars :green)))
                                  (:cars_r [] (select (= :tl_cars :yellow) (assign :tl_cars :red)))
                                  (:peds_r [] (select (= :tl_peds :green) (assign :tl_peds :red)))
                                  (:peds_g [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (assign :tl_peds :green)))))))

(def traffic-light-time-ref (b (refinement :TrafficLightTime_Ref :TrafficLight2
                                           (includes [:GenericTimersMC :COMMANDS])
                                           (variables :tl_cars :tl_peds)
                                           (init (parallel-sub (assign :tl_cars :red) (assign :tl_peds :red)))
                                           (operations
                                             (:Send_command_cars_ry [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (op-call :AddDeadline :cmd_cars_redyellow 500)))
                                             (:Send_command_cars_y [] (select (= :tl_cars :green) (op-call :AddDeadline :cmd_cars_yellow 500)))
                                             (:Send_command_cars_g [] (select (= :tl_cars :redyellow) (op-call :AddDeadline :cmd_cars_green 500)))
                                             (:Send_command_cars_r [] (select (= :tl_cars :yellow) (op-call :AddDeadline :cmd_cars_red 500)))
                                             (:Send_command_peds_r [] (select (= :tl_peds :green) (op-call :AddDeadline :cmd_peds_red 500)))
                                             (:Send_command_peds_g [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (op-call :AddDeadline :cmd_peds_green 500)))
                                             (:Timeout [:cmd] (select (and (contains? (dom :curDeadlines) :cmd) (= (fn-call :curDeadlines :cmd) 0)) (op-call :TimeoutDeadline :cmd 0)))
                                             (:cars_ry [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (parallel-sub (assign :tl_cars :redyellow) (any [:delta] (contains? (interval 0 500) :delta) (op-call :IncreaseTimeUntilDeadline :cmd_cars_redyellow :delta)))))
                                             (:cars_y [] (select (= :tl_cars :green) (parallel-sub (assign :tl_cars :yellow) (any [:delta] (contains? (interval 0 500) :delta) (op-call :IncreaseTimeUntilDeadline :cmd_cars_yellow :delta)))))
                                             (:cars_g [] (select (= :tl_cars :redyellow) (parallel-sub (assign :tl_cars :green) (any [:delta] (contains? (interval 0 500) :delta) (op-call :IncreaseTimeUntilDeadline :cmd_cars_green :delta)))))
                                             (:cars_r [] (select (= :tl_cars :yellow) (parallel-sub (assign :tl_cars :red) (any [:delta] (contains? (interval 0 500) :delta) (op-call :IncreaseTimeUntilDeadline :cmd_cars_red :delta)))))
                                             (:peds_r [] (select (= :tl_peds :green) (parallel-sub (assign :tl_peds :red) (any [:delta] (contains? (interval 0 500) :delta) (op-call :IncreaseTimeUntilDeadline :cmd_peds_red :delta)))))
                                             (:peds_g [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (parallel-sub (assign :tl_peds :green) (any [:delta] (contains? (interval 0 500) :delta) (op-call :IncreaseTimeUntilDeadline :cmd_peds_green :delta)))))
                                             (:RTIME_Passes [:delta] (select (contains? #{100} :delta) (op-call :IncreaseTime :delta)))))))
