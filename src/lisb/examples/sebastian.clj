(ns lisb.examples.sebastian
  (:require [lisb.translation.lisb2ir :refer [lisb->ir b]]))

(def generic-timer-mc (b (machine
                           [:GenericTimersMC :TIMERS]
                           (variables :curDeadlines)
                           (invariants (contains? (+-> :TIMERS natural-set) :curDeadlines))
                           (init (assign :curDeadlines #{}))
                           (operations
                             (operation [] :AbsoluteSetDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign (apply :curDeadlines :timer) :deadline)))
                             (operation [] :AddDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign (apply :curDeadlines :timer) :deadline)))
                             (operation [] :TimeoutDeadline [:timer :deadline]
                                        (pre (and (contains? :TIMERS :timer) (contains? natural-set :deadline))
                                             (assign :curDeadlines (difference :curDeadlines #{[:timer :deadline]}))))
                             (operation [] :IncreaseTime [:delta]
                                        (select (and (contains? natural-set :delta) (=> (not= :curDeadlines #{}) (<= :delta (min (ran :curDeadlines)))))
                                                (assign :curDeadlines (lambda [:x] (contains? (dom :curDeadlines) :x) (- (apply :curDeadlines :x) :delta)))))
                             (operation [] :IncreaseTimeUntilDeadline [:timer :delta]
                                        (select (and (contains? natural-set :delta) (contains? (dom :curDeadlines) :timer) (= :delta (min (ran :curDeadlines))) (= :delta (apply :curDeadlines :timer)))
                                                (assign :curDeadlines (lambda [:x] (contains? (difference (dom :curDeadlines) #{:timer}) :x) (- (apply :curDeadlines :x) :delta)))))))))

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
                                  (operation [] :Send_command_cars_ry [] skip)
                                  (operation [] :Send_command_cars_y [] skip)
                                  (operation [] :Send_command_cars_g [] skip)
                                  (operation [] :Send_command_cars_r [] skip)
                                  (operation [] :Send_command_peds_r [] skip)
                                  (operation [] :Send_command_peds_g [] skip)
                                  (operation [] :Timeout [:cmd] (select (contains? :COMMANDS :cmd) skip))
                                  (operation [] :RTIME_Passes [:delta] (select (contains? natural-set :delta) skip))
                                  (operation [] :cars_ry [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (assign :tl_cars :redyellow)))
                                  (operation [] :cars_y [] (select (= :tl_cars :green) (assign :tl_cars :yellow)))
                                  (operation [] :cars_g [] (select (= :tl_cars :redyellow) (assign :tl_cars :green)))
                                  (operation [] :cars_r [] (select (= :tl_cars :yellow) (assign :tl_cars :red)))
                                  (operation [] :peds_r [] (select (= :tl_peds :green) (assign :tl_peds :red)))
                                  (operation [] :peds_g [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (assign :tl_peds :green)))))))

(def traffic-light-time-ref (b (refinement :TrafficLightTime_Ref :TrafficLight2
                                           (includes [:GenericTimersMC :COMMANDS])
                                           (variables :tl_cars :tl_peds)
                                           (init (parallel-sub (assign :tl_cars :red) (assign :tl_peds :red)))
                                           (operations
                                             (operation [] :Send_command_cars_ry [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (op-sub :AddDeadline :cmd_cars_redyellow 500)))
                                             (operation [] :Send_command_cars_y [] (select (= :tl_cars :green) (op-sub :AddDeadline :cmd_cars_yellow 500)))
                                             (operation [] :Send_command_cars_g [] (select (= :tl_cars :redyellow) (op-sub :AddDeadline :cmd_cars_green 500)))
                                             (operation [] :Send_command_cars_r [] (select (= :tl_cars :yellow) (op-sub :AddDeadline :cmd_cars_red 500)))
                                             (operation [] :Send_command_peds_r [] (select (= :tl_peds :green) (op-sub :AddDeadline :cmd_peds_red 500)))
                                             (operation [] :Send_command_peds_g [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (op-sub :AddDeadline :cmd_peds_green 500)))
                                             (operation [] :Timeout [:cmd] (select (and (contains? (dom :curDeadlines) :cmd) (= (apply :curDeadlines :cmd) 0)) (op-sub :TimeoutDeadline :cmd 0)))
                                             (operation [] :cars_ry [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (parallel-sub (assign :tl_cars :redyellow) (any [:delta] (contains? (interval 0 500) :delta) (op-sub :IncreaseTimeUntilDeadline :cmd_cars_redyellow :delta)))))
                                             (operation [] :cars_y [] (select (= :tl_cars :green) (parallel-sub (assign :tl_cars :yellow) (any [:delta] (contains? (interval 0 500) :delta) (op-sub :IncreaseTimeUntilDeadline :cmd_cars_yellow :delta)))))
                                             (operation [] :cars_g [] (select (= :tl_cars :redyellow) (parallel-sub (assign :tl_cars :green) (any [:delta] (contains? (interval 0 500) :delta) (op-sub :IncreaseTimeUntilDeadline :cmd_cars_green :delta)))))
                                             (operation [] :cars_r [] (select (= :tl_cars :yellow) (parallel-sub (assign :tl_cars :red) (any [:delta] (contains? (interval 0 500) :delta) (op-sub :IncreaseTimeUntilDeadline :cmd_cars_red :delta)))))
                                             (operation [] :peds_r [] (select (= :tl_peds :green) (parallel-sub (assign :tl_peds :red) (any [:delta] (contains? (interval 0 500) :delta) (op-sub :IncreaseTimeUntilDeadline :cmd_peds_red :delta)))))
                                             (operation [] :peds_g [] (select (and (= :tl_cars :red) (= :tl_peds :red)) (parallel-sub (assign :tl_peds :green) (any [:delta] (contains? (interval 0 500) :delta) (op-sub :IncreaseTimeUntilDeadline :cmd_peds_green :delta)))))
                                             (operation [] :RTIME_Passes [:delta] (select (contains? #{100} :delta) (op-sub :IncreaseTime :delta)))))))
