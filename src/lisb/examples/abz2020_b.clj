(ns lisb.examples.abz2020-b
  (:require [lisb.translation.util :as util]
            [lisb.translation.eventb.util :as eventb-util]
            [com.rpl.specter :as s]
            [lisb.translation.eventb.dsl :as dsl]
            [lisb.translation.eventb.specter-util :refer :all]
            [lisb.translation.eventb.ir2eventb :refer [ir->prob rodin-name]]
            [lisb.translation.eventb.b2eventb :as b2eventb :refer [op->events sub->events]]))

(def Sensors
  (util/b (machine
 :Sensors
 (sets
  (enumerated-set :SWITCH_STATUS :switch_on :switch_off)
  (enumerated-set
   :PITMAN_POSITION
   :Neutral
   :Downward5
   :Downward7
   :Upward5
   :Upward7)
  (enumerated-set
   :KEY_STATE
   :NoKeyInserted
   :KeyInserted
   :KeyInsertedOnPosition))
 (constants :PITMAN_DIRECTION_BLINKING :PITMAN_TIP_BLINKING)
 (properties
  (= :PITMAN_DIRECTION_BLINKING #{:Upward7 :Downward7})
  (= :PITMAN_TIP_BLINKING #{:Downward5 :Upward5}))
 (variables
  :hazardWarningSwitchOn
  :pitmanArmUpDown
  :keyState
  :engineOn)
 (invariants
  (member? :hazardWarningSwitchOn :SWITCH_STATUS)
  (member? :pitmanArmUpDown :PITMAN_POSITION)
  (member? :keyState :KEY_STATE)
  (member? :engineOn bool-set))
 (init
  (parallel-sub
   (assign :hazardWarningSwitchOn :switch_off)
   (assign :pitmanArmUpDown :Neutral)
   (assign :keyState :KeyInsertedOnPosition)
   (assign :engineOn false)))
 (operations
  (:SET_EngineOn
   []
   (select
    (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
    (assign
 :engineOn true)))
  (:SET_EngineOff
   []
   (select (= :engineOn true) (assign :engineOn false)))
  (:SET_Pitman_DirectionBlinking
   [:newPos]
   (select
    (and
     (member? :newPos :PITMAN_DIRECTION_BLINKING)
     (not= :newPos :pitmanArmUpDown))
    (assign :pitmanArmUpDown :newPos)))
  (:SET_Pitman_Reset_to_Neutral
   []
   (select
    (not= :pitmanArmUpDown :Neutral)
    (assign :pitmanArmUpDown :Neutral)))
  (:SET_Pitman_Tip_blinking_short
   [:newPos]
   (select
    (and
     (member? :newPos :PITMAN_TIP_BLINKING)
     (not= :newPos :pitmanArmUpDown))
    (assign :pitmanArmUpDown :newPos)))
  (:SET_Hazard_blinking
   [:newSwitchPos]
   (select
    (and
     (member? :newSwitchPos :SWITCH_STATUS)
     (not= :newSwitchPos :hazardWarningSwitchOn))
    (assign :hazardWarningSwitchOn :newSwitchPos)))))))

(def BlinkLamps
  (util/b (machine
           :BlinkLamps_v3
           (sets
            (enumerated-set :DIRECTIONS :left_blink :right_blink :neutral_blink))
           (constants
            :BLINK_DIRECTION
            :LAMP_STATUS
            :lamp_on
            :lamp_off
            :continuousBlink
            :BLINK_CYCLE_COUNTER
            :cycleMaxLampStatus)
           (properties
            (= :BLINK_DIRECTION #{:left_blink :right_blink})
            (= :LAMP_STATUS #{0 100})
            (= :continuousBlink -1)
            (= :lamp_off 0)
            (= :lamp_on 100)
            (= :BLINK_CYCLE_COUNTER (interval -1 3))
            (member? :cycleMaxLampStatus (--> bool-set :LAMP_STATUS))
            (= :cycleMaxLampStatus #{(|-> true :lamp_on) (|-> false :lamp_off)}))
           (variables
            :active_blinkers
            :remaining_blinks
            :onCycle
            :blinkLeft
            :blinkRight)
           (invariants
            (subset? :active_blinkers :BLINK_DIRECTION)
            (member? :remaining_blinks :BLINK_CYCLE_COUNTER)
            (member? :blinkLeft :LAMP_STATUS)
            (member? :blinkRight :LAMP_STATUS)
            (member? :onCycle bool-set)
            (<=>
             (and
              (= :remaining_blinks 0)
              (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off)))
             (= :active_blinkers #{}))
            (=>
             (not= :blinkRight :lamp_off)
             (member? :right_blink :active_blinkers))
            (=>
             (not= :blinkLeft :lamp_off)
             (member? :left_blink :active_blinkers))
            (=> (= :active_blinkers :BLINK_DIRECTION) (= :blinkLeft :blinkRight))
            (=>
             (= :onCycle false)
             (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off)))
            (=>
             (and (= :onCycle true) (not= :active_blinkers #{}))
             (not (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off)))))
           (init
            (parallel-sub
             (assign :active_blinkers #{})
             (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
             (assign :remaining_blinks 0)
             (assign :onCycle false)))
           (operations
            (:SET_AllBlinkersOff
             []
             (parallel-sub
              (assign :active_blinkers #{})
              (assign :remaining_blinks 0)
              (assign :blinkLeft :lamp_off :blinkRight :lamp_off)))
            (:SET_AllBlinkersOn
             []
             (parallel-sub
              (assign :active_blinkers :BLINK_DIRECTION)
              (assign :remaining_blinks :continuousBlink)
              (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
              (assign :blinkRight (fn-call
                                   :cycleMaxLampStatus :onCycle))))
            (:SET_BlinkersOn
             [:direction :rem]
             (pre
              (and
               (member? :direction :BLINK_DIRECTION)
               (member? :rem :BLINK_CYCLE_COUNTER)
               (not= :rem 0))
              (parallel-sub
               (assign :active_blinkers #{:direction})
               (assign :remaining_blinks :rem)
               (if-sub
                (= :direction :right_blink)
                (parallel-sub
                 (assign :blinkLeft :lamp_off)
                 (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle)))
                (parallel-sub
                 (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
                 (assign :blinkRight :lamp_off))))))
            (:SET_RemainingBlinks
             [:rem]
             (pre
              (and
               (member? :rem :BLINK_CYCLE_COUNTER)
               (not= :rem 0)
               (not= :remaining_blinks 0))
              (assign :remaining_blinks :rem)))
            (:TIME_BlinkerOn
             []
             (select
              (and
               (= :blinkLeft :lamp_off)
               (= :blinkRight :lamp_off)
               (not= :remaining_blinks 0))
              (parallel-sub
               (assign :onCycle true)
               (if-sub
                (member? :left_blink :active_blinkers)

                (assign :blinkLeft :lamp_on))
               (if-sub
                (member? :right_blink :active_blinkers)
                (assign :blinkRight :lamp_on))
               (if-sub
                (> :remaining_blinks 0)
                (assign :remaining_blinks (- :remaining_blinks 1))))))
            (:TIME_BlinkerOff
             []
             (select
              (not (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off)))
              (parallel-sub
               (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
               (assign :onCycle false)
               (if-sub (= :remaining_blinks 0) (assign :active_blinkers #{})))))
            (:TIME_Nothing
             [:newOnCycle]
             (select
              (and
               (= :blinkLeft :lamp_off)
               (= :blinkRight :lamp_off)
               (= :active_blinkers #{})
               (= :newOnCycle false))
              (assign :onCycle :newOnCycle)))))))

(def PitmanController
  (util/b (machine
 :PitmanController_v6
 (includes :BlinkLamps_v3 :Sensors)
 (promotes
  (op-call :TIME_BlinkerOn)
  (op-call :TIME_BlinkerOff)
  (op-call :TIME_Nothing))
 (constants :pitman_direction :continuousBlink)
 (properties
  (=
   :pitman_direction
   #{(|-> :Upward7 :right_blink)
     (|-> :Upward5 :right_blink)
     (|-> :Neutral :neutral_blink)
     (|-> :Downward5 :left_blink)
     (|-> :Downward7 :left_blink)})
  (= :continuousBlink -1))
 (assertions
  (member? :pitman_direction (--> :PITMAN_POSITION :DIRECTIONS)))
 (invariants
  (=>
   (= :hazardWarningSwitchOn :switch_on)
   (= :active_blinkers :BLINK_DIRECTION))
  (=>
   (and
    (= :hazardWarningSwitchOn :switch_off)
    (= :remaining_blinks :continuousBlink))
   (=
    :active_blinkers
    #{(fn-call :pitman_direction :pitmanArmUpDown)}))
  (=>
   (and
    (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
    (= :engineOn true))
   (subset?
    #{(fn-call :pitman_direction :pitmanArmUpDown)}
    :active_blinkers))
  (=>
   (and (= :engineOn false) (= :hazardWarningSwitchOn :switch_off))
   (= :active_blinkers #{}))
  (=>
   (= :hazardWarningSwitchOn :switch_on)
   (= :remaining_blinks :continuousBlink))
  (=>
   (and
    (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
    (= :engineOn true))
   (= :remaining_blinks :continuousBlink)))
 (operations
  (:ENV_Turn_EngineOn
   []
   (parallel-sub
    (op-call :SET_EngineOn)
    (if-sub
     (and
      (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
      (= :hazardWarningSwitchOn :switch_off))
     (op-call
      :SET_BlinkersOn
      (fn-call :pitman_direction :pitmanArmUpDown)
      :continuousBlink))))
  (:ENV_Turn_EngineOff
   []
   (parallel-sub
    (op-call :SET_EngineOff)
    (if-sub
     (= :hazardWarningSwitchOn :switch_off)
     (op-call :SET_AllBlinkersOff))))
  (:ENV_Pitman_DirectionBlinking
   [:newPos]
   (pre
    (not= :newPos :pitmanArmUpDown)
    (parallel-sub
     (if-sub
      (and (= :hazardWarningSwitchOn :switch_off) (= :engineOn true))
      (op-call
       :SET_BlinkersOn
       (fn-call :pitman_direction :newPos)
       :continuousBlink))
     (op-call :SET_Pitman_DirectionBlinking :newPos))))
  (:ENV_Pitman_Reset_to_Neutral
   []
   (parallel-sub
    (op-call :SET_Pitman_Reset_to_Neutral)
    (if-sub
     (and
      (= :hazardWarningSwitchOn :switch_off)
      (= :remaining_blinks :continuousBlink))
     (op-call :SET_AllBlinkersOff))))
  (:ENV_Pitman_Tip_blinking_short
   [:newPos]
   (pre
    (and
     (member? :newPos :PITMAN_TIP_BLINKING)
     (not= :newPos :pitmanArmUpDown))
    (parallel-sub
     (op-call :SET_Pitman_Tip_blinking_short :newPos)
     (if-sub
      (and (= :hazardWarningSwitchOn :switch_off) (= :engineOn true))
      (op-call
       :SET_BlinkersOn
       (fn-call :pitman_direction :newPos)
       3)))))
  (:TIME_Tip_blinking_Timeout
   []
   (select
    (and
     (member? :pitmanArmUpDown :PITMAN_TIP_BLINKING)
     (> :remaining_blinks 1)
     (=
      :active_blinkers
      #{(fn-call :pitman_direction :pitmanArmUpDown)}))
    (op-call :SET_RemainingBlinks :continuousBlink)))
  (:ENV_Hazard_blinking
   [:newSwitchPos]
   (select
    (not= :newSwitchPos :hazardWarningSwitchOn)
    (parallel-sub
     (select
      (= :newSwitchPos :switch_on)
      (op-call :SET_AllBlinkersOn)
      (= :newSwitchPos :switch_off)
      (cond
       (or (= :pitmanArmUpDown :Neutral) (= :engineOn false))
       (op-call :SET_AllBlinkersOff)
       (not (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING))
       (op-call :SET_AllBlinkersOff)
       (op-call
        :SET_BlinkersOn
        (fn-call :pitman_direction :pitmanArmUpDown)
        :remaining_blinks)))
     (op-call :SET_Hazard_blinking :newSwitchPos))))))))

(defn replace-args [args op] (let [replacement (into {} (map vector (:args op)args))]
    (s/transform [:body (s/walker replacement)] replacement op)))

(comment
  (clojure.pprint/pp)

  (->> (-> PitmanController
           (b2eventb/includes->inline Sensors)
           (b2eventb/includes->refinement BlinkLamps))
       (s/select [(CLAUSE :operations) :values s/ALL
                  (NAME  :ENV_Turn_EngineOn)])
       first
       op->events
       clojure.pprint/pprint)

  (s/select [(CLAUSE :operations) :values s/ALL (NAME :SET_BlinkersOn)] BlinkLamps)

  (s/transform [(CLAUSE :operations) :values s/ALL]
               op->events
               (-> PitmanController
      (b2eventb/includes->inline Sensors)
      (b2eventb/includes->refinement BlinkLamps)))


  (->> (b2eventb/includes->inline PitmanController Sensors)
       b2eventb/extract-context
       ir->prob
       .getAxioms
      (map #(.getPredicate %)))

  (clojure.pprint/pprint BlinkLamps)

  (def pc (-> "../../models/abz2020-models-master/LightModel/PitmanController_v6.mch"
      slurp
      util/b->ir))

  (def sen (-> "../../models/abz2020-models-master/LightModel/Sensors.mch"
      slurp
      util/b->ir))

  (def bl (-> "../../models/abz2020-models-master/LightModel/BlinkLamps_v3.mch"
      slurp
      util/b->ir))

  (-> bl
    clojure.pprint/pprint
      )

  (as-> pc x
    (b2eventb/includes->inline x sen)
    (b2eventb/includes->refinement x bl)
    ((juxt b2eventb/extract-machine b2eventb/extract-context) x)
    (apply eventb-util/ir->prob-model (b2eventb/extract-machine bl) (b2eventb/extract-context bl) x)
    #_(clojure.pprint/pprint x)
    (eventb-util/prob-model->rodin x "translation" "./resources/eventb/abz2020_v4/" ))

  )
