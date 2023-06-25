(ns lisb.examples.eventb.abz2020
  (:require [lisb.translation.eventb.util :refer [eventb]]))

(def
  c0
  (eventb
   (context
     :c0
     (sets :DIRECTIONS)
     (constants
      :neutral_blink
      :left_blink
      :BLINK_DIRECTION
      :LAMP_STATUS
      :continuousBlink
      :cycleMaxLampStatus
      :lamp_off
      :right_blink
      :lamp_on
      :BLINK_CYCLE_COUNTER)
     (axioms
      (= :DIRECTIONS #{:neutral_blink :left_blink :right_blink})
      (= (card :DIRECTIONS) 3)
      (= :BLINK_DIRECTION #{:left_blink :right_blink})
      (= :LAMP_STATUS #{0 100})
      (and (= :lamp_off 0) (= :lamp_on 100))
      (= :BLINK_CYCLE_COUNTER (interval -1 3))
      (= :continuousBlink -1)
      (=
       :cycleMaxLampStatus
       #{(|-> true :lamp_on) (|-> false :lamp_off)})
      (member? :cycleMaxLampStatus (--> bool-set :LAMP_STATUS))))))

(def
  c1
  (eventb
   (context
     :c1
     (extends
      (context
        :c0
        (sets :DIRECTIONS)
        (constants
         :neutral_blink
         :left_blink
         :BLINK_DIRECTION
         :LAMP_STATUS
         :continuousBlink
         :cycleMaxLampStatus
         :lamp_off
         :right_blink
         :lamp_on
         :BLINK_CYCLE_COUNTER)
        (axioms
         (= :DIRECTIONS #{:neutral_blink :left_blink :right_blink})
         (= (card :DIRECTIONS) 3)
         (= :BLINK_DIRECTION #{:left_blink :right_blink})
         (= :LAMP_STATUS #{0 100})
         (and (= :lamp_off 0) (= :lamp_on 100))
         (= :BLINK_CYCLE_COUNTER (interval -1 3))
         (= :continuousBlink -1)
         (=
          :cycleMaxLampStatus
          #{(|-> true :lamp_on) (|-> false :lamp_off)})
         (member? :cycleMaxLampStatus (--> bool-set :LAMP_STATUS)))))
     (sets :PITMAN_POSITION :SWITCH_STATUS :KEY_STATE)
     (constants
      :KeyInserted
      :Upward5
      :Upward7
      :PITMAN_DIRECTION_BLINKING
      :pitman_direction
      :switch_off
      :Downward5
      :Downward7
      :Neutral
      :switch_on
      :KeyInsertedOnPosition
      :PITMAN_TIP_BLINKING
      :NoKeyInserted)
     (axioms
      (partition :SWITCH_STATUS [#{:switch_on} #{:switch_off}])
      (partition
       :PITMAN_POSITION
       [#{:Neutral} #{:Downward5} #{:Downward7} #{:Upward5} #{:Upward7}])
      (partition
       :KEY_STATE
       [#{:NoKeyInserted} #{:KeyInserted} #{:KeyInsertedOnPosition}])
      (= :PITMAN_DIRECTION_BLINKING #{:Upward7 :Downward7})
      (= :PITMAN_TIP_BLINKING #{:Downward5 :Upward5})
      (=
       :pitman_direction
       #{(|-> :Upward7 :right_blink)
         (|-> :Upward5 :right_blink)
         (|-> :Neutral :neutral_blink)
         (|-> :Downward5 :left_blink)
         (|-> :Downward7 :left_blink)})
      (member? :pitman_direction (--> :PITMAN_POSITION :DIRECTIONS))))))

(def
  BlinkLamps
  (eventb
   (machine
    :BlinkLamps
    (sees :c0)
    (variables
     :onCycle
     :active_blinkers
     :blinkLeft
     :remaining_blinks
     :blinkRight)
    (invariants
     (subset? :active_blinkers :BLINK_DIRECTION)
     (and
      (member? :blinkLeft :LAMP_STATUS)
      (member? :blinkRight :LAMP_STATUS))
     (member? :remaining_blinks :BLINK_CYCLE_COUNTER)
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
     (=>
      (= :active_blinkers :BLINK_DIRECTION)
      (= :blinkLeft :blinkRight))
     (=>
      (= :onCycle false)
      (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off)))
     (=>
      (and (= :onCycle true) (not= :active_blinkers #{}))
      (not (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off)))))
    (init
     (assign :active_blinkers #{})
     (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
     (assign :remaining_blinks 0)
     (assign :onCycle false))
    (events
     (event
      :SET_AllBlinkersOff
      (status :ordinary)
      (then
       (assign :active_blinkers #{})
       (assign :remaining_blinks 0)
       (assign :blinkLeft :lamp_off)
       (assign :blinkRight :lamp_off)))
     (event
      :SET_AllBlinkersOn
      (status :ordinary)
      (then
       (assign :active_blinkers :BLINK_DIRECTION)
       (assign :remaining_blinks :continuousBlink)
       (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))))
     (event
      :SET_LeftBlinkersOn
      (status :ordinary)
      (any :rem)
      (when (member? :rem :BLINK_CYCLE_COUNTER) (not= :rem 0))
      (then
       (assign :active_blinkers #{:left_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkRight :lamp_off)))
     (event
      :SET_RightBlinkersOn
      (status :ordinary)
      (any :rem)
      (when (member? :rem :BLINK_CYCLE_COUNTER) (not= :rem 0))
      (then
       (assign :active_blinkers #{:right_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkLeft :lamp_off)))
     (event
      :SET_RemainingBlinks
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (not= :remaining_blinks 0))
      (then (assign :remaining_blinks :rem)))
     (event
      :TIME_BlinkerOn
      (status :ordinary)
      (when
       (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))
        (not= :remaining_blinks 0))
      (then
       (assign
        :blinkLeft
        (fn-call
         #{(|-> true :lamp_on) (|-> false :lamp_off)}
         (pred->bool (member? :left_blink :active_blinkers))))
       (assign
        :blinkRight
        (fn-call
         #{(|-> true :lamp_on) (|-> false :lamp_off)}
         (pred->bool (member? :right_blink :active_blinkers))))
       (assign
        :remaining_blinks
        (max #{:continuousBlink (- :remaining_blinks 1)}))
       (assign :onCycle true)))
     (event
      :TIME_BlinkerOff
      (status :ordinary)
      (when
       (not (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))))
      (then
       (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
       (assign
        :active_blinkers
        (comprehension-set
         [:x]
         (and
          (member? :x :DIRECTIONS)
          (and
           (member? :x :active_blinkers)
           (not= :remaining_blinks 0)))))
       (assign :onCycle false)))
     (event
      :TIME_Nothing
      (status :ordinary)
      (any :newOnCycle)
      (when
       (not= :newOnCycle :onCycle)
        (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))
        (= :active_blinkers #{}))
      (then (assign :onCycle :newOnCycle)))))))

(def
  PitmanController
  (eventb
   (refinement
    :PitmanController
    :BlinkLamps
    (sees :c1)
    (variables
     :pitmanArmUpDown
     :onCycle
     :hazardWarningSwitchOn
     :active_blinkers
     :keyState
     :blinkLeft
     :remaining_blinks
     :blinkRight
     :engineOn)
    (invariants
     (member? :engineOn bool-set)
     (member? :pitmanArmUpDown :PITMAN_POSITION)
     (member? :hazardWarningSwitchOn :SWITCH_STATUS)
     (member? :keyState :KEY_STATE)
     (=>
      (= :hazardWarningSwitchOn :switch_on)
      (= :remaining_blinks :continuousBlink))
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
      (and
       (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
       (= :engineOn true))
      (= :remaining_blinks :continuousBlink))
     (=>
      (and (= :engineOn false) (= :hazardWarningSwitchOn :switch_off))
      (= :active_blinkers #{})))
    (init
     (assign :active_blinkers #{})
     (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
     (assign :remaining_blinks 0)
     (assign :onCycle false)
     (assign :engineOn false)
     (assign :pitmanArmUpDown :Neutral)
     (assign :hazardWarningSwitchOn :switch_off)
     (assign :keyState :KeyInsertedOnPosition))
    (events
     (event
      :ENV_Turn_EngineOn_Noblink
      (status :ordinary)
      (when
       (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
        (or
         (not (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING))
         (= :hazardWarningSwitchOn :switch_on)))
      (then (assign :engineOn true)))
     (event
      :ENV_Turn_EngineOn_BlinkLeft
      (event-refines :SET_LeftBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
        (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
        (= (fn-call :pitman_direction :pitmanArmUpDown) :left_blink)
        (= :hazardWarningSwitchOn :switch_off)
        (= :rem :continuousBlink))
      (then
       (assign :active_blinkers #{:left_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkRight :lamp_off)
       (assign :engineOn true)))
     (event
      :ENV_Turn_EngineOn_BlinkRight
      (event-refines :SET_RightBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
        (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
        (= (fn-call :pitman_direction :pitmanArmUpDown) :right_blink)
        (= :hazardWarningSwitchOn :switch_off)
        (= :rem :continuousBlink))
      (then
       (assign :active_blinkers #{:right_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkLeft :lamp_off)
       (assign :engineOn true)))
     (event
      :ENV_TurnEngineOff_Blink
      (status :ordinary)
      (when (= :engineOn true) (= :hazardWarningSwitchOn :switch_on))
      (then (assign :engineOn false)))
     (event
      :ENV_TurnEngineOff_Noblink
      (event-refines :SET_AllBlinkersOff)
      (status :ordinary)
      (when (= :engineOn true) (= :hazardWarningSwitchOn :switch_off))
      (then
       (assign :active_blinkers #{})
       (assign :remaining_blinks 0)
       (assign :blinkLeft :lamp_off)
       (assign :blinkRight :lamp_off)
       (assign :engineOn false)))
     (event
      :ENV_Pitman_Update_Nochange
      (status :ordinary)
      (any :newPos)
      (when
       (not= :pitmanArmUpDown :newPos)
        (not
         (and (= :hazardWarningSwitchOn :switch_off) (= :engineOn true)))
        (not= :newPos :Neutral))
      (then (assign :pitmanArmUpDown :newPos)))
     (event
      :ENV_Pitman_DirectionBlinkingLeft_Blink
      (event-refines :SET_LeftBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (not= :pitmanArmUpDown :Downward7)
        (= :hazardWarningSwitchOn :switch_off)
        (= :engineOn true)
        (= :rem :continuousBlink))
      (then
       (assign :active_blinkers #{:left_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkRight :lamp_off)
       (assign :pitmanArmUpDown :Downward7)))
     (event
      :ENV_Pitman_DirectionBlinkingRight_Blink
      (event-refines :SET_RightBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (not= :pitmanArmUpDown :Upward7)
        (= :hazardWarningSwitchOn :switch_off)
        (= :engineOn true)
        (= :rem :continuousBlink))
      (then
       (assign :active_blinkers #{:right_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkLeft :lamp_off)
       (assign :pitmanArmUpDown :Upward7)))
     (event
      :ENV_Pitman_TipBlinkingLeft_Blink
      (event-refines :SET_LeftBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (not= :pitmanArmUpDown :Downward5)
        (= :hazardWarningSwitchOn :switch_off)
        (= :engineOn true)
        (= :rem 3))
      (then
       (assign :active_blinkers #{:left_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkRight :lamp_off)
       (assign :pitmanArmUpDown :Downward5)))
     (event
      :ENV_Pitman_TipBlinkingRight_Blink
      (event-refines :SET_RightBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (not= :pitmanArmUpDown :Upward5)
        (= :hazardWarningSwitchOn :switch_off)
        (= :engineOn true)
        (= :rem 3))
      (then
       (assign :active_blinkers #{:right_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkLeft :lamp_off)
       (assign :pitmanArmUpDown :Upward5)))
     (event
      :ENV_Pitman_Reset_to_Neutral_Nochange
      (status :ordinary)
      (when
       (not= :pitmanArmUpDown :Neutral)
        (not
         (and
          (= :hazardWarningSwitchOn :switch_off)
          (= :remaining_blinks :continuousBlink))))
      (then (assign :pitmanArmUpDown :Neutral)))
     (event
      :ENV_Pitman_Reset_to_Neutral_Noblink
      (event-refines :SET_AllBlinkersOff)
      (status :ordinary)
      (when
       (not= :pitmanArmUpDown :Neutral)
        (= :hazardWarningSwitchOn :switch_off)
        (= :remaining_blinks :continuousBlink))
      (then
       (assign :active_blinkers #{})
       (assign :remaining_blinks 0)
       (assign :blinkLeft :lamp_off)
       (assign :blinkRight :lamp_off)
       (assign :pitmanArmUpDown :Neutral)))
     (event
      :ENV_HazardBlinkingOn
      (event-refines :SET_AllBlinkersOn)
      (status :ordinary)
      (when (= :hazardWarningSwitchOn :switch_off))
      (then
       (assign :active_blinkers :BLINK_DIRECTION)
       (assign :remaining_blinks :continuousBlink)
       (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
       (assign :hazardWarningSwitchOn :switch_on)))
     (event
      :ENV_HazardBlinkingOff
      (event-refines :SET_AllBlinkersOff)
      (status :ordinary)
      (when
       (= :hazardWarningSwitchOn :switch_on)
        (or
         (= :pitmanArmUpDown :Neutral)
         (or
          (= :engineOn false)
          (not (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)))))
      (then
       (assign :active_blinkers #{})
       (assign :remaining_blinks 0)
       (assign :blinkLeft :lamp_off)
       (assign :blinkRight :lamp_off)
       (assign :hazardWarningSwitchOn :switch_off)))
     (event
      :ENV_HazardBlinkingOff_LeftBlink
      (event-refines :SET_LeftBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (= :hazardWarningSwitchOn :switch_on)
        (not (or (= :pitmanArmUpDown :Neutral) (= :engineOn false)))
        (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
        (= (fn-call :pitman_direction :pitmanArmUpDown) :left_blink)
        (= :rem :remaining_blinks))
      (then
       (assign :active_blinkers #{:left_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkRight :lamp_off)
       (assign :hazardWarningSwitchOn :switch_off)))
     (event
      :ENV_HazardBlinkingOff_RightBlink
      (event-refines :SET_RightBlinkersOn)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (= :hazardWarningSwitchOn :switch_on)
        (not (or (= :pitmanArmUpDown :Neutral) (= :engineOn false)))
        (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
        (= (fn-call :pitman_direction :pitmanArmUpDown) :right_blink)
        (= :rem :remaining_blinks))
      (then
       (assign :active_blinkers #{:right_blink})
       (assign :remaining_blinks :rem)
       (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
       (assign :blinkLeft :lamp_off)
       (assign :hazardWarningSwitchOn :switch_off)))
     (event
      :TIME_Tip_blinking_Timeout
      (event-refines :SET_RemainingBlinks)
      (status :ordinary)
      (any :rem)
      (when
       (member? :rem :BLINK_CYCLE_COUNTER)
        (not= :rem 0)
        (not= :remaining_blinks 0)
        (> :remaining_blinks 1)
        (member? :pitmanArmUpDown :PITMAN_TIP_BLINKING)
        (=
         :active_blinkers
         #{(fn-call :pitman_direction :pitmanArmUpDown)})
        (= :rem :continuousBlink))
      (then (assign :remaining_blinks :rem)))
     (event
      :TIME_BlinkerOn
      (event-refines :TIME_BlinkerOn)
      (status :ordinary)
      (when
       (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))
        (not= :remaining_blinks 0))
      (then
       (assign
        :blinkLeft
        (fn-call
         #{(|-> true :lamp_on) (|-> false :lamp_off)}
         (pred->bool (member? :left_blink :active_blinkers))))
       (assign
        :blinkRight
        (fn-call
         #{(|-> true :lamp_on) (|-> false :lamp_off)}
         (pred->bool (member? :right_blink :active_blinkers))))
       (assign
        :remaining_blinks
        (max #{:continuousBlink (- :remaining_blinks 1)}))
       (assign :onCycle true)))
     (event
      :TIME_BlinkerOff
      (event-refines :TIME_BlinkerOff)
      (status :ordinary)
      (when
       (not (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))))
      (then
       (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
       (assign
        :active_blinkers
        (comprehension-set
         [:x]
         (and
          (member? :x :DIRECTIONS)
          (and
           (member? :x :active_blinkers)
           (not= :remaining_blinks 0)))))
       (assign :onCycle false)))
     (event
      :TIME_Nothing
      (event-refines :TIME_Nothing)
      (status :ordinary)
      (any :newOnCycle)
      (when
       (not= :newOnCycle :onCycle)
        (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))
        (= :active_blinkers #{}))
      (then (assign :onCycle :newOnCycle)))))))

(def PitmanController2_TIME
 (eventb
  (refinement
   :PitmanController2_TIME
   :PitmanController
   (sees :c1)
   (variables
    :tipStartTime
    :pitmanArmUpDown
    :onCycle
    :hazardWarningSwitchOn
    :curTime
    :active_blinkers
    :keyState
    :blinkLeft
    :remaining_blinks
    :blinkRight
    :engineOn)
   (invariants
    (member? :curTime natural-set)
    (member? :tipStartTime natural-set)
    (<= :tipStartTime :curTime))
   (init
    (assign :active_blinkers #{})
    (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
    (assign :remaining_blinks 0)
    (assign :onCycle false)
    (assign :engineOn false)
    (assign :pitmanArmUpDown :Neutral)
    (assign :hazardWarningSwitchOn :switch_off)
    (assign :keyState :KeyInsertedOnPosition)
    (assign :curTime 0)
    (assign :tipStartTime 0))
   (events
    (event
     :ENV_Turn_EngineOn_Noblink
     (event-refines :ENV_Turn_EngineOn_Noblink)
     (status :ordinary)
     (when
      (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
      (or
       (not (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING))
       (= :hazardWarningSwitchOn :switch_on)))
     (then (assign :engineOn true)))
    (event
     :ENV_Turn_EngineOn_BlinkLeft
     (event-refines :ENV_Turn_EngineOn_BlinkLeft)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
      (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
      (= (fn-call :pitman_direction :pitmanArmUpDown) :left_blink)
      (= :hazardWarningSwitchOn :switch_off)
      (= :rem :continuousBlink))
     (then
      (assign :active_blinkers #{:left_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkRight :lamp_off)
      (assign :engineOn true)))
    (event
     :ENV_Turn_EngineOn_BlinkRight
     (event-refines :ENV_Turn_EngineOn_BlinkRight)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (and (= :engineOn false) (= :keyState :KeyInsertedOnPosition))
      (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
      (= (fn-call :pitman_direction :pitmanArmUpDown) :right_blink)
      (= :hazardWarningSwitchOn :switch_off)
      (= :rem :continuousBlink))
     (then
      (assign :active_blinkers #{:right_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkLeft :lamp_off)
      (assign :engineOn true)))
    (event
     :ENV_TurnEngineOff_Blink
     (event-refines :ENV_TurnEngineOff_Blink)
     (status :ordinary)
     (when (= :engineOn true) (= :hazardWarningSwitchOn :switch_on))
     (then (assign :engineOn false)))
    (event
     :ENV_TurnEngineOff_Noblink
     (event-refines :ENV_TurnEngineOff_Noblink)
     (status :ordinary)
     (when (= :engineOn true) (= :hazardWarningSwitchOn :switch_off))
     (then
      (assign :active_blinkers #{})
      (assign :remaining_blinks 0)
      (assign :blinkLeft :lamp_off)
      (assign :blinkRight :lamp_off)
      (assign :engineOn false)))
    (event
     :ENV_Pitman_Update_Nochange
     (event-refines :ENV_Pitman_Update_Nochange)
     (status :ordinary)
     (any :newPos)
     (when
      (not= :pitmanArmUpDown :newPos)
      (not
       (and (= :hazardWarningSwitchOn :switch_off) (= :engineOn true)))
      (not= :newPos :Neutral))
     (then (assign :pitmanArmUpDown :newPos)))
    (event
     :ENV_Pitman_DirectionBlinkingLeft_Blink
     (event-refines :ENV_Pitman_DirectionBlinkingLeft_Blink)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (not= :pitmanArmUpDown :Downward7)
      (= :hazardWarningSwitchOn :switch_off)
      (= :engineOn true)
      (= :rem :continuousBlink))
     (then
      (assign :active_blinkers #{:left_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkRight :lamp_off)
      (assign :pitmanArmUpDown :Downward7)))
    (event
     :ENV_Pitman_DirectionBlinkingRight_Blink
     (event-refines :ENV_Pitman_DirectionBlinkingRight_Blink)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (not= :pitmanArmUpDown :Upward7)
      (= :hazardWarningSwitchOn :switch_off)
      (= :engineOn true)
      (= :rem :continuousBlink))
     (then
      (assign :active_blinkers #{:right_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkLeft :lamp_off)
      (assign :pitmanArmUpDown :Upward7)))
    (event
     :ENV_Pitman_TipBlinkingLeft_Blink
     (event-refines :ENV_Pitman_TipBlinkingLeft_Blink)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (not= :pitmanArmUpDown :Downward5)
      (= :hazardWarningSwitchOn :switch_off)
      (= :engineOn true)
      (= :rem 3))
     (then
      (assign :active_blinkers #{:left_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkRight :lamp_off)
      (assign :pitmanArmUpDown :Downward5)
      (assign :tipStartTime :curTime)))
    (event
     :ENV_Pitman_TipBlinkingRight_Blink
     (event-refines :ENV_Pitman_TipBlinkingRight_Blink)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (not= :pitmanArmUpDown :Upward5)
      (= :hazardWarningSwitchOn :switch_off)
      (= :engineOn true)
      (= :rem 3))
     (then
      (assign :active_blinkers #{:right_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkLeft :lamp_off)
      (assign :pitmanArmUpDown :Upward5)
      (assign :tipStartTime :curTime)))
    (event
     :ENV_Pitman_Reset_to_Neutral_Nochange
     (event-refines :ENV_Pitman_Reset_to_Neutral_Nochange)
     (status :ordinary)
     (when
      (not= :pitmanArmUpDown :Neutral)
      (not
       (and
        (= :hazardWarningSwitchOn :switch_off)
        (= :remaining_blinks :continuousBlink))))
     (then (assign :pitmanArmUpDown :Neutral)))
    (event
     :ENV_Pitman_Reset_to_Neutral_Noblink
     (event-refines :ENV_Pitman_Reset_to_Neutral_Noblink)
     (status :ordinary)
     (when
      (not= :pitmanArmUpDown :Neutral)
      (= :hazardWarningSwitchOn :switch_off)
      (= :remaining_blinks :continuousBlink))
     (then
      (assign :active_blinkers #{})
      (assign :remaining_blinks 0)
      (assign :blinkLeft :lamp_off)
      (assign :blinkRight :lamp_off)
      (assign :pitmanArmUpDown :Neutral)))
    (event
     :ENV_HazardBlinkingOn
     (event-refines :ENV_HazardBlinkingOn)
     (status :ordinary)
     (when (= :hazardWarningSwitchOn :switch_off))
     (then
      (assign :active_blinkers :BLINK_DIRECTION)
      (assign :remaining_blinks :continuousBlink)
      (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
      (assign :hazardWarningSwitchOn :switch_on)))
    (event
     :ENV_HazardBlinkingOff
     (event-refines :ENV_HazardBlinkingOff)
     (status :ordinary)
     (when
      (= :hazardWarningSwitchOn :switch_on)
      (or
       (= :pitmanArmUpDown :Neutral)
       (or
        (= :engineOn false)
        (not (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)))))
     (then
      (assign :active_blinkers #{})
      (assign :remaining_blinks 0)
      (assign :blinkLeft :lamp_off)
      (assign :blinkRight :lamp_off)
      (assign :hazardWarningSwitchOn :switch_off)))
    (event
     :ENV_HazardBlinkingOff_LeftBlink
     (event-refines :ENV_HazardBlinkingOff_LeftBlink)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (= :hazardWarningSwitchOn :switch_on)
      (not (or (= :pitmanArmUpDown :Neutral) (= :engineOn false)))
      (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
      (= (fn-call :pitman_direction :pitmanArmUpDown) :left_blink)
      (= :rem :remaining_blinks))
     (then
      (assign :active_blinkers #{:left_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkLeft (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkRight :lamp_off)
      (assign :hazardWarningSwitchOn :switch_off)))
    (event
     :ENV_HazardBlinkingOff_RightBlink
     (event-refines :ENV_HazardBlinkingOff_RightBlink)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (= :hazardWarningSwitchOn :switch_on)
      (not (or (= :pitmanArmUpDown :Neutral) (= :engineOn false)))
      (member? :pitmanArmUpDown :PITMAN_DIRECTION_BLINKING)
      (= (fn-call :pitman_direction :pitmanArmUpDown) :right_blink)
      (= :rem :remaining_blinks))
     (then
      (assign :active_blinkers #{:right_blink})
      (assign :remaining_blinks :rem)
      (assign :blinkRight (fn-call :cycleMaxLampStatus :onCycle))
      (assign :blinkLeft :lamp_off)
      (assign :hazardWarningSwitchOn :switch_off)))
    (event
     :TIME_Tip_blinking_Timeout
     (event-refines :TIME_Tip_blinking_Timeout)
     (status :ordinary)
     (any :rem)
     (when
      (member? :rem :BLINK_CYCLE_COUNTER)
      (not= :rem 0)
      (not= :remaining_blinks 0)
      (> :remaining_blinks 1)
      (member? :pitmanArmUpDown :PITMAN_TIP_BLINKING)
      (=
       :active_blinkers
       #{(fn-call :pitman_direction :pitmanArmUpDown)})
      (= :rem :continuousBlink)
      (>= :curTime (+ :tipStartTime 500)))
     (then (assign :remaining_blinks :rem)))
    (event
     :TIME_BlinkerOn
     (event-refines :TIME_BlinkerOn)
     (status :ordinary)
     (any :delta)
     (when
      (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))
      (not= :remaining_blinks 0)
      (member? :delta (interval 1 500))
      (= (mod (+ :curTime :delta) 500) 0))
     (then
      (assign
       :blinkLeft
       (fn-call
        #{(|-> true :lamp_on) (|-> false :lamp_off)}
        (pred->bool (member? :left_blink :active_blinkers))))
      (assign
       :blinkRight
       (fn-call
        #{(|-> true :lamp_on) (|-> false :lamp_off)}
        (pred->bool (member? :right_blink :active_blinkers))))
      (assign
       :remaining_blinks
       (max #{:continuousBlink (- :remaining_blinks 1)}))
      (assign :onCycle true)
      (assign :curTime (+ :curTime :delta))))
    (event
     :TIME_BlinkerOff
     (event-refines :TIME_BlinkerOff)
     (status :ordinary)
     (any :delta)
     (when
      (not (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off)))
      (member? :delta (interval 1 500))
      (= (mod (+ :curTime :delta) 500) 0))
     (then
      (assign :blinkLeft :lamp_off :blinkRight :lamp_off)
      (assign
       :active_blinkers
       (comprehension-set
        [:x]
        (and
         (member? :x :DIRECTIONS)
         (and
          (member? :x :active_blinkers)
          (not= :remaining_blinks 0)))))
      (assign :onCycle false)
      (assign :curTime (+ :curTime :delta))))
    (event
     :TIME_Nothing
     (event-refines :TIME_Nothing)
     (status :ordinary)
     (any :newOnCycle :delta)
     (when
      (not= :newOnCycle :onCycle)
      (and (= :blinkLeft :lamp_off) (= :blinkRight :lamp_off))
      (= :active_blinkers #{})
      (member? :delta (interval 1 500))
      (= (mod (+ :curTime :delta) 500) 0))
     (then
      (assign :onCycle :newOnCycle)
      (assign :curTime (+ :curTime :delta))))
    (event
     :TIME_Passes
     (status :ordinary)
     (any :delta)
     (when
      (member? :delta #{100})
      (for-all
       [:d]
       (and (member? :d integer-set) (member? :d (interval 1 :delta)))
       (not= (mod (+ :curTime :d) 500) 0)))
     (then (assign :curTime (+ :curTime :delta))))))))


(comment
  (clojure.pprint/pprint PitmanController))

