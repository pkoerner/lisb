(ns lisb.examples.eventb.traffic-lights
  (:require [lisb.translation.eventb.dsl :refer [eventb]]
            [lisb.translation.eventb.ir2eventb :refer [ir->prob]]
            [lisb.translation.eventb.util :refer [ir->prob-model prob-model->rodin]]))

(def tlc_0 (eventb (context :tlc_0
                            (sets :tl_state)
                            (constants :go :stop)
                            (axioms
                              (partition :tl_state #{:go} #{:stop})))))

(def tlm_0 (eventb (machine :tlm_0
                            (sees :tlc_0)
                            (variables :TL_Car :TL_Ped)
                            (invariants
                             (=> (= :TL_Car :go) (= :TL_Ped :stop))
                             (=> (= :TL_Ped :go) (= :TL_Car :stop)))
                            (init
                             (assign
                              :TL_Car :stop
                              :TL_Ped :stop))
                            (events
                             (event :carGo
                                    (when
                                     (= :TL_Car :stop)
                                      (= :TL_Ped :stop))
                                    (then (assign :TL_Car :go)))
                             (event :pedGo
                                    (when
                                     (= :TL_Car :stop)
                                      (= :TL_Ped :stop))
                                    (then (assign :TL_Ped :go)))
                             (event :carStop
                                    (when (= :TL_Car :go))
                                    (then (assign :TL_Car :stop)))
                             (event :pedStop
                                    (when (= :TL_Ped :go))
                                    (then (assign :TL_Ped :stop)))))))

(def tlc_1 (eventb (context :tlc_1
                     (event-extends :tlc_0)
                     (sets :colors)
                     (constants :green :red :yellow)
                     (axioms
                      (partition :colors #{:green} #{:red} #{:yellow})))))

(def tlm_1 (eventb (refinement :tlm_1 :tlm_0
                               (sees :tlc_1)
                               (variables :TL_Car_Color :TL_Ped_Color)
                               (invariants
                                 (<=> (= :TL_Car :go) (not= :TL_Car_Color #{:red}))
                                 (<=> (= :TL_Ped :go) (not= :TL_Ped_Color #{:red})))
                               (init
                                 (assign
                                   :TL_Car_Color #{:red}
                                   :TL_Ped_Color #{:red}))
                               (events
                                (event :carRedToRedYellow
                                       (event-refines :carGo)
                                       (when (= :TL_Ped_Color #{:red}))
                                       (then (assign :TL_Car_Color #{:red :yellow})))
                                (event :carRedYellowToGreen
                                       (when (= :TL_Car_Color #{:red :yellow}))
                                       (then (assign :TL_Car_Color #{:green})))
                                (event :carGreenToYellow
                                       (when (= :TL_Ped_Color #{:green}))
                                       (then (assign :TL_Car_Color #{:yellow})))
                                (event :carYellowToRed
                                       (event-refines :carStop)
                                       (when (= :TL_Ped_Color #{:yellow}))
                                       (then (assign :TL_Car_Color #{:yellow})))
                                (event :pedRedToGreen
                                       (event-refines :pedGo)
                                       (when (= :TL_Ped_Color #{:red}))
                                       (then (assign :TL_Ped_Color #{:green})))
                                (event :pedGreenToRed
                                       (event-refines :pedStop)
                                       (when (= :TL_Ped_Color #{:green}))
                                       (then (assign :TL_Ped_Color #{:red})))
                                ))))

(def tlm_2 (eventb (refinement :tlm_2 :tlm_1
                               (sees :tlc_1)
                               (variables :CallButtonState :TL_Car_Color :TL_Ped_Color)
                               (invariants (in :CallButtonState bool-set))
                               (init (assign :CallButtonState false))
                               (events
                                 (event :carRedToRedYellow
                                        (event-extends :carRedToRedYellow)
                                        (when (= :CallButtonState false)))
                                 (event :carRedYellowToGreen
                                        (event-extends :carRedYellowToGreen))
                                 (event :carGreenToYellow
                                        (event-extends :carGreenToYellow)
                                        (when (= :CallButtonState true)))
                                 (event :carYellowToRed
                                        (event-extends :carYellowToRed)
                                        (when (= :CallButtonState true)))
                                 (event :pedRedToGreen
                                        (event-extends :pedRedToGreen)
                                        (when (= :CallButtonState true)))
                                 (event :pedGreenToRed
                                        (event-extends :pedGreenToRed)
                                        (when (= :CallButtonState true)))
                                 (event :requestPedLight
                                        (when (not= :TL_Ped :go))
                                        (then (assign :CallButtonState true)))
                                 ))))
(comment
  (clojure.pprint/pprint tlm_1)
  (def model (ir->prob-model tlc_0 tlc_1 tlm_0 tlm_1 tlm_2))
  (prob-model->rodin model "traffic_lights" "./resources/eventb")
  )
