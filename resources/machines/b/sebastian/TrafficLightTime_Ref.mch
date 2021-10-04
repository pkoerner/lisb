REFINEMENT TrafficLightTime_Ref REFINES TrafficLight2
INCLUDES GenericTimersMC(COMMANDS)
VARIABLES tl_cars, tl_peds
INITIALISATION  tl_cars := red || tl_peds := red
OPERATIONS
  Send_command_cars_ry = SELECT tl_cars = red & tl_peds = red THEN AddDeadline(cmd_cars_redyellow,500) END;
  Send_command_cars_y = SELECT tl_cars = green THEN AddDeadline(cmd_cars_yellow,500) END;
  Send_command_cars_g = SELECT tl_cars = redyellow THEN AddDeadline(cmd_cars_green,500) END; 
  Send_command_cars_r = SELECT tl_cars = yellow THEN AddDeadline(cmd_cars_red,500) END;
  Send_command_peds_r = SELECT tl_peds = green THEN AddDeadline(cmd_peds_red,500) END;
  Send_command_peds_g = SELECT tl_cars = red & tl_peds = red THEN AddDeadline(cmd_peds_green,500) END;

  Timeout(cmd) = 
   SELECT 
     cmd : dom(curDeadlines) &
     curDeadlines(cmd) = 0
   THEN
     TimeoutDeadline(cmd, 0)
   END;

   cars_ry = 
     SELECT 
       tl_cars = red & tl_peds = red
     THEN 
       tl_cars := redyellow ||
       ANY delta WHERE delta ∈ 0‥500 THEN 
         IncreaseTimeUntilDeadline(cmd_cars_redyellow,delta)
       END
     END;

   cars_y = 
     SELECT 
       tl_cars = green 
     THEN 
      tl_cars := yellow ||
       ANY delta WHERE delta ∈ 0‥500 THEN 
         IncreaseTimeUntilDeadline(cmd_cars_yellow,delta)
       END
     END;

   cars_g = 
     SELECT 
       tl_cars = redyellow
     THEN 
       tl_cars := green ||
       ANY delta WHERE delta ∈ 0‥500 THEN 
         IncreaseTimeUntilDeadline(cmd_cars_green,delta)
       END
     END; 

   cars_r = 
     SELECT 
       tl_cars = yellow
     THEN 
       tl_cars := red ||
       ANY delta WHERE delta ∈ 0‥500 THEN 
         IncreaseTimeUntilDeadline(cmd_cars_red,delta)
       END
     END;

   peds_r = 
     SELECT 
       tl_peds = green
     THEN 
       tl_peds := red ||
       ANY delta WHERE delta ∈ 0‥500 THEN 
         IncreaseTimeUntilDeadline(cmd_peds_red,delta)
       END
     END;

   peds_g = 
      SELECT 
        tl_cars = red & tl_peds = red
      THEN 
        tl_peds := green ||
       ANY delta WHERE delta ∈ 0‥500 THEN 
         IncreaseTimeUntilDeadline(cmd_peds_green,delta)
       END
      END;

   RTIME_Passes(delta) = SELECT delta : {100}
    THEN
     IncreaseTime(delta)
    END

END