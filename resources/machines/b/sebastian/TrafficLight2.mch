MACHINE TrafficLight2
SETS colors = {red, redyellow, yellow, green};
     COMMANDS = {cmd_cars_redyellow, cmd_cars_yellow, cmd_cars_green, cmd_cars_red,
                 cmd_peds_red, cmd_peds_green}
VARIABLES tl_cars, tl_peds
INVARIANT tl_cars : colors & tl_peds : {green, red} &
	  (tl_peds = red or tl_cars = red)
              
INITIALISATION  tl_cars := red || tl_peds := red
OPERATIONS
  Send_command_cars_ry = skip;
  Send_command_cars_y = skip;
  Send_command_cars_g = skip;
  Send_command_cars_r = skip;
  Send_command_peds_r = skip;
  Send_command_peds_g = skip;
  Timeout(cmd) = SELECT cmd : COMMANDS THEN skip END;
  RTIME_Passes(delta) = SELECT delta : NATURAL THEN skip END;

  cars_ry = SELECT tl_cars = red & tl_peds = red THEN tl_cars := redyellow END;
  cars_y = SELECT tl_cars = green THEN tl_cars := yellow END;
  cars_g = SELECT tl_cars = redyellow THEN tl_cars := green END; 
  cars_r = SELECT tl_cars = yellow THEN tl_cars := red END;
  peds_r = SELECT tl_peds = green THEN tl_peds := red END;
  peds_g = SELECT tl_cars = red & tl_peds = red THEN tl_peds := green END
END