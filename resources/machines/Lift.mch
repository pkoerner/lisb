MACHINE Lift
VARIABLES
  etage
INVARIANT
  etage : 0..99
INITIALISATION
  BEGIN
    etage := 4
  END
OPERATIONS
  inc = 
    PRE 
      etage<99
    THEN 
      etage := etage + 1
    END ;
  dec = 
    PRE 
      etage>0
    THEN 
      etage := etage - 1
    END
END
