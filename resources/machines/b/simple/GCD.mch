MACHINE GCD
VARIABLES x,y
INVARIANT
 x:NAT & y:NAT
INITIALISATION x:=70 || y:=40
OPERATIONS
  s <-- GCDSolution =
        IF y=0 THEN s:= x ELSE s:= -1 END;
  Step = IF y>0 THEN x := y || y := x mod y END;
  Restart(w1,w2) = PRE w1:NAT1 & w2:NAT1 THEN
                      IF w1>w2 THEN x,y := w1,w2 ELSE y,x := w1,w2 END
                   END
END

