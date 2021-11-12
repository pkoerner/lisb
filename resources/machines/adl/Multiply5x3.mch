MACHINE Multiply
  VARIABLES pc, x, y, p
  INVARIANT
    pc : NAT &
    x : NAT &
    y : NAT &
    p : NAT &
    (pc=0 => p+x*y=5*3) &
    (pc=3 => p=5*3)
  INITIALISATION x,y,p,pc := 5,3,0,0
  OPERATIONS
    while0_enter = PRE pc=0 & x>0 THEN pc := 1 END ;
    if0_then = PRE pc=1 & 0/=x mod 2 THEN p := p+y ; pc := 2 END ;
    assign1 = PRE pc=2 THEN x,y := x/2,y*2 ; pc := 0 END ;
    if0_else = PRE pc=1 & not(0/=x mod 2) THEN x,y := x/2,y*2 ; pc := 0 END ;
    while0_exit = PRE pc=0 & not(x>0) THEN pc := 3 END ;
    algorithm_end = PRE pc=3 THEN skip END
END