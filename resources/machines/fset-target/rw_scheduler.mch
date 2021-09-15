MACHINE scheduler

/* a1=TRUE => PID1 : active etc. */
VARIABLES a1,a2,r1,r2,w1,w2

INVARIANT  a1 : BOOL & a2 : BOOL & r1 : BOOL & r2 : BOOL & w1 : BOOL & w2 : BOOL &
           /* (ready /\ waiting) = {} */
           not ((r1=TRUE & w1=TRUE) or (r2=TRUE & w2=TRUE)) &
           /* active /\ (ready \/ waiting) = {} */
           not ((a1=TRUE & r1=TRUE) or (a2=TRUE & r2=TRUE)) &
           not ((a1=TRUE & w1=TRUE) or (a2=TRUE & w2=TRUE)) &
           /* card(active) <= 1*/
           not (a1=TRUE & a2=TRUE) &
           /* ((active = {})  => (ready = {})) */
           ((a1=FALSE & a2=FALSE) => (r1=FALSE & r2=FALSE))

INITIALISATION
    a1:=FALSE ||
    a2:=FALSE ||
    r1:=FALSE ||
    r2:=FALSE ||
    w1:=FALSE ||
    w2:=FALSE

OPERATIONS

new_PID1 =
    SELECT
        a1=FALSE &
        r1=FALSE &
        w1=FALSE
    THEN
        w1:=TRUE
    END;

new_PID2 =
    PRE
        a2=FALSE &
        r2=FALSE &
        w2=FALSE
    THEN
        w2:=TRUE
    END;

del_PID1 =
    PRE
        w1=TRUE
    THEN
        w1:=FALSE
    END;


del_PID2 =
    PRE
        w2=TRUE
    THEN
        w2:=FALSE
    END;

ready_PID1 =
    PRE
        w1=TRUE
    THEN
        w1:=FALSE ||
        IF (a1=FALSE & a2=FALSE) THEN
            a1:=TRUE
        ELSE
            r1:=TRUE
        END
    END;

ready_PID2 =
    PRE
        w2=TRUE
    THEN
        w2:=FALSE ||
        IF (a1=FALSE & a2=FALSE) THEN
            a2:=TRUE
        ELSE
            r2:=TRUE
        END
    END;

swap_PID1 =
    PRE
        a1=TRUE
    THEN
        a1:=FALSE ||
        w1:=TRUE ||
        IF r2=TRUE THEN
            r2:=FALSE ||
            a2:=TRUE
        END
    END;


swap_PID2 =
    PRE
        a2=TRUE
    THEN
        a2:=FALSE ||
        w2:=TRUE ||
        IF r1=TRUE THEN
            r1:=FALSE ||
            a1:=TRUE
        END
    END
END
