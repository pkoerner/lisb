MACHINE FunctionReturns
OPERATIONS
  a <-- f1 =
    PRE
      a : NAT
    THEN
      a := 1
    END;
  a, b <-- f2 =
    PRE
      a : NAT & b : INT
    THEN
      a := 1 || b := -1
    END
END