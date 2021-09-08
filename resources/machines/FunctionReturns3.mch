MACHINE FunctionReturns
OPERATIONS
  a <-- f1 =
    ASSERT
      a : NAT
    THEN
      a := 1
    END;
  a, b <-- f2 =
    ASSERT
      a : NAT & b : INT
    THEN
      a := 1 || b := -1
    END
END