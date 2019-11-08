  DEFINE VARIABLE xcta LIKE cuentas.cuenta.
  PROMPT-FOR xcta LABEL "New credit limit" WITH SIDE-LABELS
    NO-BOX ROW 10 FRAME b.
  INPUT xcta.
  ASSIGN xcta.
  FIND cuentas WHERE cuentas.cuenta = xcta NO-ERROR.
  IF AVAILABLE(cuentas) THEN DO:
     UPDATE cta_homologada cuenta nombre.
  END.
  
