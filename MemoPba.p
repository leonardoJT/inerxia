 DEFI VAR P_SdoCre LIKE Anexos.Sdo_Inicial INIT 0.
 DEFI VAR P_SdoDeb LIKE Anexos.Sdo_Inicial INIT 0.
 DEFI VAR P_Sdoact LIKE Anexos.Sdo_Inicial INIT 0.

 DEFINE VAR i AS INTEGER.
  FOR EACH Anexos WHERE 
           Anexos.Nit         = "71171298" AND
           Anexos.Agencia    GE 1 AND
           Anexos.Agencia    LE 1 AND
           Anexos.Cen_Costos GE 0 AND
           Anexos.Cen_Costos LE 999 AND
           Anexos.Cuenta      = "24652001" AND
           Anexos.Ano         = 2004 NO-LOCK:
    ASSIGN P_SdoAct   = P_SdoAct + Anexos.Sdo_Inicial.
    DO i = 1 TO 5 BY 1:
      ASSIGN P_SdoCre   = P_SdoCre + Anexos.Cr[i]
             P_SdoDeb   = P_SdoDeb + Anexos.Db[i].
    END.
  END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ "24652001" NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN DO:
     IF Cuentas.Naturaleza EQ "DB" THEN
        P_SdoAct = P_SdoAct + P_SdoDeb - P_SdoCre.
     ELSE
        P_SdoAct = P_SdoAct + P_SdoCre - P_SdoDeb.
  END.

MESSAGE P_SdoAct
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
