/* FIND FIRST Act_Fijo WHERE Act_Fijo.Agencia EQ 1 NO-LOCK. */
DEFINE VAR wsaldo AS DECIMAL NO-UNDO INIT 0.
FOR EACH cuentas  WHERE Cuentas.Id_NoMvto EQ TRUE  NO-LOCK BY Cuentas.Cuenta :
    FIND FIRST Sal_Cuenta  NO-LOCK NO-ERROR .
    IF AVAILABLE Sal_Cuenta 
        THEN wsaldo = Sal_Cuenta.sal_inicial. 
        ELSE wsaldo = 0.  
        DISPLAY Cuentas.Cuenta
                Cuentas.Id_NoMvto
                wsaldo WITH WIDTH 250.
END.
/* MESSAGE Act_Fijo.Agencia */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
