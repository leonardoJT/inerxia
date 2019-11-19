/* FIND FIRST Act_Fijo WHERE Act_Fijo.Agencia EQ 1 NO-LOCK. */
FOR EACH cuentas  WHERE Cuentas.Id_NoMvto EQ TRUE  NO-LOCK BY Cuentas.Cuenta :
    FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Cuenta = Cuentas.Cuenta NO-LOCK NO-ERROR .
    DISPLAY Cuentas.Cuenta
/*             Cuentas.Ctr_Naturaleza VIEW-AS TEXT */
/*             Cuentas.Estado         VIEW-AS TEXT */
            Cuentas.Id_NoMvto.
            IF AVAILABLE Sal_Cuenta THEN DISPLAY Sal_Cuenta.sal_inicial WITH WIDTH 250.
END.
/* MESSAGE Act_Fijo.Agencia */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
