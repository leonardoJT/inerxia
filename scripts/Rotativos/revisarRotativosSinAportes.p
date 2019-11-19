FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.estado = 2 NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = creditos.nit
                         AND ahorros.tip_ahorro = 4
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ahorros THEN
        MESSAGE creditos.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
