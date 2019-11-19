OUTPUT TO d:\Leonardo\Tarjetas.csv.
FOR EACH creditos WHERE cod_credito = 123 AND estado = 2 NO-LOCK:
    IF creditos.sdo_capital = 0 THEN DO:
        FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
        IF clientes.estado <> 1 THEN
            NEXT.
        /*ELSE
            MESSAGE creditos.nit
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.

    EXPORT DELIMITER ";" creditos.agencia creditos.nit 28200.
END.
