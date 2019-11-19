FOR EACH p.creditos WHERE p.creditos.agencia = 2
                      AND p.creditos.cod_credito = 27
                      AND p.creditos.estado = 2 NO-LOCK:
    FIND FIRST d.creditos WHERE d.creditos.nit = p.creditos.nit
                            AND d.creditos.num_credito = p.creditos.num_credito NO-LOCK NO-ERROR.
    IF AVAILABLE d.creditos THEN DO:
        IF p.creditos.sdo_capital <> d.creditos.sdo_capital THEN DO:
            MESSAGE p.creditos.nit p.creditos.num_credito p.creditos.pagare p.creditos.sdo_capital d.creditos.sdo_capital p.creditos.sdo_capital - d.creditos.sdo_capital
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
END.
