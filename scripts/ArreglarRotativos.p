FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND sdo_capital > 0
                    AND dias_atraso > 0:
    FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                             AND facturacion.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE facturacion THEN DO:
        DISPLAY creditos.agencia creditos.nit creditos.num_credito creditos.dias_atraso.
        creditos.fec_pago = 03/05/2012.
        creditos.INT_morCobrar = 0.
        creditos.dias_atraso = 0.
        creditos.val_atraso = 0.
    END.
END.
