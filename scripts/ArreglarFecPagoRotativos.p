FOR EACH creditos WHERE creditos.cod_credito = 123 AND creditos.estado = 2:
    FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                             AND facturacion.num_credito = creditos.num_credito
                             AND facturacion.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE facturacion THEN DO:
        IF creditos.fec_pago = 03/05/2012 THEN
            /*creditos.fec_pago = 04/05/2012.*/
            DISPLAY creditos WITH 1 COL.
    END.
    
END.
