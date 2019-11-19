FOR EACH creditos WHERE creditos.cod_credito = 123
                    AND creditos.estado = 2
                    AND creditos.fec_pago = 04/05/2012:
    FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                             AND facturacion.num_credito = creditos.num_credito
                             AND facturacion.fec_pago = creditos.fec_pago NO-ERROR.
    IF AVAILABLE facturacion THEN
        facturacion.fec_pago = 04/09/2012.

    creditos.fec_pago = 04/09/2012.
END.
