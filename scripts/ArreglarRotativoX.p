FOR EACH creditos WHERE creditos.nit = "6402643"
                    AND cod_credito = 123
                    AND estado = 2:
    UPDATE creditos WITH 1 COL.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.fec_pago >= 04/10/2016 BY fec_pago:
        UPDATE facturacion WITH 1 COL.
        /*DELETE facturacion.*/
    END.
END.
