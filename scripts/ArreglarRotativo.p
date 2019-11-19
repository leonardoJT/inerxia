FOR EACH creditos WHERE nit = "14440051"
                    AND cod_credito = 123
                    AND estado = 2:
    UPDATE creditos WITH 1 COL.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           /*AND facturacion.estado = 1*/
                           AND facturacion.fec_pago >= 02/01/2015 BY fec_pago:
        UPDATE facturacion WITH 1 COL.
    END.
END.
