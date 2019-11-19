DEFINE VAR corriente AS DECIMAL.

DEFINE TEMP-TABLE pagos
    FIELD cedula AS CHARACTER
    FIELD db AS DECIMAL.

INPUT FROM c:\INFO_Fodun\Leonardo\Rotativo05092012.csv.
REPEAT :
    CREATE pagos.
    IMPORT DELIMITER ";" pagos.

    FOR EACH creditos WHERE creditos.cod_credito = 123
                        AND creditos.nit = pagos.cedula
                        AND creditos.estado = 2 NO-LOCK:
        corriente = 0.

        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito
                               AND facturacion.estado = 1 NO-LOCK BY facturacion.fec_pago:
            corriente = corriente + (facturacion.int_corriente - facturacion.pago_intCorriente).
        END.

        IF creditos.int_corriente - pagos.db < 0 THEN DO:
            MESSAGE creditos.int_corriente - pagos.db creditos.INT_corriente pagos.db corriente
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            DISPLAY creditos WITH 1 COL.

            FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                                   AND facturacion.num_credito = creditos.num_credito
                                   AND facturacion.estado = 1 NO-LOCK BY facturacion.fec_pago:
                DISPLAY facturacion WITH 1 COL.
            END.

        END.
    END.
END.
