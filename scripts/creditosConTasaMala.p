FOR EACH solicitud WHERE cod_credito = 17 AND fec_solicitud >= 06/01/2018 NO-LOCK BY fec_solicitud:
    FIND FIRST creditos WHERE creditos.cod_credito = 17 AND creditos.num_solicitud = solicitud.num_solicitud AND creditos.tasa <> 12.6 AND creditos.estado = 2 NO-ERROR.
    IF AVAILABLE creditos  THEN DO:
        UPDATE creditos.nit creditos.num_solicitud creditos.tasa WITH WIDTH 300.
        FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit AND amortizacion.num_credito = creditos.num_credito:
            DELETE amortizacion.
        END.
    END.
END.
