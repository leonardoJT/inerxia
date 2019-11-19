DEFINE VAR tFecha AS DATE.

FOR EACH creditos WHERE creditos.cod_credito <> 123
                    AND creditos.cod_credito <> 108
                    AND creditos.cod_credito <> 113
                    AND creditos.cod_credito <> 114
                    AND creditos.estado = 2 NO-LOCK:
    FIND FIRST extras WHERE extras.nit = creditos.nit
                        AND extras.num_solicitud = creditos.num_solicitud NO-LOCK NO-ERROR.
    IF NOT AVAILABLE extras THEN
        NEXT.

    tFecha = ADD-INTERVAL(creditos.fec_pago,-1,"months").

    FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                            AND amortizacion.num_credito = creditos.num_credito
                            AND amortizacion.fec_pago >= creditos.fec_pago NO-LOCK BY amortizacion.fec_pago:
        FIND FIRST extras WHERE extras.nit = creditos.nit
                            AND extras.num_solicitud = creditos.num_solicitud
                            AND extras.fec_vcto > tFecha
                            AND extras.fec_vcto <= amortizacion.fec_pago
                            AND extras.nro_cuota <> amortizacion.nro_cuota /*NO-LOCK*/ NO-ERROR.
        IF AVAILABLE extras THEN DO:
            /*MESSAGE creditos.nit SKIP
                    creditos.num_credito SKIP
                    amortizacion.nro_cuota amortizacion.fec_pago SKIP
                    extras.nro_cuota extras.fec_vcto
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

            extras.nro_cuota = amortizacion.nro_cuota.
        END.

        tFecha = amortizacion.fec_pago.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
