FOR EACH creditos WHERE creditos.cod_credito = 191
                    AND creditos.fec_desembolso >= 01/29/2018
                    AND creditos.estado = 2
                    AND creditos.tasa < 9.794 BY creditos.cod_credito
                                              BY creditos.fec_desembolso DESC:
    FIND FIRST solicitud WHERE solicitud.agencia = creditos.agencia
                           AND solicitud.nit = creditos.nit
                           AND solicitud.num_solicitud = creditos.num_solicitud
                           AND solicitud.fec_solicitud >= 01/01/2018
                           AND solicitud.tasa = 10.0338 NO-ERROR.
    IF AVAILABLE solicitud THEN DO:
        UPDATE creditos.agencia
               creditos.nit
               creditos.cod_credito 
               creditos.per_pago
               creditos.num_credito
               solicitud.fec_solicitud
               creditos.fec_desembolso
               solicitud.tasa
               creditos.tasa
            WITH WIDTH 300 1 COL.

        LEAVE.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


/*FOR EACH pro_creditos WHERE cod_credito > 189 NO-LOCK BY cod_credito:
    DISPLAY pro_creditos WITH 1 COL.
END.*/

/*FOR EACH solicitud WHERE cod_credito = 190 NO-LOCK BY fec_solicitud DESC:
    DISPLAY solicitud WITH 1 COL.
END.*/
