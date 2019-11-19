DEFINE VAR varsuma AS DECIMAL.

FOR EACH creditos WHERE creditos.agencia = 1
                    AND creditos.estado = 1 NO-LOCK:
    FIND FIRST mov_instancias WHERE mov_instancias.nit = creditos.nit
                                AND mov_instancias.num_solicitud = creditos.num_solicitud
                                AND mov_instancias.instancia = 950
                                AND mov_instancias.estado = NO NO-LOCK NO-ERROR.
    IF AVAILABLE mov_instancias THEN
        varsuma = varsuma + creditos.monto.
END.

MESSAGE varsuma
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
