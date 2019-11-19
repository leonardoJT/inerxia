DISABLE TRIGGERS FOR LOAD OF mov_creditos.

FOR EACH creditos WHERE creditos.estado = 3 NO-LOCK:
    FOR EACH mov_creditos WHERE mov_creditos.agencia = creditos.agencia
                            AND mov_creditos.cod_credito = creditos.cod_credito
                            AND mov_creditos.num_credito = creditos.num_credito
                            AND mov_creditos.fecha <= 01/01/2011
                            AND mov_creditos.nit = creditos.nit:
        DELETE mov_creditos.
    END.
END.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
