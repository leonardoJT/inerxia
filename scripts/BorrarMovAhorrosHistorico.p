DISABLE TRIGGERS FOR LOAD OF mov_ahorros.

FOR EACH ahorros WHERE estado = 2 NO-LOCK:
    FOR EACH mov_ahorros WHERE mov_ahorros.agencia = ahorros.agencia
                           AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                           AND mov_ahorros.cue_ahorro = ahorros.cue_ahorro
                           AND mov_ahorros.fecha <= 01/01/2011
                           AND mov_ahorros.nit = ahorros.nit:
        DELETE mov_ahorros.
    END.
END.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
