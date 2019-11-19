DISABLE TRIGGERS FOR LOAD OF mov_contable_niif.

FOR EACH agencias /*WHERE agencias.agencia >= 2*/ NO-LOCK BY agencias.agencia:
    FOR EACH mov_contable_niif WHERE agencia = agencias.agencia
                                 AND fec_contable = 06/30/2017:
        DELETE mov_contable_niif.
    END.
END.

MESSAGE "OK - Borrado"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
