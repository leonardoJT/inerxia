DISABLE TRIGGERS FOR LOAD OF mov_contable_NIIF.

DEFINE VAR cont AS INTEGER.

FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable_niif WHERE mov_contable_niif.agencia = agencias.agencia
                                 AND mov_contable_NIIF.fec_contable >= 01/01/2015
                                 AND mov_contable_NIIF.fec_contable <= 11/30/2017:
        DELETE mov_contable_NIIF.
        cont = cont + 1.
    END.
END.

MESSAGE "Fin" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
