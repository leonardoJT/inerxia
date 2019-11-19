DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE VAR cont AS INTEGER.

FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable >= 04/01/2018
                            AND mov_contable.fec_contable <= 04/30/2018
                            AND mov_contable.cen_costos = 0:     
        mov_contable.cen_costos = 999.
        DISPLAY mov_contable WITH WIDTH 300 1 COL.
        cont = cont + 1.
    END.
END.

MESSAGE "Fin" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* 95 */
