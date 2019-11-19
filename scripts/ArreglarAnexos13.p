DISABLE TRIGGERS FOR LOAD OF anexos13.

FOR EACH mov_contable WHERE mov_contable.comprobante = 20
                        AND mov_contable.fec_contable = 12/31/2012
                        AND mov_contable.agencia = 4
                        AND mov_contable.num_documento = 1513
                        AND mov_contable.usuario = "2305" NO-LOCK:
    FIND FIRST anexos13 WHERE anexos13.agencia = mov_contable.agencia
                          AND anexos13.cuenta = mov_contable.cuenta
                          AND anexos13.ano = 2012
                          AND anexos13.nit = mov_contable.nit NO-ERROR.
    IF NOT AVAILABLE anexos13 THEN
        DISPLAY mov_contable WITH WIDTH 200 1 COL.

    anexos13.db[12] = anexos13.db[12] - mov_contable.db.
    anexos13.cr[12] = anexos13.cr[12] - mov_contable.cr.
END.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
