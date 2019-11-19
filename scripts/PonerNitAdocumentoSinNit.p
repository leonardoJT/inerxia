DISABLE TRIGGERS FOR LOAD OF mov_contable.
    
FOR EACH mov_contable WHERE mov_contable.cuenta = "16259501"
                        AND mov_contable.nit = "75069082-4"
                        AND mov_contable.fec_contable = 11/13/2012
                        AND mov_contable.agencia = 3:
    UPDATE mov_contable WITH WIDTH 200 1 COL.

    FIND FIRST anexos WHERE anexos.nit = mov_contable.nit
                        AND anexos.cuenta = mov_contable.cuenta
                        AND anexos.agencia = mov_contable.agencia
                        AND anexos.ano = YEAR(mov_contable.fec_contable) NO-ERROR.
    IF AVAILABLE anexos THEN
        UPDATE anexos WITH 1 COL.

    FIND FIRST anexos13 WHERE anexos13.nit = mov_contable.nit
                          AND anexos13.cuenta = mov_contable.cuenta
                          AND anexos13.agencia = mov_contable.agencia
                          AND anexos13.ano = YEAR(mov_contable.fec_contable) NO-ERROR.
    IF AVAILABLE anexos13 THEN
        UPDATE anexos13 WITH 1 COL.
END.
