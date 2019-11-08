DEFINE VAR mdb AS DECIMAL.
DEFINE VAR mcr AS DECIMAL.

DEFINE VAR bdb AS DECIMAL.
DEFINE VAR bcr AS DECIMAL.

FOR EACH mov_contable WHERE mov_contable.agencia = 4
                        AND mov_contable.fec_contable >= 02/01/2017
                        AND mov_contable.fec_contable <= 02/28/2017 NO-LOCK BREAK BY mov_contable.cuenta:
    IF FIRST-OF(mov_contable.cuenta) THEN DO:
        mdb = 0.
        mcr = 0.
        bdb = 0.
        bcr = 0.
    END.
    
    mdb = mdb + db.
    mcr = mcr + cr.

    IF LAST-OF(mov_cotble.cuenta) THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = mov_contable.agencia
                              AND sal_cuenta.ano = 2018
                              AND sal_cuenta.cuenta = mov_contable.cuenta NO-LOCK:
            bdb = bdb + sal_cuenta.db[2].
            bcr = bcr + sal_cuenta.cr[2].
        END.

        IF mdb <> bdm OR mcr <> bcr THEN
            MESSAGE mov_contable.cuenta skip
                    mdb bdm mdb - bdm skip
                    mcr bcr mcr - bcr
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
