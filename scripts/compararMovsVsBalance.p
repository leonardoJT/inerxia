DEFINE VAR mdb AS DECIMAL.
DEFINE VAR mcr AS DECIMAL.

DEFINE VAR bdb AS DECIMAL.
DEFINE VAR bcr AS DECIMAL.

OUTPUT TO d:\leonardo\errorBalance.csv.
FOR EACH mov_contable WHERE mov_contable.agencia = 1
                        AND mov_contable.fec_contable >= 01/01/2020
                        AND mov_contable.fec_contable <= 01/31/2020 NO-LOCK BREAK BY mov_contable.cuenta:
    IF FIRST-OF(mov_contable.cuenta) THEN DO:
        mdb = 0.
        mcr = 0.
        bdb = 0.
        bcr = 0.
    END.
    
    mdb = mdb + db.
    mcr = mcr + cr.

    IF LAST-OF(mov_contable.cuenta) THEN DO:
        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = mov_contable.agencia
                              AND sal_cuenta.ano = 2020
                              AND sal_cuenta.cuenta = mov_contable.cuenta NO-LOCK:
            bdb = bdb + sal_cuenta.db[1].
            bcr = bcr + sal_cuenta.cr[1].
        END.

        EXPORT DELIMITER ";" mov_contable.cuenta bdb bcr.

        IF mdb <> bdb OR mcr <> bcr THEN
            MESSAGE mov_contable.cuenta skip
                    mdb bdb mdb - bdb skip
                    mcr bcr mcr - bcr
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
