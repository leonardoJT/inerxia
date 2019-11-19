DEFINE VAR sumDb AS DECIMAL.
DEFINE VAR sumCr AS DECIMAL.
DEFINE VAR sumDbNIIF AS DECIMAL.
DEFINE VAR sumCrNIIF AS DECIMAL.
DEFINE VAR vAgencia AS INTEGER INITIAL 1.


FOR EACH cuentas WHERE cuentaNIIF <> "" AND SUBSTRING(cuenta,1,4) = "2445" AND tipo = 2 NO-LOCK:
    sumDb = 0.
    sumCr = 0.
    sumDbNIIF = 0.
    sumCrNIIF = 0.

    FOR EACH mov_contable WHERE mov_contable.fec_contable >= 02/01/2017
                            AND mov_contable.fec_contable <= 02/28/2017
                            AND mov_contable.cuenta = cuentas.cuenta
                            AND mov_contable.agencia = vAgencia NO-LOCK:
        sumDb = sumDb + mov_contable.db.
        sumCr = sumCr + mov_contable.cr.
    END.

    FOR EACH mov_contable_NIIF WHERE mov_contable_NIIF.fec_contable >= 02/01/2017
                                 AND mov_contable_NIIF.fec_contable <= 02/28/2017
                                 AND mov_contable_NIIF.cuenta = cuentas.cuentaNIIF
                                 AND mov_contable_NIIF.agencia = vAgencia NO-LOCK:
        sumDbNIIF = sumDbNIIF + mov_contable_NIIF.db.
        sumCrNIIF = sumCrNIIF + mov_contable_NIIF.cr.
    END.

    MESSAGE cuentas.cuenta cuentas.cuentaNIIF SKIP
            sumDB sumDbNIIF sumDB - sumDbNIIF SKIP
            sumCr sumCrNIIF sumCr - sumCrNIIF
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
