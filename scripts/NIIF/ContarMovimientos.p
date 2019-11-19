DEFINE VAR sumaDb AS DECIMAL.
DEFINE VAR sumaCr AS DECIMAL.
DEFINE VAR sumaDbNiif AS DECIMAL.
DEFINE VAR sumaCrNiif AS DECIMAL.

DEFINE VAR fecTemp AS DATE INITIAL 02/28/2017.
DEFINE VAR vAgencia AS INTEGER INITIAL 4.

FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                        AND mov_contable.fec_contable = fecTemp NO-LOCK:
    sumaDb = sumaDb + mov_contable.db.
    sumaCr = sumaCr + mov_contable.cr.
END.

FOR EACH mov_contable_NIIF WHERE mov_contable_niif.agencia = vAgencia
                             AND mov_contable_NIIF.fec_contable = fecTemp NO-LOCK:
    sumaDbNiif = sumaDbNiif + mov_contable_NIIF.db.
    sumaCrNiif = sumaCrNiif + mov_contable_NIIF.cr.
END.

MESSAGE sumaDb - sumaDbNIIF SKIP
        sumaCr - sumaCrNIIF
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
