DEFINE VAR vAgencia AS INTEGER INITIAL 1.
DEFINE VAR fecIni AS DATE INITIAL 04/01/2019.
DEFINE VAR fecFin AS DATE INITIAL TODAY.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD sdoIni AS DECIMAL
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    FIELD sdoFin AS DECIMAL
    FIELD calculado AS DECIMAL
    FIELD diferencia AS DECIMAL.

FOR EACH rep_creditos WHERE fecCorte = fecIni - 1
                       AND rep_creditos.agencia = vAgencia
                       AND rep_creditos.cod_credito = 123 NO-LOCK:
    FIND FIRST tt WHERE tt.nit = rep_credito.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.nit = rep_creditos.nit.
    END.

    tt.sdoIni = tt.sdoIni + rep_creditos.INT_corriente.
END.


FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                        AND mov_contable.fec_contable >= fecIni
                        AND mov_contable.fec_contable <= fecFin
                        AND (mov_contable.cuenta = "1443050125" OR
                             mov_contable.cuenta = "1443100125" OR
                             mov_contable.cuenta = "1443150125" OR
                             mov_contable.cuenta = "1443200125" OR
                             mov_contable.cuenta = "1443250125")
                        /*AND mov_contable.nit = "17070466"*/ NO-LOCK:
    FIND FIRST tt WHERE tt.nit = mov_contable.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.nit = mov_contable.nit.
    END.

    tt.db = tt.db + mov_contable.db.
    tt.cr = tt.cr + mov_contable.cr.
END.

FOR EACH mov_creditos WHERE mov_creditos.agencia = 1
                       AND mov_creditos.cod_credito = 123
                       AND mov_creditos.fecha >= 04/01/2019
                       AND mov_creditos.fecha <= 04/30/2019
                       /*AND mov_ahorros.nit = "17070466"*/ NO-LOCK:
    EXPORT DELIMITER ";" 'e' nit fecha cpte num_documento descrip val_efectivo sdo_capital.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
