DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR fecIni AS DATE.
DEFINE VAR fecFin AS DATE.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD sdoIni AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD movs AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD sdoFin AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD sdoFinCalc AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD dif AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    INDEX idx1 nit.

vAgencia = 2.
fecIni = 08/01/2018.
fecFin = 08/31/2018.

FOR EACH rep_ahorros WHERE rep_ahorros.fecCorte = fecIni - 1
                       AND rep_ahorros.agencia = vAgencia
                       AND rep_ahorros.cod_ahorro = 2 NO-LOCK:
    FIND FIRST tt WHERE tt.nit = rep_ahorros.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.nit = rep_ahorros.nit.
    END.

    tt.sdoIni = tt.sdoIni + rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje.
END.

FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                        AND mov_contable.fec_contable >= fecIni
                        AND mov_contable.fec_contable <= fecFin
                        AND cuenta = "31050501" NO-LOCK:
    FIND FIRST tt WHERE tt.nit = mov_contable.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.nit = mov_contable.nit.
    END.

    tt.movs = tt.movs + mov_contable.cr - mov_contable.db.
END.

FOR EACH ahorros WHERE ahorros.agencia = vAgencia
                   AND ahorros.cod_ahorro = 2 NO-LOCK:
    FIND FIRST tt WHERE tt.nit = ahorros.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.nit = ahorros.nit.
    END.

    tt.sdoFin = tt.sdoFin + ahorros.sdo_disponible + ahorros.sdo_canje.
END.

FOR EACH tt:
    tt.sdoFinCalc = tt.sdoIni + tt.movs.
    tt.dif = tt.sdoFin - tt.sdoFinCalc.

    IF tt.dif <> 0 THEN
        DISPLAY tt WITH 1 COL.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
