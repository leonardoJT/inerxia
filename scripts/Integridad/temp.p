DEFINE VAR rep1 AS DECIMAL.
DEFINE VAR rep2 AS DECIMAL.
DEFINE VAR sumAh AS DECIMAL.
DEFINE VAR sumCont AS DECIMAL.

FOR EACH rep_ahorros WHERE fecCorte = 04/30/2018
                       AND agencia = 1
                       AND cod_ahorro = 4 NO-LOCK:
    rep2 = rep2 + sdo_disponible.
END.

FOR EACH rep_ahorros WHERE fecCorte = 03/31/2018
                       AND agencia = 1
                       AND cod_ahorro = 4 NO-LOCK:
    rep1 = rep1 + sdo_disponible.
END.

sumAh = rep2 - rep1.

FOR EACH mov_contable WHERE agencia = 1
                        AND MONTH(fec_contable) = 4
                        AND YEAR(fec_contable) = 2018
                        AND cuenta = "21050501" NO-LOCK:
    sumCont = sumCont + (cr - db).
END.

MESSAGE sumAh - sumCont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
