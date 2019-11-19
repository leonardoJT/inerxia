DEFINE VAR sumaDb AS DECIMAL.
DEFINE VAR sumaCR AS DECIMAL.

FOR EACH mov_contable WHERE agencia = 3
                        AND cuenta = "21050501"
                        AND fec_contable >= 11/01/2018 AND fec_contable <= 11/30/2018 NO-LOCK:
    sumaDB = sumaDB + Mov_contable.db.
    sumaCR = sumaCR + Mov_contable.cr.
END.

MESSAGE sumaDB sumaCR
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
