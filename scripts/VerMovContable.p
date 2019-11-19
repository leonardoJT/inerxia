FOR EACH mov_contable WHERE YEAR(fec_contable) = 2011
                        AND cuenta <> ?
                        AND cuenta <> ""
                        AND fec_contable <> 12/31/2011 NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = mov_contable.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN
        MESSAGE mov_contable.cuenta fec_contable
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
