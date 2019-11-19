DEFINE VAR dbs AS DECIMAL.
DEFINE VAR crs AS DECIMAL.

FOR EACH mov_contable WHERE cuenta = "16601501" AND MONTH(fec_contable) = 8 AND YEAR(fec_contable) = 2012 AND agencia = 1 NO-LOCK:
    dbs = dbs + mov_contable.db.
    crs = crs + mov_contable.cr.
END.

MESSAGE dbs crs
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

dbs = 0.
crs = 0.

FOR EACH anexos WHERE anexos.cuenta = "16601501" AND ano = 2012 AND agencia = 1 NO-LOCK:
    dbs = dbs + anexos.db[8].
    crs = crs + anexos.cr[8].
END.

MESSAGE dbs crs
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
