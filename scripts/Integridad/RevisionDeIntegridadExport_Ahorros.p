DEFINE VAR vAgencia AS INTEGER INITIAL 3.
DEFINE VAR fecIni AS DATE INITIAL 09/01/2018.
DEFINE VAR fecFin AS DATE INITIAL 09/30/2018.

OUTPUT TO d:\leonardo\movs.csv.
FOR EACH mov_contable WHERE agencia = vAgencia
                        AND cuenta = "24451001"
                        AND fec_contable >= fecIni
                        AND fec_contable <= fecFin NO-LOCK BY nit BY db + cr:
    EXPORT DELIMITER ";" "C" nit doc_referencia comentario db + cr.
END.

FOR EACH mov_ahorros WHERE cod_ahorro = 8
                       AND agencia = vAgencia
                       AND fecha >= fecIni
                       AND fecha <= fecFin NO-LOCK BY nit BY val_efectivo + val_cheque:
    EXPORT DELIMITER ";" "A" nit cue_ahorros descrip val_efectivo + val_cheque.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
