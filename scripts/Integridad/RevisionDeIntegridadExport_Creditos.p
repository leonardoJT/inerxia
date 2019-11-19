DEFINE VAR vAgencia AS INTEGER INITIAL 3.
DEFINE VAR vFecha AS DATE INITIAL 08/31/2018.

OUTPUT TO d:\leonardo\movs.csv.
FOR EACH mov_contable WHERE agencia = vAgencia
                        AND (SUBSTRING(mov_contable.cuenta,1,4) = "1412" OR
                             SUBSTRING(mov_contable.cuenta,1,4) = "1441" OR
                             SUBSTRING(mov_contable.cuenta,1,4) = "1442" OR
                             SUBSTRING(mov_contable.cuenta,1,4) = "1443")
                        AND fec_contable = vFecha
                        AND comprobante <> 20 NO-LOCK BY nit BY db + cr:
    EXPORT DELIMITER ";" "C" nit doc_referencia comentario db + cr.
END.

FOR EACH mov_creditos WHERE agencia = vAgencia
                       AND fecha = vFecha
                       AND cpte <> 20 NO-LOCK BY nit BY val_efectivo + val_cheque:
    EXPORT DELIMITER ";" "A" nit descrip val_efectivo + val_cheque.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
