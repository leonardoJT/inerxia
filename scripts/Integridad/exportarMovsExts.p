OUTPUT TO d:\leonardo\movs.csv.
FOR EACH mov_contable WHERE mov_contable.agencia = 2
                        AND mov_contable.fec_contable >= 04/01/2019
                        AND mov_contable.fec_contable <= 04/28/2019
                        AND cuenta = "21050501"
                        AND mov_contable.nit = "3454766" NO-LOCK:
    EXPORT DELIMITER ";" 'm' nit fec_contable comprobante num_documento comentario db cr.
END.

FOR EACH mov_ahorros WHERE mov_ahorros.agencia = 2
                       AND mov_ahorro.cod_ahorro = 4
                       AND mov_ahorros.fecha >= 04/01/2019
                       AND mov_ahorros.fecha <= 04/28/2019
                       AND mov_ahorros.nit = "3454766" NO-LOCK:
    EXPORT DELIMITER ";" 'e' nit fecha cpte num_documento descrip val_efectivo sdo_disponible.
END.
OUTPUT CLOSE.

