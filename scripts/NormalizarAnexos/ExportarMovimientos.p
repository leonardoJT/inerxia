OUTPUT TO d:\leonardo\16909501.csv.
FOR EACH mov_contable WHERE cuenta = "16909501"
                        /*AND nit = "860003020"*/
                        AND fec_contable >= 03/01/2011 NO-LOCK BY fec_contable:
    EXPORT DELIMITER ";"
        fec_contable
        agencia
        comprobante
        num_documento
        nit
        comentario
        db
        cr.
END.
OUTPUT CLOSE.
