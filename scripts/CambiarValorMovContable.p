DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE /*fec_contable = 05/31/2011
                        AND agencia = 3
                        AND comprobante = 3
                        /*AND num_documento = 257*/
                        AND*/ cuenta = "51100204"
                        AND db = 100:
    UPDATE mov_contable WITH WIDTH 200 1 COL.
END.
