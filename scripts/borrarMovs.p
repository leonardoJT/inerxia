FOR EACH mov_contable_NIIF WHERE agencia = 4
                        AND fec_contable = 04/30/2017
                        AND comprobante = 20
                        AND num_documento >= 4904:
    DELETE mov_contable_NIIF.
END.
