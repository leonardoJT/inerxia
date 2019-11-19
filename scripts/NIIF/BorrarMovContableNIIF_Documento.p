DISABLE TRIGGERS FOR LOAD OF mov_contable_niif.

FOR EACH mov_contable_niif WHERE mov_contable_niif.agencia = 1
                             AND mov_contable_niif.fec_contable = 05/05/2017
                             AND mov_contable_NIIF.comprobante = 20
                             AND mov_contable_NIIF.num_documento = 4922:
    DELETE mov_contable_niif.
END.

MESSAGE "Fin borrado"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
