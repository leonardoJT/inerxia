OUTPUT TO c:\INFO_fodun\Leonardo\DocumentoAjuste.txt.

DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE mov_contable.agencia = 1
                        AND mov_contable.comprobante = 20
                        AND mov_contable.fec_contable = 05/31/2013
                        AND mov_contable.num_documento = 1807 NO-LOCK:
    EXPORT mov_contable.
END.

FOR EACH mov_contable WHERE mov_contable.agencia = 2
                        AND mov_contable.comprobante = 20
                        AND mov_contable.fec_contable = 05/31/2013
                        AND mov_contable.num_documento = 1804 NO-LOCK:
    EXPORT mov_contable.
END.

FOR EACH mov_contable WHERE mov_contable.agencia = 3
                        AND mov_contable.comprobante = 20
                        AND mov_contable.fec_contable = 05/31/2013
                        AND mov_contable.num_documento = 1802 NO-LOCK:
    EXPORT mov_contable.
END.
