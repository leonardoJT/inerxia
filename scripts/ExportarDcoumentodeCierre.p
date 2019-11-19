OUTPUT TO c:\INFO_fodun\ComprobanteCierreCarteraDiciembre.txt.

DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE mov_contable.comprobante = 20
                        AND mov_contable.fec_contable = 12/31/2011
                        /*AND (mov_contable.num_documento = 319 OR
                             mov_contable.num_documento = 320 OR
                             mov_contable.num_documento = 321 OR
                             mov_contable.num_documento = 316 OR
                             mov_contable.num_documento = 317 OR
                             mov_contable.num_documento = 318 OR
                             mov_contable.num_documento = 315)*/
                        AND mov_contable.fec_grabacion = TODAY
                        AND mov_contable.usuario = "2305"
                        /*AND (mov_contable.comentario = "ProvGeneral-Nómina" OR
                             mov_contable.comentario = "ProvGeneral-Caja")*/ NO-LOCK:
    EXPORT mov_contable.
END.
