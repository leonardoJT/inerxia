OUTPUT TO c:\INFO_Fodun\Leonardo\MovContableSinMovAhorros_21050502.csv.

FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable <= 12/31/2011
                            AND mov_contable.fec_contable >= 03/01/2011
                            /*AND mov_contable.nit <> ""*/
                            AND mov_contable.cuenta = "21050502"
                            AND mov_contable.comprobante <> 20 NO-LOCK BY mov_contable.fec_contable
                                                                       BY mov_contable.comprobante
                                                                       BY mov_contable.num_documento:
        FIND FIRST mov_ahorros WHERE mov_ahorros.fecha = mov_contable.fec_contable
                                 AND mov_ahorros.nit = mov_contable.nit
                                 /*AND mov_ahorros.cue_ahorros = mov_contable.doc_ref*/
                                 AND mov_ahorros.val_efectivo + mov_ahorros.val_cheque = mov_contable.db + mov_contable.cr
                                 AND mov_ahorros.usuario = mov_contable.usuario
                                 AND mov_ahorros.num_documento = STRING(mov_contable.num_documento) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE mov_ahorros THEN DO:
            EXPORT DELIMITER ";" mov_contable.agencia
                                 mov_contable.fec_contable
                                 mov_contable.nit
                                 mov_contable.num_documento
                                 mov_contable.comprobante
                                 mov_contable.comentario
                                 mov_contable.db
                                 mov_contable.cr
                                 mov_contable.usuario
                                 mov_contable.fec_grabacion.
        END.
    END.
END.
