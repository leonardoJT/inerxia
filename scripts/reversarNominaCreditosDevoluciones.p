DISABLE TRIGGERS FOR LOAD OF p.mov_contable.
DISABLE TRIGGERS FOR LOAD OF p.ahorros.
DISABLE TRIGGERS FOR LOAD OF p.mov_ahorros.

DEFINE TEMP-TABLE ttMovs LIKE p.mov_contable.

FOR EACH p.mov_contable WHERE p.mov_contable.agencia = 2
                          AND p.mov_contable.fec_contable = TODAY
                          AND p.mov_contable.comprobante = 7
                          AND p.mov_contable.num_documento = 2934
                          AND p.mov_contable.cuenta = "24451001" NO-LOCK:
    CREATE ttMovs.
    BUFFER-COPY p.mov_contable TO ttMovs.
    /*DISPLAY ttMovs WITH WIDTH 300 1 COL.*/
END.

Reversion:
DO TRANSACTION ON ERROR UNDO Reversion:
    FOR EACH ttMovs NO-LOCK:
        /* Busco en Produccion el ahorro y le resto el valor consignado */
        FIND FIRST p.ahorros WHERE p.ahorros.nit = ttMovs.nit
                               AND p.ahorros.cod_ahorro = 8
                               AND p.ahorros.estado = 1 NO-ERROR.
        IF AVAILABLE p.ahorros THEN DO:
            p.ahorros.sdo_disponible = p.ahorros.sdo_disponible - ttMovs.cr.

            FOR EACH p.mov_ahorros WHERE p.mov_ahorros.nit = ttMovs.nit
                                     AND p.mov_ahorros.cod_ahorro = 8
                                     AND p.mov_ahorros.cue_ahorros = p.ahorros.cue_ahorros
                                     AND p.mov_ahorros.fecha = TODAY
                                     AND p.mov_ahorros.val_efectivo = ttMovs.cr:
                DELETE p.mov_ahorros.
            END.
        
            /* Por último, borro los movimientos contables */
            FOR EACH p.mov_contable WHERE p.mov_contable.agencia = 2
                                      AND p.mov_contable.fec_contable = TODAY
                                      AND p.mov_contable.comprobante = 7
                                      AND p.mov_contable.num_documento = ttMovs.num_documento
                                      AND p.mov_contable.nit = ttMovs.nit
                                      AND p.mov_contable.doc_referencia = ttMovs.doc_referencia
                                      AND p.mov_contable.cuenta = ttMovs.cuenta:
                DELETE p.mov_contable.
            END.
        END.
    END.
END.

MESSAGE "Finaliza"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
