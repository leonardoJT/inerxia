DISABLE TRIGGERS FOR LOAD OF p.mov_contable.
DISABLE TRIGGERS FOR LOAD OF p.creditos.
DISABLE TRIGGERS FOR LOAD OF p.mov_creditos.
DISABLE TRIGGERS FOR LOAD OF p.CONTROL_pagos.
DISABLE TRIGGERS FOR LOAD OF p.planPagos.
DISABLE TRIGGERS FOR LOAD OF p.amortizacion.

DEFINE TEMP-TABLE ttMovs LIKE p.mov_contable.

FOR EACH p.mov_contable WHERE p.mov_contable.agencia = 2
                          AND p.mov_contable.fec_contable = TODAY
                          AND p.mov_contable.comprobante = 7
                          AND p.mov_contable.doc_referencia <> ""
                          AND p.mov_contable.num_documento = 2934 NO-LOCK:
    CREATE ttMovs.
    BUFFER-COPY p.mov_contable TO ttMovs.
END.

Reversion:
DO TRANSACTION ON ERROR UNDO Reversion:
    FOR EACH ttMovs WHERE ttMovs.agencia = 2
                      AND ttMovs.fec_contable = TODAY
                      AND ttMovs.comprobante = 7
                      AND ttMovs.doc_referencia <> "" NO-LOCK BREAK BY ttMovs.doc_referencia:
        IF FIRST-OF(ttMovs.doc_referencia) THEN DO:
            /* Busco en Produccion el crédito y lo borro */
            FIND FIRST p.creditos WHERE p.creditos.nit = ttMovs.nit
                                    AND p.creditos.num_credito = INTEGER(ttMovs.doc_referencia) NO-ERROR.
            IF AVAILABLE p.creditos THEN
                DELETE p.creditos.

            FOR EACH p.mov_creditos WHERE p.mov_creditos.nit = ttMovs.nit
                                      AND p.mov_creditos.num_credito = INTEGER(ttMovs.doc_referencia)
                                      AND p.mov_creditos.fecha = TODAY:
                DELETE p.mov_creditos.
            END.

            FOR EACH p.CONTROL_pagos WHERE p.CONTROL_pagos.nit = ttMovs.nit
                                       AND p.CONTROL_pagos.num_credito = INTEGER(ttMovs.doc_referencia):
                DELETE p.CONTROL_pagos.
            END.

            FOR EACH p.planPagos WHERE p.planPagos.nit = ttMovs.nit
                                   AND p.planPagos.num_credito = INTEGER(ttMovs.doc_referencia):
                DELETE p.planPagos.
            END.

            FOR EACH p.amortizacion WHERE p.amortizacion.nit = ttMovs.nit
                                      AND p.amortizacion.num_credito = INTEGER(ttMovs.doc_referencia):
                DELETE p.amortizacion.
            END.
            /* ----------------------- */

            /* Ahora paso todos los registros de desarrollo a Produccion */
            FIND FIRST d.creditos WHERE d.creditos.nit = ttMovs.nit
                                    AND d.creditos.num_credito = INTEGER(ttMovs.doc_referencia) NO-LOCK NO-ERROR.
            IF AVAILABLE d.creditos THEN DO:
                CREATE p.creditos.
                BUFFER-COPY d.creditos TO p.creditos.
            END.

            FOR EACH d.CONTROL_pagos WHERE d.CONTROL_pagos.nit = ttMovs.nit
                                       AND d.CONTROL_pagos.num_credito = INTEGER(ttMovs.doc_referencia) NO-LOCK:
                CREATE p.CONTROL_pagos.
                BUFFER-COPY d.CONTROL_pagos TO p.CONTROL_pagos.
            END.

            FOR EACH d.planPagos WHERE d.planPagos.nit = ttMovs.nit
                                   AND d.planPagos.num_credito = INTEGER(ttMovs.doc_referencia):
                CREATE p.planPagos.
                BUFFER-COPY d.planPagos TO p.planPagos.
            END.

            FOR EACH d.amortizacion WHERE d.amortizacion.nit = ttMovs.nit
                                      AND d.AMORTIZACION.num_credito = INTEGER(ttMovs.doc_referencia):
                CREATE p.amortizacion.
                BUFFER-COPY d.amortizacion TO p.amortizacion.
            END.
            /* ----------------------- */

            /* Por último, borro todos los movimientos contables pertenecientes a ese crédito */
            FOR EACH p.mov_contable WHERE p.mov_contable.agencia = 2
                                      AND p.mov_contable.fec_contable = TODAY
                                      AND p.mov_contable.comprobante = 7
                                      AND p.mov_contable.num_documento = ttMovs.num_documento
                                      AND p.mov_contable.nit = ttMovs.nit
                                      AND p.mov_contable.doc_referencia = ttMovs.doc_referencia:
                DELETE p.mov_contable.
            END.
        END.
    END.
END.

MESSAGE "Finaliza"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
