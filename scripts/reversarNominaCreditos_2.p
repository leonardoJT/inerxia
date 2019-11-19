DISABLE TRIGGERS FOR LOAD OF p.mov_contable.
DISABLE TRIGGERS FOR LOAD OF p.creditos.
DISABLE TRIGGERS FOR LOAD OF p.mov_creditos.
DISABLE TRIGGERS FOR LOAD OF p.CONTROL_pagos.
DISABLE TRIGGERS FOR LOAD OF p.planPagos.
DISABLE TRIGGERS FOR LOAD OF p.amortizacion.

DEFINE TEMP-TABLE ttMovs
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER.

CREATE ttMovs.
ttMovs.nit = "43074030".
ttMovs.num_credito = 99011457.

Reversion:
DO TRANSACTION ON ERROR UNDO Reversion:
    FOR EACH ttMovs NO-LOCK:
        /* Busco en Produccion el crédito y lo borro */
        FIND FIRST p.creditos WHERE p.creditos.nit = ttMovs.nit
                                AND p.creditos.num_credito = ttMovs.num_credito NO-ERROR.
        IF AVAILABLE p.creditos THEN
            DELETE p.creditos.

        FOR EACH p.mov_creditos WHERE p.mov_creditos.nit = ttMovs.nit
                                  AND p.mov_creditos.num_credito = ttMovs.num_credito
                                  AND p.mov_creditos.fecha = TODAY:
            DELETE p.mov_creditos.
        END.

        FOR EACH p.CONTROL_pagos WHERE p.CONTROL_pagos.nit = ttMovs.nit
                                   AND p.CONTROL_pagos.num_credito = ttMovs.num_credito:
            DELETE p.CONTROL_pagos.
        END.

        FOR EACH p.planPagos WHERE p.planPagos.nit = ttMovs.nit
                               AND p.planPagos.num_credito = ttMovs.num_credito:
            DELETE p.planPagos.
        END.

        FOR EACH p.amortizacion WHERE p.amortizacion.nit = ttMovs.nit
                                  AND p.amortizacion.num_credito = ttMovs.num_credito:
            DELETE p.amortizacion.
        END.
        /* ----------------------- */

        /* Ahora paso todos los registros de desarrollo a Produccion */
        FIND FIRST d.creditos WHERE d.creditos.nit = ttMovs.nit
                                AND d.creditos.num_credito = ttMovs.num_credito NO-LOCK NO-ERROR.
        IF AVAILABLE d.creditos THEN DO:
            CREATE p.creditos.
            BUFFER-COPY d.creditos TO p.creditos.
        END.

        FOR EACH d.CONTROL_pagos WHERE d.CONTROL_pagos.nit = ttMovs.nit
                                   AND d.CONTROL_pagos.num_credito = ttMovs.num_credito NO-LOCK:
            CREATE p.CONTROL_pagos.
            BUFFER-COPY d.CONTROL_pagos TO p.CONTROL_pagos.
        END.

        FOR EACH d.planPagos WHERE d.planPagos.nit = ttMovs.nit
                               AND d.planPagos.num_credito = ttMovs.num_credito:
            CREATE p.planPagos.
            BUFFER-COPY d.planPagos TO p.planPagos.
        END.

        FOR EACH d.amortizacion WHERE d.amortizacion.nit = ttMovs.nit
                                  AND d.AMORTIZACION.num_credito = ttMovs.num_credito:
            CREATE p.amortizacion.
            BUFFER-COPY d.amortizacion TO p.amortizacion.
        END.
        /* ----------------------- */
    END.
END.

MESSAGE "Finaliza"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
