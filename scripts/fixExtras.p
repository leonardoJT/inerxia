FOR EACH creditos WHERE creditos.estado = 2
                    AND creditos.num_credito > 23774 NO-LOCK BY creditos.num_credito:
    FIND FIRST extras WHERE extras.nit = creditos.nit
                        AND extras.num_solicitud = creditos.num_solicitud NO-LOCK NO-ERROR.
    IF NOT AVAILABLE extras THEN
        NEXT.
    ELSE DO:
        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito
                                 AND CONTROL_pagos.id_pdoMes < 2
                                 AND CONTROL_pagos.cuota > creditos.cuota
                                 AND CONTROL_pagos.fec_vcto <= 12/31/2020
                                 AND CONTROL_pagos.nro_cuota < creditos.plazo NO-LOCK:
            FIND FIRST amortizacion WHERE amortizacion.nit = credito.nit
                                      AND amortizacion.num_credito = creditos.num_credito
                                      AND amortizacion.fec_pago = CONTROL_pagos.fec_vcto NO-LOCK NO-ERROR.
            IF AVAILABLE amortizacion THEN DO:
                IF CONTROL_pagos.cuota > amortizacion.cuota THEN
                    MESSAGE creditos.nit creditos.cod_credito creditos.num_credito CONTROL_pagos.fec_vcto
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
        END.
    END.
END.
