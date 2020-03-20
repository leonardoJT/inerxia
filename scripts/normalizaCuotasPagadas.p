DEFINE VAR cuotas AS INTEGER.
DEFINE VAR diferencia AS INTEGER.
DEFINE VAR valorCredito AS DECIMAL.

FOR EACH creditos WHERE creditos.cod_credito <> 123
                    AND creditos.cod_credito <> 108
                    AND creditos.cod_credito <> 113
                    AND creditos.cod_credito <> 114
                    AND creditos.estado = 2 BY creditos.fec_desembolso:
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.nro_cuota >= 1 NO-LOCK BY CONTROL_pagos.nro_cuota DESC:
        cuotas = CONTROL_pagos.nro_cuota.
        LEAVE.
    END.

    IF creditos.plazo <> cuotas THEN DO:
        MESSAGE creditos.nit creditos.num_credito creditos.fec_desembolso creditos.plazo cuotas creditos.cuo_pagadas
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        diferencia = cuotas - creditos.plazo.
        valorCredito = 0.

        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito BY CONTROL_pagos.nro_cuota DESC:
            CONTROL_pagos.nro_cuota = CONTROL_pagos.nro_cuota - diferencia.
        END.

        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito BY CONTROL_pagos.nro_cuota DESC:
            valorCredito = valorCredito + control_pagos.pagos_capitalAcum.

            IF valorCredito > creditos.sdo_Capital THEN DO:
                creditos.cuo_pagadas = CONTROL_pagos.nro_cuota - 1.

                IF creditos.cuo_pagadas < 0 THEN
                    creditos.cuo_pagadas = 0.

                LEAVE.
            END.
        END.

        MESSAGE creditos.plazo cuotas - diferencia creditos.cuo_pagadas
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        LEAVE.
    END.
END.
