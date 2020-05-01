DEFINE VAR cuotas AS INTEGER.
DEFINE VAR diferencia AS INTEGER.
DEFINE VAR valorCredito AS DECIMAL.

FOR EACH creditos WHERE (creditos.cod_credito = 108 OR
                         creditos.cod_credito = 113 OR
                         creditos.cod_credito = 114)
                    AND creditos.estado = 2 BY creditos.fec_desembolso:
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.nro_cuota >= 1 NO-LOCK BY CONTROL_pagos.nro_cuota DESC:
        cuotas = CONTROL_pagos.nro_cuota.
        LEAVE.
    END.

    IF cuotas <> 1 THEN DO:
        MESSAGE creditos.nit creditos.num_credito creditos.fec_desembolso creditos.plazo cuotas creditos.cuo_pagadas
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        creditos.plazo = 1.

        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito BY CONTROL_pagos.nro_cuota DESC:
            CONTROL_pagos.nro_cuota = 1.
            LEAVE.
        END.

        MESSAGE creditos.plazo cuotas - diferencia creditos.cuo_pagadas
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
