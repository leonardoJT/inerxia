DEFINE VAR valorCredito AS DECIMAL.

FOR EACH creditos WHERE creditos.sdo_Capital > 0
                    AND creditos.estado = 2
                    AND creditos.cod_credito <> 123:
    creditos.cuo_pagadas = 0.

    valorCredito = creditos.monto.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito NO-LOCK BY CONTROL_pagos.nro_cuota:
        valorCredito = valorCredito - control_pagos.pagos_capitalAcum.

        IF valorCredito > creditos.sdo_Capital THEN
            creditos.cuo_pagadas = CONTROL_pagos.nro_cuota.
        ELSE
            LEAVE.
    END.
END.
