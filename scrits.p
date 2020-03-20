DEFINE VAR cuotas AS INTEGER.

FOR EACH creditos WHERE creditos.cod_credito <> 123
                    AND creditos.num_credito > 6210
                    AND creditos.estado = 2 NO-LOCK BY creditos.num_credito:
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.nro_cuota >= 1 NO-LOCK:
        DISPLAY creditos.nit creditos.num_credito creditos.cuo_pagadas CONTROL_pagos.nro_cuota CONTROL_pagos.fec_vcto CONTROL_pagos.id_pdoMes WITH WIDTH 320.
    END.

    LEAVE.

END.
