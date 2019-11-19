FOR EACH creditos WHERE creditos.estado = 2 AND creditos.cod_credito <> 123 NO-LOCK BY fec_desembolso:
    FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                               AND CONTROL_pagos.num_credito = creditos.num_credito
                               AND CONTROL_pagos.fec_vcto = ? NO-LOCK NO-ERROR.
    IF AVAILABLE CONTROL_pagos THEN
        DISPLAY creditos.agencia creditos.nit creditos.num_credito fec_desembolso.
END.
