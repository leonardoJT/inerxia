FOR EACH creditos WHERE creditos.sdo_capital > 0
                    AND creditos.per_pago = 8
                    AND creditos.fec_desembolso <= 02/28/2011 NO-LOCK:
    FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                               AND CONTROL_pagos.num_credito = creditos.num_credito
                               AND CONTROL_pagos.id_PdoMes < 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CONTROL_pagos THEN DO:
        RUN D:\SPS\soportes\fodun\Prog\scripts\GeneraPlanDePagos.p(INPUT creditos.nit,
                                                                   INPUT creditos.num_credito,
                                                                   INPUT creditos.plazo).


        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.cod_credito = creditos.cod_credito
                                 AND CONTROL_pagos.num_credito = creditos.num_credito
                                 AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
            CONTROL_pagos.causacion = creditos.INT_corriente.
            LEAVE.
        END.
    END.
    ELSE
        MESSAGE creditos.nit creditos.num_credito
            VIEW-AS ALERT-BOX INFO BUTTONS OK.



    /*RUN D:\SPS\soportes\fodun\Prog\scripts\CorregirControlPagosTodos.p(INPUT creditos.nit,
                                                                       INPUT creditos.num_credito).*/
END.
