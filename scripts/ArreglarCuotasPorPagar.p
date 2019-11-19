DISABLE TRIGGERS FOR LOAD OF creditos.

DEFINE VAR valorCredito AS DECIMAL.

FOR EACH creditos WHERE creditos.cod_credito <> 123
                    AND creditos.nit = "32310214"
                    AND creditos.estado = 2 BY creditos.fec_desembolso:
    valorCredito = 0.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito BY CONTROL_pagos.nro_cuota DESC:
        valorCredito = valorCredito + control_pagos.pagos_capitalAcum.
        
        IF valorCredito >= creditos.sdo_Capital THEN DO:
            MESSAGE valorCredito sdo_Capital nro_cuota
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            creditos.cuo_pagadas = CONTROL_pagos.nro_cuota - 1.

            IF CONTROL_pagos.nro_cuota - 1 < 0 THEN
                creditos.cuo_pagadas = 0.

            LEAVE.
        END.
    END.
END.
