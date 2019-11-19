DEFINE VAR flagFecPago AS LOGICAL.
DEFINE VAR deuda AS DECIMAL.
DEFINE VAR saldoProyectado AS DECIMAL.

FOR EACH creditos WHERE creditos.cod_credito <> 123
                    AND creditos.sdo_capital > 0:
    flagfecPago = FALSE.
    deuda = Creditos.monto.
    creditos.val_atraso = 0.
    creditos.dias_atraso = 0.
    
    /* Arreglamos el Control Pagos cerrando las cuotas ya pagadas */
    FOR EACH CONTROL_pagos WHERE control_pagos.Nit EQ Creditos.Nit
                             and control_pagos.Num_Credito EQ Creditos.Num_Credito BY control_pagos.Nro_Cuota:
        IF ROUND(control_pagos.pagos_capitalAcum,0) > 0 THEN
            deuda = deuda - ROUND(control_pagos.pagos_capitalAcum,0).

        IF deuda >= creditos.sdo_Capital AND creditos.monto <> creditos.sdo_capital AND control_pagos.fec_Vcto < creditos.fec_pago THEN
            CONTROL_pagos.id_pdoMes = 2.
        ELSE DO:
            IF flagfecPago = FALSE THEN DO:
                CONTROL_pagos.id_PdoMes = 0.
                creditos.cuo_pagadas = CONTROL_pagos.nro_cuota.
                flagFecPago = TRUE.
            END.
            ELSE
                CONTROL_pagos.id_PdoMes = 1.
        END.
    END.


    /* Días de atraso */
    IF creditos.fec_pago < TODAY THEN
        creditos.dias_atraso = TODAY - creditos.fec_pago.
    ELSE
        creditos.dias_atraso = 0.

    /* Cuotas atrasadas - Valor del atraso */
    IF creditos.dias_atraso > 0 THEN DO:
        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito
                                 AND CONTROL_pagos.id_pdoMes < 2
                                 AND CONTROL_pagos.fec_Vcto <= TODAY NO-LOCK:
            creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
            creditos.cuo_atraso = creditos.cuo_atraso + 1.
        END.
    END.

    /* Saldo proyectado */
    saldoProyectado = creditos.monto.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.fec_vcto <= TODAY NO-LOCK BY CONTROL_pagos.fec_vcto:
        saldoProyectado = saldoProyectado - CONTROL_pagos.pagos_capitalAcum.
    END.

    IF saldoProyectado < creditos.sdo_capital OR creditos.dias_atraso > 0 THEN
        creditos.sdo_proyectado = saldoProyectado.
    ELSE
        creditos.sdo_proyectado = creditos.sdo_capital.
END.

