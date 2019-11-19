DEFINE VAR valorCredito AS DECIMAL.
DEFINE VAR deuda AS DECIMAL.
DEFINE VAR saldoProyectado AS DECIMAL.
DEFINE VAR saldoAdescontar AS DECIMAL.
    
FOR EACH creditos WHERE creditos.cod_credito = 123
                    /*AND fec_pagAnti = 10/05/2011*/
                    AND creditos.sdo_capital > 0:
    saldoAdescontar = 0.
    creditos.val_atraso = 0.
    creditos.cuo_atraso = 0.

    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                            AND mov_creditos.num_credito = creditos.num_credito
                            AND mov_creditos.fecha <= ADD-INTERVAL(creditos.fec_pagAnti,-1,"months") + 11 NO-LOCK BY mov_creditos.fecha DESCENDING:
        valorCredito = mov_creditos.Sdo_Capital.
        LEAVE.
    END.

    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                            AND mov_creditos.num_credito = creditos.num_credito
                            AND mov_creditos.fecha > ADD-INTERVAL(creditos.fec_pagAnti,-1,"months") + 11 NO-LOCK BY mov_creditos.fecha:
        FIND FIRST operacion WHERE operacion.cod_operacion = mov_creditos.cod_operacion NO-LOCK NO-ERROR.
        IF AVAILABLE operacion THEN DO:
            IF operacion.tipo_operacion = 2 THEN
                saldoAdescontar = saldoAdescontar + mov_creditos.val_efectivo + mov_creditos.val_cheque.
        END.
    END.

    deuda = valorCredito.

    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                                AND mov_creditos.num_credito = creditos.num_credito
                                AND mov_creditos.fecha <= ADD-INTERVAL(creditos.fec_pagAnti,-1,"months") + 11 NO-LOCK BY mov_creditos.fecha DESCENDING:
        valorCredito = mov_creditos.Sdo_Capital.
        LEAVE.
    END.

    FOR EACH CONTROL_pagos WHERE control_pagos.Nit = creditos.nit
                             AND control_pagos.Num_Credito = creditos.num_credito BY CONTROL_pagos.nro_cuota:
        IF round(CONTROL_pagos.pagos_capitalAcum,0) > 0 THEN
            valorCredito = valorCredito - round(CONTROL_pagos.pagos_capitalAcum,0).

        creditos.fec_pago = CONTROL_pagos.fec_Vcto.
        CONTROL_pagos.id_pdoMes = 2.

        IF valorCredito < (creditos.sdo_capital - saldoAdescontar) OR control_pagos.fec_Vcto >= TODAY + 30 OR (creditos.sdo_capital - saldoAdescontar) = deuda THEN DO:
            CONTROL_pagos.id_PdoMes = 1.
            creditos.cuo_pagadas = CONTROL_pagos.nro_cuota - 1.
            LEAVE.
        END.
    END.

    IF creditos.fec_pago <= TODAY THEN
        creditos.dias_atraso = TODAY - creditos.fec_pago.

    /* 2. Calculamos el saldo proyectado y otras variables */
    saldoProyectado = deuda.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.fec_vcto <= TODAY NO-LOCK BY CONTROL_pagos.fec_vcto:
        saldoProyectado = saldoProyectado - CONTROL_pagos.pagos_capitalAcum.

        /* Calculamos las otras variables */
        IF creditos.fec_pago <= CONTROL_pagos.fec_Vcto THEN DO:
            creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
            creditos.cuo_atraso = creditos.cuo_atraso + 1.
        END.
    END.

    creditos.sdo_proyectado = saldoProyectado.

    IF creditos.dias_atraso = 0 THEN DO:
        creditos.val_atraso = 0.
        creditos.cuo_atraso = 0.
        creditos.sdo_proyectado = creditos.sdo_Capital.
    END.
END.
