DEFINE VAR saldoProyectado AS DECIMAL.
DEFINE VAR w_fechaTr AS DATE.
DEFINE VAR fechaAux AS DATE.
DEFINE VAR diasAsumar AS INTEGER.
DEFINE VAR diasAtrasoAux AS INTEGER.
DEFINE VAR valorCredito AS DECIMAL.
DEFINE VAR flagFecPago AS LOGICAL.
DEFINE VAR fecPago AS DATE.

FIND FIRST creditos WHERE creditos.nit = "71180876"
                      AND creditos.num_credito = 1498 NO-LOCK NO-ERROR.

/* 1. Calculamos la fecha de pago */
valorCredito = creditos.monto.

FOR EACH CONTROL_pagos WHERE control_pagos.Nit = creditos.nit
                         AND control_pagos.Num_Credito = creditos.num_credito BY CONTROL_pagos.nro_cuota:
    IF ROUND(CONTROL_pagos.pagos_capitalAcum,0) > 0 THEN
        valorCredito = valorCredito - ROUND(CONTROL_pagos.pagos_capitalAcum,0).

    fecPago = CONTROL_pagos.fec_Vcto.
    CONTROL_pagos.id_pdoMes = 2.

    MESSAGE control_pagos.nro_Cuota fecPago valorCredito
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF valorCredito < creditos.sdo_capital OR control_pagos.fec_Vcto >= w_fechaTr + 30 OR creditos.monto = creditos.sdo_capital THEN DO:
        CONTROL_pagos.id_PdoMes = 1.
        /*creditos.cuo_pagadas = CONTROL_pagos.nro_cuota - 1.*/
        LEAVE.
    END.
END.
/*
/* 2. Calculamos el saldo proyectado y otras variables */
saldoProyectado = creditos.monto.

FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                         AND CONTROL_pagos.num_credito = creditos.num_credito
                         AND CONTROL_pagos.fec_vcto <= w_fecha NO-LOCK BY CONTROL_pagos.fec_vcto:
    saldoProyectado = saldoProyectado - CONTROL_pagos.pagos_capitalAcum.
    numeroCuota = CONTROL_pagos.nro_cuota.

    /* Calculamos las otras variables */
    IF creditos.fec_pago <= CONTROL_pagos.fec_Vcto THEN DO:
        creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
        creditos.cuo_atraso = creditos.cuo_atraso + 1.
    END.
END.

creditos.sdo_proyectado = saldoProyectado.

IF creditos.val_atraso LE 0 THEN
    Creditos.Cod_Califica = 1.

/* 3. Cálculo de calificacion y provision*/
RUN Calificar NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

END PROCEDURE.

*/
