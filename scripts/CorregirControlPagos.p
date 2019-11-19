DEFINE VAR deuda AS DECIMAL.
DEFINE VAR fechaPago AS DATE.
DEFINE VAR numCuota AS INTEGER.
DEFINE VAR plz AS INTEGER.
DEFINE VAR fecDesembolso AS DATE.
DEFINE VAR deudaActual AS DECIMAL.
DEFINE VAR flag AS LOGICAL.

/*OUTPUT TO c:\RevisionCreditos.txt.*/

FOR EACH creditos WHERE creditos.agencia = 2
                    AND creditos.nit = "98544399"
                    AND creditos.num_credito = 11434
                    AND creditos.sdo_capital > 0
                    AND plazo > 1 
                    AND creditos.fec_desembolso <= 02/28/2011 NO-LOCK:
    deudaActual = creditos.sdo_Capital.

    /* Reviso si con las cuotas abiertas alcanza a pagar el saldo de capital actual */
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.cod_credito = creditos.cod_credito
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2 NO-LOCK BY CONTROL_pagos.nro_cuota:
        IF flag = FALSE THEN DO: /* Arranco en la primera -  Uso un flag porque con Break-By no las está cogiendo en orden ??? */
            ASSIGN fechaPago = CONTROL_pagos.fec_vcto
                   numCuota = CONTROL_pagos.nro_cuota.

            flag = TRUE.
        END.
                   
        deudaActual = deudaActual - CONTROL_pagos.pagos_capitalAcum + CONTROL_pagos.cap_pagado.
    END.

    flag = FALSE.

    /* Si no alcanza */
    IF deudaActual > 0 THEN DO:
        DISPLAY creditos.agencia creditos.nit creditos.num_credito numCuota  fechaPago creditos.fec_pago WITH WIDTH 200.

        deudaActual = creditos.sdo_capital.

        /* Recorro control pagos de forma inversa (de la última hacia atrás) */
        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.cod_credito = creditos.cod_credito
                                 AND CONTROL_pagos.num_credito = creditos.num_credito
                                 /*AND CONTROL_pagos.id_pdoMes < 2*/ BY CONTROL_pagos.nro_cuota DESCENDING:
            deudaActual = deudaActual - CONTROL_pagos.pagos_capitalAcum.

            /* Abro la cuota */
            CONTROL_pagos.id_pdoMes = 0.
            CONTROL_pagos.INT_pagado = 0.
            CONTROL_pagos.cap_pagado = 0.

            /* Si da negativo significa que a partir de esta cuota puedo pagar el saldo de capital actual */
            IF deudaActual <= 0 THEN DO:
                CONTROL_pagos.id_pdoMes = 1. /* La marco como la actual */
                CONTROL_pagos.cap_pagado = ABS(deudaActual).
                LEAVE.
            END.
        END.

        /* Corro las fechas en control_pagos a partir de la fecha de pago de creditos.fec_pago */
        fechaPago = creditos.fec_pago.

        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.cod_credito = creditos.cod_credito
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
            CONTROL_pagos.fec_Vcto = fechaPago.
            fechaPago = ADD-INTERVAL(fechaPago,1,"months").
        END.
    END.
END.
