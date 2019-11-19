DEFINE VAR flagCorte AS LOGICAL.
DEFINE VAR flagAtraso AS LOGICAL.

FOR EACH creditos WHERE creditos.cod_credito = 123 AND creditos.sdo_Capital > 0:
    flagCorte = FALSE.
    flagCorte = FALSE.

    IF creditos.fec_pago > 11/05/2011 THEN
        creditos.fec_pago = 11/05/2011.

    /* Buscamos el resgitro en la tabla facturación */
    FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                             AND facturacion.num_credito = creditos.num_credito
                             AND facturacion.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE facturacion THEN DO:
        /* Si no está ninguno activo, significa que es nuevo o que está al día, entonces creamos uno nuevo */
        CREATE facturacion.
        ASSIGN facturacion.nit = creditos.nit
               facturacion.num_credito = creditos.num_credito
               facturacion.estado = 1.
    END.
    ELSE DO:
        IF creditos.dias_atraso > 0 THEN
            flagAtraso = TRUE. /* Si existe una facturación activa significa que está atrasado */
    END.

    /* Recorremos mov_Creditos para identificar si es necesario refinanciar */
    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                            AND mov_creditos.num_credito = creditos.num_credito
                            AND mov_creditos.fecha > ADD-INTERVAL(10/16/2011,-1,"months") NO-LOCK BY mov_creditos.fecha DESCENDING:
        FIND FIRST operacion WHERE operacion.cod_operacion = mov_creditos.cod_operacion NO-LOCK NO-ERROR.
        IF AVAILABLE operacion THEN DO:
            IF operacion.tipo_operacion = 2 THEN DO:
                /*creditos.cuota = ROUND((creditos.sdo_capital * (((creditos.tasa / 1200) * EXP((creditos.tasa / 1200) + 1,creditos.plazo)) / (EXP((creditos.tasa / 1200) + 1,creditos.plazo) - 1))),6).*/
                creditos.cuota = (creditos.sdo_capital / 18) + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar.
                creditos.cuota = TRUNCATE((creditos.cuota + 100) / 100,0) * 100.

                IF flagAtraso = FALSE THEN DO: /* Si no está atrasado cambio las fechas de desembolso, de inicio y de pago */
                    creditos.fec_desembolso = /*ADD-INTERVAL(TODAY,-1,"months") + 1*/ 09/17/2011.
                    creditos.fec_pagAnti = ADD-INTERVAL(TODAY,1,"months").
                    creditos.fec_pagAnti = 11/05/2011.
                    creditos.fec_pago = 11/05/2011.
                END.

                facturacion.cuota = facturacion.cuota + creditos.cuota.
                facturacion.fec_pago = creditos.fec_pago.
                facturacion.capital = facturacion.capital + (creditos.sdo_capital / 18).
                facturacion.int_corriente = facturacion.int_corriente + creditos.INT_corriente + creditos.INT_difCobro.
                facturacion.int_mora = facturacion.int_mora + creditos.int_MorCobrar.

                creditos.cuo_pagadas = 0.

                flagCorte = TRUE.

                LEAVE.
            END.
        END.
    END.

    /* Si no es refinanciado, montamos los valores en el registro de facturación existente o en el nuevo que se creó */
    IF flagCorte = FALSE THEN DO:
        facturacion.cuota = facturacion.cuota + creditos.cuota.

        IF creditos.fec_pago = 10/05/2011 THEN
            facturacion.cuota = facturacion.cuota + creditos.cuota.

        facturacion.fec_pago = creditos.fec_pago.
        facturacion.int_corriente = facturacion.int_corriente + creditos.INT_corriente + creditos.INT_difCobro.
        facturacion.int_mora = facturacion.int_mora + creditos.int_MorCobrar.
        facturacion.capital = facturacion.capital + (facturacion.cuota - facturacion.INT_corriente - facturacion.INT_mora).

        IF facturacion.capital < 0 THEN
            facturacion.capital = 0.
    END.

    CREATE mov_creditos.
    ASSIGN mov_creditos.agencia = creditos.agencia
           mov_creditos.cod_credito = 123
           mov_creditos.num_credito = creditos.num_credito
           mov_creditos.pagare = creditos.pagare
           mov_creditos.cod_operacion = 020303001
           mov_creditos.ofi_fuente = creditos.agencia
           mov_creditos.ofi_destino = creditos.agencia
           mov_creditos.fecha = 10/16/2011
           mov_creditos.val_efectivo = creditos.cuota
           mov_creditos.usuario = "2305"
           mov_creditos.hora = TIME
           mov_creditos.nit = creditos.nit
           mov_creditos.sdo_capital = creditos.sdo_capital
           mov_creditos.descrip = "Proceso Corte - Asigna Cuota".
END.

