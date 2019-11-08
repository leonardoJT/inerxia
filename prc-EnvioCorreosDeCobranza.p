DEFINE VAR vDiaHabil AS LOGICAL.

/* Determinamos si es o no dìa hàbil */
IF WEEKDAY(TODAY) <> 7 /* Sábado */ AND
   WEEKDAY(TODAY) <> 1 /* Domingo */ THEN DO:
    FIND FIRST calendario WHERE calendario.dia = DAY(TODAY)
                            AND calendario.mes = MONTH(TODAY)
                            AND calendario.ano = YEAR(TODAY) NO-LOCK NO-ERROR.
    
    vDiaHabil = calendario.tipo.
END.
/* ----------------------- */

/* Enviamos los correos pendientes */
IF vDiaHabil = TRUE THEN DO:
    FOR EACH correos_cobranzas:
        FIND FIRST creditos WHERE creditos.nit = correos_cobranzas.cliente_id
                              AND creditos.cod_credito = correos_cobranzas.cod_credito
                              AND creditos.num_credito = correos_cobranzas.num_credito
                              AND creditos.dias_atraso >= correos_cobranzas.dias_mora NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN
            RUN enviar_correo.

        DELETE correos_cobranzas.
    END.
END.

/* Revisamos los vencimientos del dìa. Si es dìa hàbil se envía el correo, de lo contrario se deja en la tabla de log para el siguiente día hábil */
FOR EACH creditos WHERE creditos.for_pago <> 2
                    AND (creditos.fec_pago = TODAY + 1 OR
                         creditos.dias_atraso = 1 OR
                         creditos.dias_atraso = 25 OR
                         creditos.dias_atraso = 31 OR
                         creditos.dias_atraso = 60 OR
                         creditos.dias_atraso = 85)
                    AND creditos.estado = 2 NO-LOCK:
    IF vDiaHabil = TRUE THEN
        RUN enviar_correo.
    ELSE DO:
        CREATE correos_cobranzas.
        correos_cobranzas.cliente_id = creditos.nit.
        correos_cobranzas.cod_credito = creditos.cod_credito.
        correos_cobranzas.num_credito = creditos.num_credito.
        correos_cobranzas.dias_mora = creditos.dias_atraso.
        correos_cobranzas.fecha = TODAY.
    END.
END.

/*MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

DISCONNECT bdcentral.
QUIT.
QUIT.

/* Fin */


PROCEDURE enviar_correo:
DEFINE VAR vCco AS CHARACTER.
DEFINE VAR enviado AS LOGICAL.
DEFINE VAR vStatus AS CHARACTER.
DEFINE VAR vValor_aPagar AS DECIMAL.

vValor_aPagar = 0.

FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.

IF clientes.email <> ? AND clientes.email <> '' AND INDEX(clientes.email,'@') > 0 THEN DO:
    CASE creditos.agencia:
        WHEN 1 THEN vCco = "dprosass@fodun.com.co".
        WHEN 2 THEN vCco = "abermudezh@fodun.com.co".
        WHEN 3 THEN vCco = "lacastrillong@fodun.com.co".
        WHEN 4 THEN vCco = "lizquierdor@fodun.com.co".
    END CASE.

    IF creditos.cod_credito <> 123 THEN DO:
        IF creditos.fec_pago <= TODAY THEN DO:
            IF creditos.cod_credito <> 108 AND creditos.cod_credito <> 113 AND creditos.cod_credito <> 114 THEN DO:
                FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                         AND CONTROL_pagos.num_credito = creditos.num_credito
                                         AND CONTROL_pagos.id_pdoMes < 2
                                         AND CONTROL_pagos.fec_Vcto <= TODAY NO-LOCK:
                    vValor_aPagar = vValor_aPagar + creditos.cuota.
                END.

                vValor_aPagar = vValor_aPagar + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
            END.
            ELSE
                vValor_aPagar = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
        END.
        ELSE DO:
            IF creditos.cod_credito <> 108 AND creditos.cod_credito <> 113 AND creditos.cod_credito <> 114 THEN
                vValor_aPagar = creditos.cuota + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
            ELSE
                vValor_aPagar = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
        END.
    END.
    ELSE DO:
        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito
                               AND facturacion.estado = 1 NO-LOCK:
            vValor_aPagar = vValor_aPagar + facturacion.cuota - facturacion.pago_capital - facturacion.pago_intCorriente - facturacion.pago_intDifCobro - facturacion.pago_Mora.
        END.
    END.

    /*MESSAGE "NOMBRE:" clientes.nit + " - " + clientes.nombre + ' ' + clientes.apellido1 + ' ' + clientes.apellido2 SKIP
            "EMAIL:" clientes.email
            "DÍAS ATRASO:" creditos.dias_atraso SKIP
            "LÍNEA DE CRÉDITO:" pro_creditos.Nom_Producto SKIP
            "SALDO DE CAPITAL:" creditos.sdo_capital SKIP
            "CUOTA:" creditos.cuota SKIP
            "FECHA DE PAGO:" creditos.fec_pago SKIP
            "CCO:" vCco
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    RUN emailRecordatorioPago.r(INPUT clientes.nombre + ' ' + clientes.apellido1 + ' ' + clientes.apellido2,
                                INPUT clientes.email,
                                INPUT creditos.dias_atraso,
                                INPUT pro_creditos.Nom_Producto,
                                INPUT creditos.num_credito,
                                INPUT creditos.sdo_capital,
                                INPUT vValor_aPagar,
                                INPUT creditos.fec_pago,
                                INPUT vCco,
                                OUTPUT enviado,
                                OUTPUT vStatus) NO-ERROR.

    /*MESSAGE enviado SKIP
            vStatus
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END.

END PROCEDURE
