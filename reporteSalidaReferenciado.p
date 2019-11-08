DEFINE INPUT PARAMETER pUsuario AS CHARACTER.

DEFINE VAR vNombreArchivo AS CHARACTER.
DEFINE BUFFER bfrAhorros FOR ahorros.
DEFINE VAR vCuota AS INTEGER.
DEFINE VAR numConvenio AS INTEGER INITIAL 1177849.
DEFINE VAR fechaPago AS DATE.

DEFINE TEMP-TABLE archivo
    FIELD registro AS CHARACTER.

/* 1. APORTES */
FOR EACH ahorros WHERE ahorros.tip_ahorro = 4
                   AND ahorros.FOR_pago = 1
                   AND ahorros.cuota > 0
                   AND ahorros.estado = 1 NO-LOCK:
    FIND FIRST mov_ahorros WHERE mov_ahorros.agencia = ahorros.agencia
                             AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                             AND mov_ahorros.cue_ahorro = ahorros.cue_ahorro
                             AND MONTH(mov_ahorros.fecha) = MONTH(TODAY)
                             AND YEAR(mov_ahorros.fecha) = YEAR(TODAY)
                             AND mov_ahorros.cod_operacion = 010101001 NO-LOCK NO-ERROR.
    IF AVAILABLE mov_ahorros THEN
        NEXT.

    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
    
    FIND FIRST bfrAhorros WHERE bfrAhorros.nit = clientes.nit
                            AND bfrAhorros.cod_ahorro = 3
                            AND bfrAhorros.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfrAhorros THEN DO:
        MESSAGE "Para la cédula" ahorros.nit "no se encontró" SKIP
                "una cuenta de Ahorro Permanente activa."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        vCuota = ahorros.cuota.
    END.
    ELSE
        vCuota = ahorros.cuota + bfrAhorros.cuota.

    CREATE archivo.
    archivo.registro = STRING(numConvenio,"99999999").
    archivo.registro = archivo.registro + STRING(DECIMAL(clientes.nit),"99999999999999999999999999999999").
    archivo.registro = archivo.registro + STRING(ahorros.cod_ahorro,"99999999999999999999999999999999").
    archivo.registro = archivo.registro + STRING(vCuota,"999999999999").
    
    IF DAY(TODAY) >= 10 THEN
        archivo.registro = archivo.registro + STRING(YEAR(ADD-INTERVAL(TODAY,1,"months")),"9999") + STRING(MONTH(ADD-INTERVAL(TODAY,1,"months")),"99") + "10".
    ELSE
        archivo.registro = archivo.registro + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + "10".

    archivo.registro = archivo.registro + "00000000000000000000000000000000000000".
END.

/* 2. CREDITOS */

FOR EACH creditos WHERE creditos.FOR_pago = 1
                    AND creditos.cuota > 0
                    AND creditos.fec_pago <> ?
                    AND creditos.estado = 2 NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.

    IF creditos.cod_credito = 123 THEN DO:
        FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                                 AND facturacion.num_credito = creditos.num_credito
                                 AND facturacion.estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE facturacion THEN
            NEXT.
    END.

    CREATE archivo.
    archivo.registro = STRING(numConvenio,"99999999").
    archivo.registro = archivo.registro + STRING(DECIMAL(clientes.nit),"99999999999999999999999999999999").
    archivo.registro = archivo.registro + STRING(creditos.num_credito,"99999999999999999999999999999999").
    
    vCuota = 0.

    IF creditos.cod_credito <> 123 THEN DO:
        IF creditos.fec_pago <= TODAY THEN DO:
            fechaPago = TODAY.

            IF creditos.cod_credito <> 108 AND creditos.cod_credito <> 113 AND creditos.cod_credito <> 114 THEN DO:
                FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                         AND CONTROL_pagos.num_credito = creditos.num_credito
                                         AND CONTROL_pagos.id_pdoMes < 2
                                         AND CONTROL_pagos.fec_Vcto <= TODAY NO-LOCK:
                    vCuota = vCuota + creditos.cuota.
                END.
            
                vCuota = vCuota + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
            END.
            ELSE
                vCuota = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
        END.
        ELSE DO:
            fechaPago = creditos.fec_pago.
            
            IF creditos.cod_credito <> 108 AND creditos.cod_credito <> 113 AND creditos.cod_credito <> 114 THEN
                vCuota = creditos.cuota + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
            ELSE
                vCuota = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
        END.
    END.
    ELSE DO:
        IF creditos.fec_pago <= TODAY THEN
            fechaPago = TODAY.
        ELSE
            fechaPago = creditos.fec_pago.

        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito
                               AND facturacion.estado = 1 NO-LOCK:
            vCuota = vCuota + facturacion.cuota - facturacion.pago_capital - facturacion.pago_intCorriente - facturacion.pago_intDifCobro - facturacion.pago_Mora.
        END.
    END.

    archivo.registro = archivo.registro + STRING(vCuota,"999999999999").
    archivo.registro = archivo.registro + STRING(YEAR(fechaPago),"9999") + STRING(MONTH(fechaPago),"99") + STRING(DAY(fechaPago),"99").
    archivo.registro = archivo.registro + "00000000000000000000000000000000000000".
END.

vNombreArchivo = "c:\INFO_Fodun\reporteReferenciado_" + STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999") + "_" + STRING(TIME,"99999") + ".txt".

OUTPUT TO VALUE(vNombreArchivo).
FOR EACH archivo NO-LOCK:
    PUT UNFORMAT archivo.registro SKIP.
END.
OUTPUT CLOSE.

CREATE recaudos_IO.
recaudos_IO.tipo_io = 'O'.
recaudos_IO.tipo_convenio = "Referenciado".
recaudos_IO.fecha = TODAY.
recaudos_IO.hora = TIME.
recaudos_IO.nombreArchivo = vNombreArchivo.
recaudos_IO.usuario = pUsuario.
