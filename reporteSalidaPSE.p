DEFINE INPUT PARAMETER pUsuario AS CHARACTER.

DEFINE VAR vNombreArchivo AS CHARACTER.
DEFINE BUFFER bfrAhorros FOR ahorros.
DEFINE VAR vCuota AS INTEGER.

DEFINE TEMP-TABLE archivo
    FIELD id AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD valorConIva AS INTEGER
    FIELD valorIva AS INTEGER
    FIELD concepto AS CHARACTER
    FIELD fecVencimiento AS CHARACTER
    FIELD email AS CHARACTER
    FIELD claveParaPago AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD apellido AS CHARACTER
    FIELD telefono AS CHARACTER
    FIELD CampoOpcional_1 AS CHARACTER
    FIELD CampoOpcional_2 AS CHARACTER
    FIELD CampoOpcional_3 AS CHARACTER.

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
    archivo.id = SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "1" + SUBSTRING(ahorros.cue_ahorros,1,7).
    archivo.nit = ahorros.nit.
    archivo.valorConIva = vCuota.
    archivo.concepto = "APORTE".
    
    IF DAY(TODAY) >= 10 THEN
        archivo.fecVencimiento = "10/" + SUBSTRING(STRING(ADD-INTERVAL(TODAY,1,"months"),"99/99/9999"),4).
    ELSE
        archivo.fecVencimiento = "10/" + SUBSTRING(STRING(TODAY,"99/99/9999"),4).

    archivo.email = clientes.email.
    archivo.nombre = clientes.nombre.
    archivo.apellido = clientes.apellido1 + " " + clientes.apellido2.
    archivo.telefono = clientes.tel_residencia.
    archivo.campoOpcional_1 = ahorros.cue_ahorros.
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
    archivo.id = SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "2" + SUBSTRING(STRING(creditos.num_credito),1,7).
    archivo.nit = creditos.nit.

    archivo.concepto = "CREDITO".

    IF creditos.cod_credito <> 123 THEN DO:
        IF creditos.fec_pago <= TODAY THEN DO:
            archivo.fecVencimiento = STRING(TODAY,"99/99/9999").

            IF creditos.cod_credito <> 108 AND creditos.cod_credito <> 113 AND creditos.cod_credito <> 114 THEN DO:
                FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                         AND CONTROL_pagos.num_credito = creditos.num_credito
                                         AND CONTROL_pagos.id_pdoMes < 2
                                         AND CONTROL_pagos.fec_Vcto <= TODAY NO-LOCK:
                    archivo.valorConIva = archivo.valorConIva + creditos.cuota.
                END.
            
                archivo.valorConIva = archivo.valorConIva + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
            END.
            ELSE DO:
                archivo.valorConIva = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
            END.
        END.
        ELSE DO:
            archivo.fecVencimiento = STRING(creditos.fec_pago,"99/99/9999").

            IF creditos.cod_credito <> 108 AND creditos.cod_credito <> 113 AND creditos.cod_credito <> 114 THEN
                archivo.valorConIva = creditos.cuota + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
            ELSE
                archivo.valorConIva = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_morCobrar + creditos.Int_MoraDifCob.
        END.
    END.
    ELSE DO:
        IF creditos.fec_pago <= TODAY THEN
            archivo.fecVencimiento = STRING(TODAY,"99/99/9999").
        ELSE
            archivo.fecVencimiento = STRING(creditos.fec_pago,"99/99/9999").

        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito
                               AND facturacion.estado = 1 NO-LOCK:
            archivo.valorConIva = archivo.valorConIva + facturacion.cuota - facturacion.pago_capital - facturacion.pago_intCorriente - facturacion.pago_intDifCobro - facturacion.pago_Mora.
        END.
    END.

    archivo.email = clientes.email.
    archivo.nombre = clientes.nombre.
    archivo.apellido = clientes.apellido1 + " " + clientes.apellido2.
    archivo.telefono = clientes.tel_residencia.
    archivo.campoOpcional_1 = string(creditos.num_credito).
END.

vNombreArchivo = "c:\INFO_Fodun\reportePSE_" + STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999") + "_" + STRING(TIME,"99999") + ".txt".

OUTPUT TO VALUE(vNombreArchivo).
FOR EACH archivo NO-LOCK:
    /*EXPORT DELIMITER "|" archivo.*/
    PUT UNFORMAT 
        archivo.id "|"
        archivo.nit "|"
        archivo.valorConIva "|"
        archivo.valorIva "|"
        archivo.concepto "|"
        archivo.fecVencimiento "|"
        archivo.email "|"
        archivo.claveParaPago "|"
        archivo.nombre "|"
        archivo.apellido "|"
        archivo.telefono "|"
        archivo.CampoOpcional_1 "|"
        archivo.CampoOpcional_2 "|"
        archivo.CampoOpcional_3
        SKIP.
END.
OUTPUT CLOSE.

CREATE recaudos_IO.
recaudos_IO.tipo_io = 'O'.
recaudos_IO.tipo_convenio = "PSE".
recaudos_IO.fecha = TODAY.
recaudos_IO.hora = TIME.
recaudos_IO.nombreArchivo = vNombreArchivo.
recaudos_IO.usuario = pUsuario.
