DEFINE TEMP-TABLE F1020
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD num_cuenta AS CHARACTER
    FIELD tipoTitulo AS INTEGER INITIAL 3
    FIELD tipoMov AS INTEGER
    FIELD sdo_inicial AS DECIMAL
    FIELD val_inversion AS DECIMAL
    FIELD INT_causado AS DECIMAL
    FIELD INT_pagado AS DECIMAL
    FIELD sdo_final AS DECIMAL.

DEFINE VAR cont AS INTEGER.
DEFINE VAR dias AS INTEGER.
DEFINE VAR vDv AS INTEGER.

/* Enero */
FOR EACH ahorros WHERE ahorros.tip_ahorro = 3
                   AND (ahorros.sdo_anuales[1] > 0 OR
                        ahorros.Sdo_AnualPerAnt[12] > 0)
                   AND ahorros.fec_apertura <= 01/31/2013
                   AND (ahorros.fec_cancelacion >= 01/01/2013 OR ahorros.fec_cancelacion = ?) NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        CREATE F1020.

        CASE clientes.tipo_identificacion:
            WHEN "R.C" THEN F1020.tipoDoc = "11".
            WHEN "T.I" THEN F1020.tipoDoc = "12".
            WHEN "C.C" THEN F1020.tipoDoc = "13".
            WHEN "T.E" THEN F1020.tipoDoc = "21".
            WHEN "C.E" THEN F1020.tipoDoc = "22".
            WHEN "NIT" THEN F1020.tipoDoc = "31".
            WHEN "PPTE" THEN F1020.tipoDoc = "41".
        END CASE.

        F1020.nit = clientes.nit.

        IF F1020.tipoDoc = "31" THEN DO:
            RUN dv (INPUT F1020.nit,
                    OUTPUT vDv).

            F1020.dv = STRING(vDv).
        END.
        ELSE
            F1020.dv = "".


        IF clientes.tipo_identificacion <> "NIT" THEN DO:
            F1020.apellido1 = clientes.apellido1.
            F1020.apellido2 = clientes.apellido2.

            IF INDEX(clientes.nombre," ") > 0 THEN DO:
                F1020.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                F1020.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
            END.
            ELSE
                F1020.nombre1 = clientes.nombre.
        END.
        ELSE
            F1020.razonSocial = clientes.nombre.

        IF clientes.DIR_residencia <> "" THEN
            F1020.direccion = clientes.DIR_residencia.
        ELSE
            F1020.direccion = clientes.DIR_comercial.

        IF F1020.direccion = "" THEN DO:
            FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
            IF AVAILABLE agencias THEN
                F1020.direccion = agencias.direccion.
        END.

        IF clientes.lugar_residencia <> "" THEN DO:
            F1020.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
            F1020.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
        END.
        ELSE DO:
            F1020.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
            F1020.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
        END.

        IF F1020.departamento = "" OR F1020.departamento = ? THEN DO:
            F1020.departamento = "05".
            F1020.municipio = "001".
        END.

        F1020.pais = "169".
        F1020.num_cuenta = ahorros.cue_ahorros.
        F1020.sdo_inicial = ahorros.Sdo_AnualPerAnt[12].
        F1020.sdo_final = ROUND(ahorros.sdo_anuales[1],0).

        IF F1020.sdo_final > F1020.sdo_inicial THEN DO:
            IF F1020.sdo_inicial = 0 THEN
                F1020.tipoMov = 1.
            ELSE
                F1020.tipoMov = 2.

            F1020.val_inversion = F1020.sdo_final - F1020.sdo_inicial.
        END.

        IF F1020.sdo_final < F1020.sdo_inicial THEN DO:
            F1020.tipoMov = 3.
        END.

        IF F1020.sdo_final = F1020.sdo_inicial THEN DO:
            F1020.tipoMov = 4.
        END.

        /* oakley */

        FOR EACH mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                               AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                               AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                               AND mov_ahorros.fecha >= 01/01/2013
                               AND mov_ahorros.fecha <= 01/31/2013
                               AND mov_ahorros.cod_operacion = 010101004 NO-LOCK:
            F1020.INT_pagado = F1020.INT_pagado + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
        END.

        IF ahorros.fec_apertura >= 01/01/2013 AND ahorros.fec_apertura <= 01/31/2013 THEN
            dias = 31 - DAY(ahorros.fec_apertura) + 1.

        IF ahorros.fec_cancelacion >= 01/01/2013 AND ahorros.fec_cancelacion <= 01/31/2013 THEN
            dias = DAY(ahorros.fec_cancelacion).

        IF ahorros.fec_apertura <= 12/31/2012 AND ahorros.fec_cancelacion >= 02/01/2013 THEN
            dias = 31.

        F1020.INT_causado = (ahorros.monto_apertura) * (ahorros.tasa / 100 / 360) * dias.
    END.
END.

OUTPUT TO C:\INFO_CUB\DIAN\F1020_1.csv.
FOR EACH F1020 NO-LOCK:
    EXPORT DELIMITER ";" F1020.
END.
OUTPUT CLOSE.

/* Los demás */
DEFINE VAR contMes AS INTEGER.
DEFINE VAR fec1 AS DATE.
DEFINE VAR fec2 AS DATE.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.

DO contMes = 2 TO 12:
    fec1 = DATE(contMes,1,2013).

    CASE contMes:
        WHEN 2 THEN fec2 = 02/28/2013.
        WHEN 3 THEN fec2 = 03/31/2013.
        WHEN 4 THEN fec2 = 04/30/2013.
        WHEN 5 THEN fec2 = 05/31/2013.
        WHEN 6 THEN fec2 = 06/30/2013.
        WHEN 7 THEN fec2 = 07/31/2013.
        WHEN 8 THEN fec2 = 08/31/2013.
        WHEN 9 THEN fec2 = 09/30/2013.
        WHEN 10 THEN fec2 = 10/31/2013.
        WHEN 11 THEN fec2 = 11/30/2013.
        WHEN 12 THEN fec2 = 12/31/2013.
    END CASE.

    EMPTY TEMP-TABLE F1020.

    FOR EACH ahorros WHERE ahorros.tip_ahorro = 3
                       AND (ahorros.sdo_anuales[contMes] > 0 OR
                            ahorros.Sdo_Anuales[contMes - 1] > 0)
                       AND ahorros.fec_apertura <= fec2
                       AND (ahorros.fec_cancelacion >= fec1 OR ahorros.fec_cancelacion = ?) NO-LOCK:
        FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CREATE F1020.

            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN F1020.tipoDoc = "11".
                WHEN "T.I" THEN F1020.tipoDoc = "12".
                WHEN "C.C" THEN F1020.tipoDoc = "13".
                WHEN "T.E" THEN F1020.tipoDoc = "21".
                WHEN "C.E" THEN F1020.tipoDoc = "22".
                WHEN "NIT" THEN F1020.tipoDoc = "31".
                WHEN "PPTE" THEN F1020.tipoDoc = "41".
            END CASE.

            F1020.nit = clientes.nit.

            IF F1020.tipoDoc = "31" THEN DO:
                RUN dv (INPUT F1020.nit,
                        OUTPUT vDv).

                F1020.dv = STRING(vDv).
            END.
            ELSE
                F1020.dv = "".

            IF clientes.tipo_identificacion <> "NIT" THEN DO:
                F1020.apellido1 = clientes.apellido1.
                F1020.apellido2 = clientes.apellido2.

                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                    F1020.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                    F1020.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    F1020.nombre1 = clientes.nombre.
            END.
            ELSE
                F1020.razonSocial = clientes.nombre.

            IF clientes.DIR_residencia <> "" THEN
                F1020.direccion = clientes.DIR_residencia.
            ELSE
                F1020.direccion = clientes.DIR_comercial.

            IF F1020.direccion = "" THEN DO:
                FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                IF AVAILABLE agencias THEN
                    F1020.direccion = agencias.direccion.
            END.

            IF clientes.lugar_residencia <> "" THEN DO:
                F1020.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                F1020.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
            END.
            ELSE DO:
                F1020.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                F1020.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
            END.

            IF F1020.departamento = "" OR F1020.departamento = ? THEN DO:
                F1020.departamento = "05".
                F1020.municipio = "001".
            END.

            F1020.pais = "169".
            F1020.num_cuenta = ahorros.cue_ahorros.
            F1020.sdo_inicial = ROUND(ahorros.Sdo_AnualPerAnt[contMes - 1],0).
            F1020.sdo_final = ROUND(ahorros.sdo_anuales[contMes],0).

            IF F1020.sdo_final > F1020.sdo_inicial THEN DO:
                F1020.tipoMov = 1.
                F1020.val_inversion = F1020.sdo_final - F1020.sdo_inicial.
            END.

            IF F1020.sdo_final < F1020.sdo_inicial THEN
                F1020.tipoMov = 3.
            
            IF F1020.sdo_final = F1020.sdo_inicial THEN
                F1020.tipoMov = 4.
            
            FOR EACH mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                   AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                   AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                   AND mov_ahorros.fecha >= fec1
                                   AND mov_ahorros.fecha <= fec2
                                   AND mov_ahorros.cod_operacion = 010101004 NO-LOCK:
                F1020.INT_pagado = F1020.INT_pagado + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
            END.

            IF ahorros.fec_apertura >= fec1 AND ahorros.fec_apertura <= fec2 THEN
                dias = 31 - DAY(ahorros.fec_apertura) + 1.

            IF ahorros.fec_cancelacion >= fec1 AND ahorros.fec_cancelacion <= fec2 THEN
                dias = DAY(ahorros.fec_cancelacion).

            IF ahorros.fec_apertura <= fec1 - 1 AND (ahorros.fec_cancelacion >= fec2 + 1 OR ahorros.fec_cancelacion = ?) THEN
                dias = 31.

            F1020.INT_causado = (ahorros.monto_apertura) * (ahorros.tasa / 100 / 360) * dias.
        END.
    END.
    
    OUTPUT TO VALUE("C:\Info_CUB\DIAN\F1020_" + string(contMes) + ".csv").
    FOR EACH F1020 NO-LOCK:
        EXPORT DELIMITER ";" F1020.
    END.
    OUTPUT CLOSE.
END.


MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

PROCEDURE dv:
    DEFINE INPUT PARAMETER numDoc AS CHARACTER.
    DEFINE OUTPUT PARAMETER dv AS INTEGER.

    DEFINE VAR primos AS INTEGER EXTENT 15 INITIAL [71, 67, 59, 53, 47, 43, 41, 37, 29, 23, 19, 17, 13, 7, 3].
    DEFINE VAR suma AS INTEGER.
    DEFINE VAR cont AS INTEGER.

    numDoc = STRING(INTEGER(numDoc),"999999999999999").

    DO cont = 1 TO 15:
        suma = suma + (INTEGER(SUBSTRING(numDoc,cont,1)) * primos[cont]).
    END.

    IF suma MOD 11 = 0 OR suma MOD 11 = 1 THEN
        dv = suma MOD 11.
    ELSE
        dv = 11 - (suma MOD 11).
END.

