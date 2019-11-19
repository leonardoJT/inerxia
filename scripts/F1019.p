DEFINE TEMP-TABLE F1019
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
    FIELD tipoCuenta AS INTEGER INITIAL 1
    FIELD codExGMF AS INTEGER INITIAL 1
    FIELD sdo_final AS DECIMAL
    FIELD promSdoFinalDiario AS DECIMAL
    FIELD mediana AS DECIMAL
    FIELD sdoMaximo AS DECIMAL
    FIELD sdoMin AS DECIMAL
    FIELD totalCR AS DECIMAL
    FIELD contCR AS INTEGER
    FIELD promedioCR AS DECIMAL
    FIELD medCR AS DECIMAL
    FIELD totalDB AS DECIMAL
    FIELD contDB AS INTEGER
    FIELD promedioDB AS DECIMAL.

DEFINE TEMP-TABLE ttMed
    FIELD valor AS DECIMAL.
    
DEFINE VAR cont AS INTEGER.
DEFINE VAR saldo AS DECIMAL.
DEFINE VAR posMed AS INTEGER.
DEFINE VAR contMed AS INTEGER.
DEFINE VAR vDv AS INTEGER.

/* Enero */
FOR EACH ahorros WHERE (ahorros.tip_ahorro = 1 OR
                        ahorros.tip_ahorro = 2)
                   AND (ahorros.sdo_anuales[1] > 0 OR
                        ahorros.Sdo_AnualPerAnt[12] > 0)
                   AND ahorros.fec_apertura <= 01/31/2013
                   AND (ahorros.fec_cancelacion >= 01/01/2013 OR ahorros.fec_cancelacion = ?) NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        CREATE F1019.

        CASE clientes.tipo_identificacion:
            WHEN "R.C" THEN F1019.tipoDoc = "11".
            WHEN "T.I" THEN F1019.tipoDoc = "12".
            WHEN "C.C" THEN F1019.tipoDoc = "13".
            WHEN "T.E" THEN F1019.tipoDoc = "21".
            WHEN "C.E" THEN F1019.tipoDoc = "22".
            WHEN "NIT" THEN F1019.tipoDoc = "31".
            WHEN "PPTE" THEN F1019.tipoDoc = "41".
        END CASE.

        F1019.nit = clientes.nit.

        IF F1019.tipoDoc = "31" THEN DO:
            RUN dv (INPUT F1019.nit,
                    OUTPUT vDv).

            F1019.dv = STRING(vDv).
        END.
        ELSE
            F1019.dv = "".

        IF clientes.tipo_identificacion <> "NIT" THEN DO:
            F1019.apellido1 = clientes.apellido1.
            F1019.apellido2 = clientes.apellido2.

            IF INDEX(clientes.nombre," ") > 0 THEN DO:
                F1019.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                F1019.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
            END.
            ELSE
                F1019.nombre1 = clientes.nombre.
        END.
        ELSE
            F1019.razonSocial = clientes.nombre.

        IF clientes.DIR_residencia <> "" THEN
            F1019.direccion = clientes.DIR_residencia.
        ELSE
            F1019.direccion = clientes.DIR_comercial.

        IF F1019.direccion = "" THEN DO:
            FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
            IF AVAILABLE agencias THEN
                F1019.direccion = agencias.direccion.
        END.

        IF clientes.lugar_residencia <> "" THEN DO:
            F1019.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
            F1019.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
        END.
        ELSE DO:
            F1019.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
            F1019.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
        END.

        IF F1019.departamento = "" OR F1019.departamento = ? THEN DO:
            F1019.departamento = "05".
            F1019.municipio = "001".
        END.

        F1019.pais = "169".
        F1019.num_cuenta = ahorros.cue_ahorros.
        F1019.sdo_final = ROUND(ahorros.sdo_anuales[1],0).

        saldo = ROUND(Sdo_AnualPerAnt[12],0).
        F1019.sdoMax = saldo.
        F1019.sdoMin = saldo.

        DO cont = 1 TO 31:
            FIND LAST mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                    AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                    AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                    AND mov_ahorros.fecha = DATE(01,cont,2013) NO-LOCK NO-ERROR.
            IF AVAILABLE mov_ahorros THEN DO:
                F1019.promSdoFinalDiario = F1019.promSdoFinalDiario + ROUND(mov_ahorros.sdo_disponible,0).
                saldo = mov_ahorros.sdo_disponible.
            END.
            ELSE
                F1019.promSdoFinalDiario = F1019.promSdoFinalDiario + saldo.

            IF cont = 16 THEN
                F1019.mediana = saldo.
        END.

        F1019.promSdoFinalDiario = round(F1019.promSdoFinalDiario / 31,0).

        EMPTY TEMP-TABLE ttMed.

        FOR EACH mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                               AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                               AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                               AND mov_ahorros.fecha >= 01/01/2013
                               AND mov_ahorros.fecha <= 01/31/2013
                               /*AND mov_ahorros.cod_operacion <> 010101004*/ NO-LOCK:
            IF mov_ahorros.cod_operacion <> 010101004 THEN DO:
                FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                IF AVAILABLE operacion THEN DO:
                    IF operacion.tipo_operacion = 1 THEN DO:
                        F1019.totalCR = F1019.totalCR + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                        F1019.contCR = F1019.contCR + 1.

                        CREATE ttMed.
                        ttMed.valor = mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                    END.

                    IF operacion.tipo_operacion = 2 THEN DO:
                        F1019.totalDB = F1019.totalDB + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                        F1019.contDB = F1019.contDB + 1.
                    END.
                END.
            END.

            IF saldo > F1019.sdoMax THEN
                F1019.sdoMax = saldo.

            IF saldo < F1019.sdoMin THEN
                F1019.sdoMin = saldo.
        END.

        ASSIGN F1019.promedioCR = ROUND(F1019.totalCR / F1019.contCR,0) WHEN F1019.contCR > 0.
        ASSIGN F1019.promedioDB = ROUND(F1019.totalDB / F1019.contDB,0) WHEN F1019.contDB > 0.

        IF contCR MOD 2 = 0 THEN
            posMed = contCR / 2.
        ELSE
            posMed = TRUNCATE(contCR / 2,0) + 1.

        contMed = 0.

        FOR EACH ttMed NO-LOCK BY ttMed.valor:
            contMed = contMed + 1.

            IF contMed = posMed THEN
                F1019.medCR = ttMed.valor.
        END.
    END.
END.

OUTPUT TO C:\INFO_CUB\DIAN\F1019_1.csv.
FOR EACH F1019 NO-LOCK:
    EXPORT DELIMITER ";" F1019.
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

    EMPTY TEMP-TABLE F1019.

    FOR EACH ahorros WHERE (ahorros.tip_ahorro = 1 OR
                            ahorros.tip_ahorro = 2)
                       AND (ahorros.sdo_anuales[contMes] > 0 OR
                            ahorros.Sdo_Anuales[contMes - 1] > 0)
                       AND ahorros.fec_apertura <= fec2
                       AND (ahorros.fec_cancelacion >= fec1 OR ahorros.fec_cancelacion = ?) NO-LOCK:
        FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CREATE F1019.

            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN F1019.tipoDoc = "11".
                WHEN "T.I" THEN F1019.tipoDoc = "12".
                WHEN "C.C" THEN F1019.tipoDoc = "13".
                WHEN "T.E" THEN F1019.tipoDoc = "21".
                WHEN "C.E" THEN F1019.tipoDoc = "22".
                WHEN "NIT" THEN F1019.tipoDoc = "31".
                WHEN "PPTE" THEN F1019.tipoDoc = "41".
            END CASE.

            F1019.nit = clientes.nit.

            IF F1019.tipoDoc = "31" THEN DO:
                RUN dv (INPUT F1019.nit,
                        OUTPUT vDv).

                F1019.dv = STRING(vDv).
            END.
            ELSE
                F1019.dv = "".

            IF clientes.tipo_identificacion <> "NIT" THEN DO:
                F1019.apellido1 = clientes.apellido1.
                F1019.apellido2 = clientes.apellido2.

                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                    F1019.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                    F1019.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    F1019.nombre1 = clientes.nombre.
            END.
            ELSE
                F1019.razonSocial = clientes.nombre.

            IF clientes.DIR_residencia <> "" THEN
                F1019.direccion = clientes.DIR_residencia.
            ELSE
                F1019.direccion = clientes.DIR_comercial.

            IF F1019.direccion = "" THEN DO:
                FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                IF AVAILABLE agencias THEN
                    F1019.direccion = agencias.direccion.
            END.

            IF clientes.lugar_residencia <> "" THEN DO:
                F1019.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                F1019.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
            END.
            ELSE DO:
                F1019.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                F1019.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
            END.

            IF F1019.departamento = "" OR F1019.departamento = ? THEN DO:
                F1019.departamento = "05".
                F1019.municipio = "001".
            END.

            F1019.pais = "169".
            F1019.num_cuenta = ahorros.cue_ahorros.
            F1019.sdo_final = ROUND(ahorros.sdo_anuales[contMes],0).

            saldo = ROUND(ahorros.Sdo_Anuales[contMes - 1],0).
            F1019.sdoMax = saldo.
            F1019.sdoMin = saldo.

            DO cont = DAY(fec1) TO DAY(fec2):
                FIND LAST mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                        AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                        AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                        AND mov_ahorros.fecha = DATE(01,cont,2013) NO-LOCK NO-ERROR.
                IF AVAILABLE mov_ahorros THEN DO:
                    F1019.promSdoFinalDiario = F1019.promSdoFinalDiario + ROUND(mov_ahorros.sdo_disponible,0).
                    saldo = mov_ahorros.sdo_disponible.
                END.
                ELSE
                    F1019.promSdoFinalDiario = F1019.promSdoFinalDiario + saldo.

                IF DAY(fec2) = 28 THEN DO:
                    IF cont = 14 OR cont = 15 THEN
                        F1019.mediana = F1019.mediana + ROUND(saldo / 2,0).
                END.
                ELSE DO:
                    IF DAY(fec2) = 30 THEN DO:
                        IF cont = 15 OR cont = 16 THEN
                            F1019.mediana = F1019.mediana + ROUND(saldo / 2,0).
                        ELSE DO:
                            IF DAY(fec2) = 31 THEN DO:
                                IF cont = 16 THEN
                                    F1019.mediana = saldo.
                            END.
                        END.
                    END.
                END.
            END.

            F1019.promSdoFinalDiario = round(F1019.promSdoFinalDiario / DAY(fec2),0).

            EMPTY TEMP-TABLE ttMed.

            FOR EACH mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                   AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                                   AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                   AND mov_ahorros.fecha >= fec1
                                   AND mov_ahorros.fecha <= fec2
                                   AND mov_ahorros.cod_operacion <> 010101004 NO-LOCK:
                IF mov_ahorros.cod_operacion <> 010101004 THEN DO:
                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                    IF AVAILABLE operacion THEN DO:
                        IF operacion.tipo_operacion = 1 THEN DO:
                            F1019.totalCR = F1019.totalCR + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                            F1019.contCR = F1019.contCR + 1.

                            CREATE ttMed.
                            ttMed.valor = mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                        END.

                        IF operacion.tipo_operacion = 2 THEN DO:
                            F1019.totalDB = F1019.totalDB + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                            F1019.contDB = F1019.contDB + 1.
                        END.
                    END.
                END.

                IF saldo > F1019.sdoMax THEN
                    F1019.sdoMax = saldo.

                IF saldo < F1019.sdoMin THEN
                    F1019.sdoMin = saldo.
            END.

            ASSIGN F1019.promedioCR = ROUND(F1019.totalCR / F1019.contCR,0) WHEN F1019.contCR > 0.
            ASSIGN F1019.promedioDB = ROUND(F1019.totalDB / F1019.contDB,0) WHEN F1019.contDB > 0.

            IF contCR MOD 2 = 0 THEN
                posMed = contCR / 2.
            ELSE
                posMed = TRUNCATE(contCR / 2,0) + 1.
        
            contMed = 0.

            FOR EACH ttMed NO-LOCK BY ttMed.valor:
                contMed = contMed + 1.
                
                IF contMed = posMed THEN
                    F1019.medCR = ttMed.valor.
            END.
        END.
    END.

    OUTPUT TO VALUE("C:\Info_CUB\DIAN\F1019_" + string(contMes) + ".csv").
    FOR EACH F1019 NO-LOCK:
        EXPORT DELIMITER ";" F1019.
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

