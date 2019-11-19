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

DEFINE TEMP-TABLE ahorros
    FIELD tipo AS INTEGER
    FIELD nit AS CHARACTER
    FIELD fec_apertura AS DATE
    FIELD fec_vencimiento AS DATE
    FIELD sdo_disponible AS DECIMAL
    FIELD sdo_inicial AS DECIMAL
    FIELD fec_corte AS DATE
    FIELD cue_ahorros AS CHARACTER
    FIELD fec_cancelacion AS DATE
    FIELD monto_apertura AS DECIMAL
    FIELD tasa AS DECIMAL.

DEFINE TEMP-TABLE ahorros2
    FIELD tipo AS INTEGER
    FIELD nit AS CHARACTER
    FIELD fec_apertura AS DATE
    FIELD fec_vencimiento AS DATE
    FIELD sdo_disponible AS DECIMAL
    FIELD sdo_inicial AS DECIMAL
    FIELD fec_corte AS DATE
    FIELD cue_ahorros AS CHARACTER
    FIELD fec_cancelacion AS DATE
    FIELD monto_apertura AS DECIMAL
    FIELD tasa AS DECIMAL
    INDEX idx1 fec_corte cue_ahorros.

DEFINE TEMP-TABLE clientes
    FIELD nit AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD DIR_residencia AS CHARACTER
    FIELD tipo_id AS CHARACTER
    FIELD lugar_residencia AS INTEGER.

DEFINE TEMP-TABLE mov_ahorros
    FIELD cue_ahorros AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD fecha AS DATE
    FIELD retiro AS DECIMAL
    FIELD consignacion AS DECIMAL
    FIELD sdo_disponible AS DECIMAL
    FIELD comentario AS CHARACTER
    FIELD intereses AS DECIMAL
    INDEX idx1 nit cue_ahorros fecha.

INPUT FROM d:\Leonardo\ahorros.csv.
REPEAT:
    CREATE ahorros.
    IMPORT DELIMITER ";" ahorros.
END.
INPUT CLOSE.

INPUT FROM d:\Leonardo\ahorros.csv.
REPEAT:
    CREATE ahorros2.
    IMPORT DELIMITER ";" ahorros2.
END.
INPUT CLOSE.

INPUT FROM d:\Leonardo\clientes.csv.
REPEAT:
    CREATE clientes.
    IMPORT DELIMITER ";" clientes.
END.
INPUT CLOSE.


INPUT FROM d:\Leonardo\extractos.csv.
REPEAT:
    CREATE mov_ahorros.
    IMPORT DELIMITER ";" mov_ahorros.
END.                      
INPUT CLOSE.


/* Los demás */
DEFINE VAR contMes AS INTEGER.
DEFINE VAR fec1 AS DATE.
DEFINE VAR fec2 AS DATE.
DEFINE VAR dia1 AS INTEGER.
DEFINE VAR dia2 AS INTEGER.

DO contMes = 5 TO 12:
    fec1 = DATE(contMes,1,2014).

    CASE contMes:
        WHEN 5 THEN fec2 = 05/31/2014.
        WHEN 6 THEN fec2 = 06/30/2014.
        WHEN 7 THEN fec2 = 07/31/2014.
        WHEN 8 THEN fec2 = 08/31/2014.
        WHEN 9 THEN fec2 = 09/30/2014.
        WHEN 10 THEN fec2 = 10/31/2014.
        WHEN 11 THEN fec2 = 11/30/2014.
        WHEN 12 THEN fec2 = 12/31/2014.
    END CASE.

    EMPTY TEMP-TABLE F1020.

    FOR EACH ahorros WHERE ahorros.fec_corte = fec2
                       AND ahorros.tipo = 3
                       AND ahorros.sdo_disponible >= 0
                       AND ahorros.fec_apertura <= fec2 NO-LOCK:
        FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CREATE F1020.

            CASE clientes.tipo_id:
                WHEN "C" THEN F1020.tipoDoc = "13".
                WHEN "N" THEN F1020.tipoDoc = "31".
                WHEN "O" THEN F1020.tipoDoc = "XX".
            END CASE.

            F1020.nit = clientes.nit.

            IF F1020.tipoDoc = "31" THEN DO:
                RUN dv (INPUT F1020.nit,
                        OUTPUT vDv).

                F1020.dv = STRING(vDv).
            END.
            ELSE
                F1020.dv = "".

            IF clientes.tipo_id <> "N" THEN DO:
                F1020.apellido1 = clientes.apellido1.
                F1020.apellido2 = clientes.apellido2.
                F1020.nombre1 = clientes.nombre1.
                F1020.nombre2 = clientes.nombre2.
            END.
            ELSE
                F1020.razonSocial = clientes.razonSocial.

            F1020.direccion = clientes.DIR_residencia.
            F1020.departamento = SUBSTRING(STRING(clientes.lugar_residencia,"99999"),1,2).
            F1020.municipio = SUBSTRING(STRING(clientes.lugar_residencia,"99999"),3,3).
            F1020.pais = "169".
            F1020.num_cuenta = ahorros.cue_ahorros.

            FIND FIRST ahorros2 WHERE ahorros2.fec_corte = ADD-INTERVAL(fec2, -1, "months")
                                  AND ahorros2.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.

            ASSIGN F1020.sdo_inicial = ROUND(ahorros2.Sdo_disponible,0) WHEN AVAILABLE ahorros2.

            F1020.sdo_final = ROUND(ahorros.sdo_disponible,0).

            IF F1020.sdo_final > F1020.sdo_inicial THEN DO:
                F1020.tipoMov = 1.
                F1020.val_inversion = F1020.sdo_final - F1020.sdo_inicial.
            END.

            IF F1020.sdo_final < F1020.sdo_inicial THEN
                F1020.tipoMov = 3.
            
            IF F1020.sdo_final = F1020.sdo_inicial THEN
                F1020.tipoMov = 4.
            
            FOR EACH mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                   AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                   AND mov_ahorros.fecha >= fec1
                                   AND mov_ahorros.fecha <= fec2
                                   AND (mov_ahorros.comentario = "liquidación de interes en cuenta por pagar"OR
                                        mov_ahorros.comentario = "Abono liquidación de interes") NO-LOCK:
                F1020.INT_pagado = F1020.INT_pagado + mov_ahorros.consignacion + mov_ahorros.interes.
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
    
    OUTPUT TO VALUE("C:\Info_Fodun\F1020_" + string(contMes) + ".csv").
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

