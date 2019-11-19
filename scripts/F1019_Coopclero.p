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

DEFINE TEMP-TABLE ahorros
    FIELD tipo AS INTEGER
    FIELD nit AS CHARACTER
    FIELD fec_apertura AS DATE
    FIELD fec_vencimiento AS DATE
    FIELD sdo_disponible AS DECIMAL
    FIELD sdo_inicial AS DECIMAL
    FIELD fec_corte AS DATE
    FIELD cue_ahorros AS CHARACTER.

DEFINE TEMP-TABLE ahorros2
    FIELD tipo AS INTEGER
    FIELD nit AS CHARACTER
    FIELD fec_apertura AS DATE
    FIELD fec_vencimiento AS DATE
    FIELD sdo_disponible AS DECIMAL
    FIELD sdo_inicial AS DECIMAL
    FIELD fec_corte AS DATE
    FIELD cue_ahorros AS CHARACTER
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

    EMPTY TEMP-TABLE F1019.

    FOR EACH ahorros WHERE ahorros.fec_corte = fec2
                       AND ahorros.tipo = 1
                       AND ahorros.sdo_disponible >= 0
                       AND ahorros.fec_apertura <= fec2 NO-LOCK:
        FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CREATE F1019.

            CASE clientes.tipo_id:
                WHEN "C" THEN F1019.tipoDoc = "13".
                WHEN "N" THEN F1019.tipoDoc = "31".
                WHEN "O" THEN F1019.tipoDoc = "XX".
            END CASE.

            F1019.nit = clientes.nit.

            IF F1019.tipoDoc = "31" THEN DO:
                RUN dv (INPUT F1019.nit,
                        OUTPUT vDv).

                F1019.dv = STRING(vDv).
            END.
            ELSE
                F1019.dv = "".

            IF clientes.tipo_id <> "N" THEN DO:
                F1019.apellido1 = clientes.apellido1.
                F1019.apellido2 = clientes.apellido2.
                F1019.nombre1 = clientes.nombre1.
                F1019.nombre2 = clientes.nombre2.
            END.
            ELSE
                F1019.razonSocial = clientes.razonSocial.

            F1019.direccion = clientes.DIR_residencia.
            F1019.departamento = SUBSTRING(STRING(clientes.lugar_residencia,"99999"),1,2).
            F1019.municipio = SUBSTRING(STRING(clientes.lugar_residencia,"99999"),3,3).
            F1019.pais = "169".
            F1019.num_cuenta = ahorros.cue_ahorros.
            F1019.sdo_final = ROUND(ahorros.sdo_disponible,0).

            saldo = 0.

            FIND FIRST ahorros2 WHERE ahorros2.fec_corte = ADD-INTERVAL(fec2, -1, "months")
                                  AND ahorros2.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.

            ASSIGN saldo = ROUND(ahorros2.Sdo_disponible,0) WHEN AVAILABLE ahorros2.
            F1019.sdoMax = saldo.
            F1019.sdoMin = saldo.
            
            DO cont = DAY(fec1) TO DAY(fec2):
                FIND LAST mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                                        AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                        AND mov_ahorros.fecha = DATE(01,cont,2014) NO-LOCK NO-ERROR.
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
                                   AND mov_ahorros.cue_ahorros = ahorros.cue_ahorros
                                   AND mov_ahorros.fecha >= fec1
                                   AND mov_ahorros.fecha <= fec2
                                   AND mov_ahorros.comentario <> "liquidación de interes en cuenta por pagar"
                                   AND mov_ahorros.comentario <> "Abono liquidación de interes"
                                   AND mov_ahorros.consignacion + mov_ahorros.retiro > 0 NO-LOCK:
                IF mov_ahorros.consignacion > 0 THEN DO:
                    F1019.totalCR = F1019.totalCR + mov_ahorros.consignacion.
                    F1019.contCR = F1019.contCR + 1.

                    CREATE ttMed.
                    ttMed.valor = mov_ahorros.consignacion.
                END.

                IF mov_ahorros.retiro > 0 THEN DO:
                    F1019.totalDB = F1019.totalDB + mov_ahorros.retiro.
                    F1019.contDB = F1019.contDB + 1.
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

    OUTPUT TO VALUE("C:\Info_Fodun\\F1019_" + string(contMes) + ".csv").
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

