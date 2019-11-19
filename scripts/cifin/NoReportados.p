DEFINE TEMP-TABLE cifin
    FIELD tipo_id AS INTEGER
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER
    FIELD agencia AS INTEGER
    FIELD estado AS INTEGER
    FIELD calificacion AS CHARACTER
    FIELD edadMora AS INTEGER
    FIELD linea AS INTEGER
    FIELD saldo AS DECIMAL
    FIELD saldoMora AS DECIMAL
    FIELD fechaTerminacion AS CHARACTER
    FIELD fechaCorte AS CHARACTER
    FIELD fechaCancelacion AS CHARACTER
    FIELD resuelve AS CHARACTER.

INPUT FROM d:\Leonardo\noReportados.csv.
REPEAT :
    CREATE cifin.
    IMPORT DELIMITER ";" cifin.
END.
INPUT CLOSE.

FOR EACH cifin:
    FIND FIRST creditos WHERE creditos.nit = cifin.nit
                          AND creditos.num_credito = cifin.num_credito
                          AND creditos.estado = 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE creditos THEN
        FIND FIRST creditos WHERE creditos.nit = cifin.nit
                      AND creditos.num_credito = cifin.num_credito + 9900000
                      AND creditos.estado = 2 NO-LOCK NO-ERROR.

    IF NOT AVAILABLE creditos THEN DO:
        cifin.resuelve = "CANCELADO".

        FIND FIRST creditos WHERE creditos.nit = cifin.nit
                              AND creditos.num_credito = cifin.num_credito NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN DO:
            IF creditos.fec_canceTotal <> ? THEN
                cifin.fechaCancelacion = string(creditos.fec_canceTotal,"99/99/9999").
            ELSE DO:
                IF creditos.fec_ultPago <> ? THEN
                    cifin.fechaCancelacion = string(creditos.fec_ultPago,"99/99/9999").
                ELSE DO:
                    FOR EACH mov_creditos WHERE mov_creditos.nit = creditos.nit
                                            AND mov_creditos.num_credito = creditos.num_credito BY mov_creditos.fecha DESC:
                        cifin.fechaCancelacion = string(mov_creditos.fecha,"99/99/9999").
                        LEAVE.
                    END.
                END.
            END.
        END.
        ELSE
            DISPLAY cifin WITH 1 COL.

        IF cifin.fechaCancelacion = "" THEN
            cifin.fechaCancelacion = fechaCorte.
    END.
    ELSE DO:
        IF creditos.agencia <> cifin.agencia THEN
            cifin.resuelve = "TRASLADO DE AGENCIA".
    END.
END.

OUTPUT TO d:\Leonardo\cifin.csv.
EXPORT DELIMITER ";" "TIPO_ID" "NIT" "NUM_CREDITO" "AGENCIA" "ESTADO" "CALIFICACION" "EDAD_MORA" "LINEA" "SALDO" "SALDO_MORA" "FECHA_TERMINACION" "FECHA_CORTE" "FECHA_CANCELACION" "RESUELVE".
FOR EACH cifin NO-LOCK:
    EXPORT DELIMITER ";" cifin.
END.
OUTPUT CLOSE.
