/* Homologar cada una de las cuenats de acuerdo al archivo entregado por Rafael */
DISABLE TRIGGERS FOR LOAD OF cuentas.
DISABLE TRIGGERS FOR LOAD OF cuentas_NIIF.

/* 1. Importamos y dejamos solo las de movimiento */
DEFINE TEMP-TABLE homologa
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD cuentaNiif AS CHARACTER
    FIELD nombreNiif AS CHARACTER.

INPUT FROM d:\Leonardo\niif\puc_niif.csv.
REPEAT:
    CREATE homologa.
    IMPORT DELIMITER ";" homologa.
END.
INPUT CLOSE.

FOR EACH homologa:
    IF homologa.cuenta = "" THEN
        DELETE homologa.
    ELSE DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = homologa.cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE cuentas THEN DO:
            IF cuentas.tipo <> 2 THEN
                DELETE homologa.
        END.
        ELSE DO:
            MESSAGE homologa.cuenta
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
END.

OUTPUT TO d:\Leonardo\niif\SinHomologar.csv.
FOR EACH homologa NO-LOCK:
    FIND FIRST cuentas_NIIF WHERE cuentas_NIIF.cuenta = homologa.cuentaNIIF
                              AND cuentas_NIIF.tipo = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas_NIIF THEN
        NEXT.

    EXPORT DELIMITER ";"
        homologa.
END.
OUTPUT CLOSE.
