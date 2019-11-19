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

OUTPUT TO d:\Leonardo\NIIF\Puc_NIIF_Movimiento.csv.
FOR EACH homologa NO-LOCK:
    EXPORT DELIMITER ";" homologa.
END.
OUTPUT CLOSE.

FOR EACH homologa WHERE homologa.cuentaNIIF <> "" NO-LOCK:
    FIND FIRST cuentas WHERE cuentas.cuenta = homologa.cuenta NO-ERROR.
    IF NOT AVAILABLE cuentas THEN
        MESSAGE homologa.cuenta
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE
        cuentas.cuentaNIIF = homologa.cuentaNIIF.
END.


MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
