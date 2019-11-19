DISABLE TRIGGERS FOR LOAD OF base_ret.

DEFINE TEMP-TABLE homologacion
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreNIIF AS CHARACTER
    FIELD cuentaSES AS CHARACTER
    FIELD nombreSES AS CHARACTER.

INPUT FROM d:\Leonardo\CatalogoNIIF_vs_SES.csv.
REPEAT:
    CREATE homologacion.
    IMPORT DELIMITER ";" homologacion.
END.
INPUT CLOSE.

FOR EACH base_ret WHERE base_ret.estado = 1:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = base_ret.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        base_ret.cuenta = homologacion.cuentaNIIF.
    ELSE
        base_ret.estado = 2.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
