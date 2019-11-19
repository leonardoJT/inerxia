DISABLE TRIGGERS FOR LOAD OF operacion.

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

FOR EACH operacion:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = operacion.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        operacion.cuenta = homologacion.cuentaNIIF.
    ELSE
        operacion.cuenta = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = operacion.cta_SyA NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        operacion.cta_SyA = homologacion.cuentaNIIF.
    ELSE
        operacion.cta_SyA = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = operacion.cta_gtoGMF NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        operacion.cta_gtoGMF = homologacion.cuentaNIIF.
    ELSE
        operacion.cta_gtoGMF = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = operacion.Cta_XPagarGMF NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        operacion.Cta_XPagarGMF = homologacion.cuentaNIIF.
    ELSE
        operacion.Cta_XPagarGMF = "".
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
