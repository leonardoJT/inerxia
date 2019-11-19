DISABLE TRIGGERS FOR LOAD OF pro_ahorros.

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

FOR EACH pro_ahorros:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = pro_ahorros.cta_revaloriz NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        pro_ahorros.cta_revaloriz = homologacion.cuentaNIIF.
    ELSE
        pro_ahorros.cta_revaloriz = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = pro_ahorros.cta_gtoGMF NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        pro_ahorros.cta_gtoGMF = homologacion.cuentaNIIF.
    ELSE
        pro_ahorros.cta_gtoGMF = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = pro_ahorros.cta_xpagarGMF NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        pro_ahorros.cta_xpagarGMF = homologacion.cuentaNIIF.
    ELSE
        pro_ahorros.cta_xpagarGMF = "".
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
