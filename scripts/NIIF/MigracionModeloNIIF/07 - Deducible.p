DISABLE TRIGGERS FOR LOAD OF deducible.

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

FOR EACH deducible:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = Deducible.Cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Deducible.Cuenta = homologacion.cuentaNIIF.
    ELSE
        Deducible.Cuenta = "".
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
