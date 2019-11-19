DISABLE TRIGGERS FOR LOAD OF cfg_activosFijos.

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

FOR EACH cfg_activosFijos:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = cfg_ActivosFijos.activoFijo NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        cfg_ActivosFijos.activoFijo = homologacion.cuentaNIIF.
    ELSE
        cfg_ActivosFijos.activoFijo = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = cfg_ActivosFijos.depreciacion NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        cfg_ActivosFijos.depreciacion = homologacion.cuentaNIIF.
    ELSE
        cfg_ActivosFijos.depreciacion = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = cfg_ActivosFijos.gasto NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        cfg_ActivosFijos.gasto = homologacion.cuentaNIIF.
    ELSE
        cfg_ActivosFijos.gasto = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = cfg_ActivosFijos.provisionCR NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        cfg_ActivosFijos.provisionCR = homologacion.cuentaNIIF.
    ELSE
        cfg_ActivosFijos.provisionCR = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = cfg_ActivosFijos.provisionDB NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        cfg_ActivosFijos.provisionDB = homologacion.cuentaNIIF.
    ELSE
        cfg_ActivosFijos.provisionDB = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = cfg_ActivosFijos.valorizacionCR NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        cfg_ActivosFijos.valorizacionCR = homologacion.cuentaNIIF.
    ELSE
        cfg_ActivosFijos.valorizacionCR = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = cfg_ActivosFijos.valorizacionDB NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        cfg_ActivosFijos.valorizacionDB = homologacion.cuentaNIIF.
    ELSE
        cfg_ActivosFijos.valorizacionDB = "".
    
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
