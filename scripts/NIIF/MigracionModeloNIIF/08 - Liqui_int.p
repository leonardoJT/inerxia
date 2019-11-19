DISABLE TRIGGERS FOR LOAD OF liqui_int.

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

FOR EACH liqui_int:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaCr_DifCob NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaCr_DifCob = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaCr_DifCob = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaCr_DifCobAso NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaCr_DifCobAso = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaCr_DifCobAso = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaCr_Liq NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaCr_Liq = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaCr_Liq = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaCr_LiqAso NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaCr_LiqAso = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaCr_LiqAso = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaCr_Mora NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaCr_Mora = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaCr_Mora = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaCr_MoraAso NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaCr_MoraAso = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaCr_MoraAso = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaCr_Ret NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaCr_Ret = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaCr_Ret = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaDb_DifCob NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaDb_DifCob = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaDb_DifCob = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaDb_DifCobAso NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaDb_DifCobAso = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaDb_DifCobAso = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaDb_Liq NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaDb_Liq = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaDb_Liq = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaDb_LiqAso NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaDb_LiqAso = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaDb_LiqAso = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaDb_Mora NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaDb_Mora = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaDb_Mora = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaDb_MoraAso NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaDb_MoraAso = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaDb_MoraAso = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaDb_Ret NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaDb_Ret = homologacion.cuentaNIIF.
    ELSE    
        Liqui_Int.CtaDb_Ret = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaInt_Ant NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaInt_Ant = homologacion.cuentaNIIF.
    ELSE    
        Liqui_Int.CtaInt_Ant = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.CtaInt_AntAso NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.CtaInt_AntAso = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.CtaInt_AntAso = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.Cta_CauCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.Cta_CauCr = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.Cta_CauCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = Liqui_Int.Cta_SucyAge NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        Liqui_Int.Cta_SucyAge = homologacion.cuentaNIIF.
    ELSE
        Liqui_Int.Cta_SucyAge = "".
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
