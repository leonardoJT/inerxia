DISABLE TRIGGERS FOR LOAD OF carteraVencida.

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

FOR EACH carteraVencida:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.CtaCal_Costas NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.CtaCal_Costas = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.CtaCal_Costas = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.CtaCal_Interes NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.CtaCal_Interes = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.CtaCal_Interes = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.CtaGto_ProvCos NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.CtaGto_ProvCos = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.CtaGto_ProvCos = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.CtaGto_ProvInt NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.CtaGto_ProvInt = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.CtaGto_ProvInt = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.CtaIng_provCos NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.CtaIng_provCos = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.CtaIng_provCos = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.CtaIng_ProvInt NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.CtaIng_ProvInt = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.CtaIng_ProvInt = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoAdDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoAdDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoAdDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoIntAdCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoIntAdCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoIntAdCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoIntAdDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoIntAdDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoIntAdDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoIntNaCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoIntNaCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoIntNaCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoIntNaDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoIntNaDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoIntNaDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoNaDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoNaDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoNaDb = "".
    
    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoPrvAdCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoPrvAdCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoPrvAdCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoPrvAdDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoPrvAdDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoPrvAdDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoPrvNaCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoPrvNaCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoPrvNaCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_AsoPrvNaDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_AsoPrvNaDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_AsoPrvNaDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_CostasCR NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_CostasCR = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_CostasCR = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_CostasDB NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_CostasDB = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_CostasDB = "".
    
    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_IntContingCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_IntContingCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_IntContingCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_IntContingDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_IntContingDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_IntContingDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaAdDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaAdDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaAdDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaIntAdCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaIntAdCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaIntAdCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaIntAdDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaIntAdDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaIntAdDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaIntNaCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaIntNaCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaIntNaCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaIntNaDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaIntNaDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaIntNaDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaNaDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaNaDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaNaDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaPrvAdCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaPrvAdCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaPrvAdCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaPrvAdDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaPrvAdDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaPrvAdDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaPrvNaCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaPrvNaCr = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaPrvNaCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CarteraVencida.Cta_NoaPrvNaDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CarteraVencida.Cta_NoaPrvNaDb = homologacion.cuentaNIIF.
    ELSE
        CarteraVencida.Cta_NoaPrvNaDb = "".
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
