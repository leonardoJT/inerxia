DISABLE TRIGGERS FOR LOAD OF cortoLargo.

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

FOR EACH cortoLargo:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_AsoAd NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_AsoAd = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_AsoAd = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_AsoNa NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_AsoNa = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_AsoNa = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_Castigo NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_Castigo = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_Castigo = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_ContingenteCr NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_ContingenteCr = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_ContingenteCr = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_ContingenteDb NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_ContingenteDb = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_ContingenteDb = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_ContrapartidaGar NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_ContrapartidaGar = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_ContrapartidaGar = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_CostasCR NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_CostasCR = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_CostasCR = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_CostasDB NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_CostasDB = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_CostasDB = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_FutGantia NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_FutGantia = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_FutGantia = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_GarPenCancel NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_GarPenCancel = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_GarPenCancel = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_HonorariosCR NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_HonorariosCR = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_HonorariosCR = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_HonorariosDB NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_HonorariosDB = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_HonorariosDB = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_NoaAd NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_NoaAd = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_NoaAd = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_NoaNa NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_NoaNa = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_NoaNa = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_OrdCasCR NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_OrdCasCR = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_OrdCasCR = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_OrdCasDB NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_OrdCasDB = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_OrdCasDB = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_PolizasCR NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_PolizasCR = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_PolizasCR = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_PolizasDB NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_PolizasDB = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_PolizasDB = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_SYA NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_SYA = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_SYA = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_VigGarAd NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_VigGarAd = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_VigGarAd = "".

    FIND FIRST homologacion WHERE homologacion.cuentaSES = CortoLargo.Cta_VigGarNa NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN
        CortoLargo.Cta_VigGarNa = homologacion.cuentaNIIF.
    ELSE
        CortoLargo.Cta_VigGarNa = "".
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
