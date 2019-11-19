DEFINE TEMP-TABLE tt
    FIELD cod_producto AS INTEGER
    FIELD nombre AS CHARACTER
    FIELD cuenta_k_C AS CHARACTER
    FIELD cuenta_k_N AS CHARACTER
    FIELD intCau AS CHARACTER
    FIELD intXpagarC AS CHARACTER
    FIELD cuentaGastoC AS CHARACTER
    FIELD intXpagarN AS CHARACTER
    FIELD cuentaGastoN AS CHARACTER.

FOR EACH pro_ahorros WHERE pro_ahorros.estado = 1 NO-LOCK:
    CREATE tt.
    tt.cod_producto = pro_ahorros.cod_ahorro.
    tt.nombre = pro_ahorros.nom_producto.

    FOR EACH cortoLargo WHERE cortoLargo.Cod_Producto = pro_ahorros.cod_ahorro
                          AND cortoLargo.agencia = 1 NO-LOCK:
        tt.cuenta_K_C = Cta_NoaAd.
        tt.cuenta_k_N = Cta_AsoAd.
        /*Cta_AsoNa Cta_Castigo Cta_ContingenteCr Cta_ContingenteDb Cta_ContrapartidaGar Cta_CostasCR Cta_CostasDB Cta_FutGantia Cta_GarPenCancel Cta_HonorariosCR Cta_HonorariosDB  Cta_NoaNa Cta_OrdCasCR Cta_OrdCasDB Cta_PolizasCR Cta_PolizasDB Cta_SYA Cta_VigGarAd Cta_VigGarNa Plazo_Final Plazo_Inicial*/
    END.

    FOR EACH liqui_int WHERE liqui_int.clase_producto = 1
                         AND liqui_int.cod_producto = pro_ahorros.cod_ahorro NO-LOCK:
        tt.intCau = liqui_int.Cta_CauCr.
        tt.intXpagarC = liqui_int.CtaCr_Liq.
        tt.cuentaGastoC = liqui_int.CtaDb_Liq.
        tt.intXpagarN = liqui_int.CtaCr_LiqAso.
        tt.cuentaGastoN = liqui_int.CtaDb_LiqAso.

        /*CtaCr_DifCob CtaCr_DifCobAso CtaCr_Mora CtaCr_MoraAso CtaCr_Ret CtaDb_DifCob CtaDb_DifCobAso  CtaDb_Mora CtaDb_MoraAso CtaDb_Ret CtaInt_Ant CtaInt_AntAso Cta_SucyAge*/
    END.
END.

FOR EACH tt NO-LOCK:
    DISPLAY tt WITH 1 COL.
END.

OUTPUT TO d:\Leonardo\ConfAhorros.csv.
EXPORT DELIMITER ";" "COD_AHORRO" "NOMBRE_PRODUCTO" "CAPITAL_CAJA" "CAPITAL_NOMINA" "INTERES_CAUSADO" "INTERESxPAGAR_CAJA" "GASTO_CAJA" "INTERESxPAGAR_NOMINA" "GASTO_NOMINA".
FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.
