DEFINE TEMP-TABLE tt
    FIELD cod_producto AS INTEGER
    FIELD nombre AS CHARACTER
    FIELD calificacion AS CHARACTER
    FIELD cuenta_k_C AS CHARACTER
    FIELD cuenta_k_N AS CHARACTER
    FIELD int_corriente AS CHARACTER
    FIELD cuentaIngresoC AS CHARACTER
    FIELD cuentaIngresoN AS CHARACTER
    FIELD INT_mora AS CHARACTER
    FIELD cuentaIngresoMoraC AS CHARACTER
    FIELD cuentaIngresoMoraN AS CHARACTER
    FIELD intDifCobro_DB AS CHARACTER
    FIELD intDifCobro_CR AS CHARACTER
    FIELD provisionK AS CHARACTER
    FIELD gastoProvisionK AS CHARACTER
    FIELD provisionI AS CHARACTER
    FIELD gastoProvisionI AS CHARACTER.

   

FOR EACH pro_creditos WHERE pro_creditos.estado = 1 NO-LOCK:
    FOR EACH carteraVencida WHERE carteraVencida.Cod_producto = pro_creditos.cod_credito NO-LOCK:
        CREATE tt.
        tt.cod_producto = pro_creditos.cod_credito.
        tt.nombre = pro_creditos.nom_producto.
        tt.calificacion = carteraVencida.Categoria.
        tt.cuenta_K_C = carteraVencida.Cta_NoaAdDb.
        tt.cuenta_K_N = carteraVencida.Cta_AsoAdDb.
        tt.INT_corriente = carteraVencida.CtaCal_Interes.

        FIND FIRST liqui_int WHERE liqui_int.clase_producto = 2 AND liqui_int.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.
        IF AVAILABLE liqui_int THEN DO:
            tt.cuentaIngresoC = liqui_int.CtaCr_Liq.
            tt.cuentaIngresoN = liqui_int.CtaCr_LiqAso.
            tt.INT_mora = liqui_int.CtaDb_Mora.
            tt.cuentaIngresoMoraC = liqui_int.CtaCr_Mora.
            tt.cuentaIngresoMoraN = liqui_int.CtaCr_MoraAso.
        END.

        tt.intDifCobro_DB = carteraVencida.Cta_IntContingDb.
        tt.intDifCobro_CR = carteraVencida.Cta_IntContingCr.
        tt.provisionK = carteraVencida.Cta_AsoPrvAdCr.
        tt.gastoProvisionK = carteraVencida.Cta_AsoPrvAdDb.
        tt.provisionI = carteraVencida.Cta_AsoIntAdDb.
        tt.gastoProvisionI = carteraVencida.Cta_AsoIntAdCr.
        
        /*
        
        
        Cta_AsoIntNaCr
        Cta_AsoIntNaDb
        Cta_NoaIntAdCr
        Cta_NoaIntAdDb
        Cta_NoaIntNaCr
        Cta_NoaIntNaDb
        Cta_NoaNaDb
        */
    END.
END.

FOR EACH tt NO-LOCK:
    DISPLAY tt WITH 1 COL.
END.

OUTPUT TO d:\Leonardo\ConfCreditos.csv.
EXPORT DELIMITER ";"
    "COD_CREDITO"
    "NOMBRE_PRODUCTO"
    "CALIFICACION"
    "CAPITAL_CAJA"
    "CAPITAL_NOMINA"
    "INTERES_CORRIENTE"
    "INGRESO_CAJA"
    "INGRESO_NOMINA"
    "INT_MORA"
    "ING_MORA_CAJA"
    "INGRESO_MORA_NOMINA"
    "INT_DIFCOBRO_DB"
    "INT_DIFCOBRO_CR"
    "PROVISION_K"
    "GASTO_PROVISION_K"
    "PROVISION_I"
    "GASTO_PROVISION_I".

FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.
