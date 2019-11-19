/*
    exportar creditos que no tienen plan pagos bien creado.
    desembolsos de nov y dic, que hicieron pagos en enero.
    a los resultantes se les debe modificar creditos.cuo_pagada y generarles nuevamente plan pagos.
*/

DEFINE VARIABLE vi AS INTEGER     NO-UNDO.
OUTPUT TO "c:\creditos1.csv".

PUT "nit;cod_credito;num_credito;fec_desembolso;fecUltimo_pago;cuotasPagadas;proxPago;numeroCuota" SKIP.
FOR EACH planPagos WHERE 
        MONTH(PlanPagos.Fec_Inic) EQ 12 AND 
        YEAR(PlanPagos.Fec_Inic) EQ 2010 AND
        /*PlanPagos.Fec_ProxPago PlanPagos.Fec_Vcto */
    TRUE NO-LOCK:

    FIND FIRST creditos WHERE creditos.nit EQ planPagos.nit AND creditos.num_credito EQ planPagos.num_credito NO-LOCK NO-ERROR.
    IF creditos.plazo GT 1 AND creditos.fec_desembolso GE 11/01/2010 THEN DO:
        ASSIGN vi = vi + 1.
        EXPORT DELIMITER ";" 
            creditos.nit
            creditos.cod_credito
            creditos.num_credito
            creditos.fec_desembolso
            Creditos.Fec_UltPago 
            Creditos.Cuo_Pagadas
            PlanPagos.Fec_ProxPago 
            PlanPagos.Nro_Cuota
            .
/*         DISPLAY planPagos WITH 1 COL. */    
    END.
END.
OUTPUT CLOSE.

MESSAGE vi
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
