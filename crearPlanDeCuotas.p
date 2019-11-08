DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pNumCredito AS INTEGER.
DEFINE INPUT PARAMETER pFecha AS DATE. /* Para eliminar - Cambiar por w_fecha */
DEFINE INPUT PARAMETER pNovedad AS CHARACTER.
DEFINE INPUT PARAMETER pGrabar AS INTEGER. /* 0 - No (Simular, devuelve el plazo), 1 - Sí */
DEFINE OUTPUT PARAMETER pPlazo AS INTEGER.

DEFINE SHARED VAR W_ManFin AS HANDLE.

DEFINE TEMP-TABLE ttAmortizacion LIKE amortizacion.

DEFINE VAR vSaldo AS DECIMAL.
DEFINE VAR fecDesembolso AS DATE.
DEFINE VAR fecInicial AS DATE.
DEFINE VAR tasaPeriodica AS DECIMAL.
DEFINE VAR saldoCapitalProyectado AS DECIMAL.
DEFINE VAR nroPeriodo AS INTEGER.
DEFINE VAR diasPeriodo AS INTEGER.
DEFINE VAR periodosYear AS INTEGER.

/* oakley --- */

FIND FIRST creditos WHERE creditos.nit = pNit
                      AND creditos.num_credito = pNumCredito NO-LOCK NO-ERROR.
IF AVAILABLE creditos THEN DO:
    IF pGrabar = 1 THEN DO:
        FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                                AND amortizacion.num_credito = creditos.num_credito
                                AND amortizacion.fec_pago >= pFecha:
            DELETE amortizacion.
        END.
    END.

    RUN crearPlanAmortizacion.
END.

PROCEDURE crearPlanAmortizacion:
    DEFINE VAR W_FecTra AS DATE.
    DEFINE VAR W_VlrExt AS DECIMAL.
    DEFINE VAR i AS INTEGER.

    RUN HallarPeriodo (INPUT Creditos.Per_Pago,
                       INPUT Creditos.Plazo,
                       OUTPUT diasPeriodo,
                       OUTPUT periodosYear).

    IF pGrabar = 1 THEN
        vSaldo = creditos.monto.
    ELSE
        vSaldo = creditos.sdo_capital.

    fecDesembolso = Creditos.fec_Desembolso.

    IF pGrabar = 1 THEN DO:
        FIND FIRST mov_creditos WHERE mov_creditos.nit = creditos.nit
                                  AND mov_creditos.num_credito = creditos.num_credito
                                  AND mov_creditos.fecha >= creditos.fec_desembolso
                                  AND mov_creditos.cod_operacion = 999999999 NO-LOCK NO-ERROR.
        IF AVAILABLE mov_creditos THEN
            fecDesembolso = creditos.fec_pagAnti - diasPeriodo.
    END.
    ELSE
        fecDesembolso = pFecha - diasPeriodo.

    IF pGrabar = 1 THEN DO:
        IF Creditos.Fec_PagAnti <> ? THEN
            fecInicial = Creditos.Fec_PagAnti.
        ELSE
            fecInicial = Creditos.Fec_desembolso.
    END.
    ELSE
        fecInicial = pFecha.

    tasaPeriodica = Creditos.Tasa / (periodosYear * 100).

    CREATE ttAmortizacion.
    ttAmortizacion.nit = creditos.nit.
    ttAmortizacion.num_credito = creditos.num_credito.
    ttAmortizacion.sdo_Capital = vSaldo.

    /*oakley - Desarrollo de simulación para obtener el plazo */

    W_FecTra = fecInicial.
    saldoCapitalProyectado = vSaldo.

    DO nroPeriodo = 0 TO creditos.plazo BY 1:
        IF nroPeriodo > 0 THEN DO:
            IF nroPeriodo > 1 THEN
                RUN Halla_FecVcto.R (INPUT fecInicial,
                                     INPUT diasPeriodo,
                                     INPUT W_FecTra,
                                     OUTPUT W_FecTra).

            amortizacion.fec_pago = W_FecTra.
        END.

        amortizacion.cuota = creditos.cuota.
        amortizacion.nro_cuota = nroPeriodo.

        IF (nroPeriodo = 0) THEN DO:
            amortizacion.fec_pago = pFecha.
            amortizacion.novedad = pNovedad.
        END.
        ELSE
            RUN P-AmortCuotaFija.

        IF nroPeriodo <= creditos.plazo AND amortizacion.sdo_Capital > 0 THEN DO:
            CREATE amortizacion.
            amortizacion.nit = creditos.nit.
            amortizacion.num_credito = creditos.num_credito.
        END.
        ELSE
            LEAVE.

        i = i + 1.
    END.
END PROCEDURE.

PROCEDURE P-AmortCuotaFija:
    DEF VAR W_SdoAnt AS DECIMAL.
    DEF VAR W_Interes AS DECIMAL.
    
    /* Suma la cuota extra en caso que exista */
    FIND FIRST Extras WHERE Extras.Nit = creditos.nit
                        AND Extras.Num_Solicitud = creditos.num_solicitud
                        AND Extras.Nro_Cuota = amortizacion.nro_cuota
                        AND Extras.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Extras) THEN
        amortizacion.cuota = creditos.cuota + Extras.Vr_CuoExtra.
    ELSE
        amortizacion.cuota = creditos.cuota.

    IF saldoCapitalProyectado > 0 THEN DO:
        IF nroPeriodo = 1 THEN
            amortizacion.cuota_i = ((saldoCapitalProyectado * tasaPeriodica) / diasPeriodo) * (fecInicial - fecDesembolso).
        ELSE
            amortizacion.cuota_i = (saldoCapitalProyectado * tasaPeriodica).
    END.

    W_SdoAnt = saldoCapitalProyectado.
    W_Interes = amortizacion.cuota_i.
    amortizacion.cuota_k = amortizacion.cuota - amortizacion.cuota_i.
    saldoCapitalProyectado = saldoCapitalProyectado - amortizacion.cuota_k.
    amortizacion.sdo_Capital = saldoCapitalProyectado.

    /* Ajuste al plan de pagos por finalizar antes */
    IF amortizacion.sdo_capital < 0 THEN DO:
        amortizacion.cuota = amortizacion.cuota_i + W_SdoAnt.
        amortizacion.cuota_k = W_SdoAnt.
        saldoCapitalProyectado = 0.
        amortizacion.sdo_Capital = 0.
    END.

    IF nroPeriodo = creditos.plazo AND amortizacion.sdo_Capital <> 0 THEN DO:
        IF amortizacion.sdo_Capital < 0 THEN DO:
            amortizacion.sdo_capital = amortizacion.cuota_k - ABS(amortizacion.sdo_capital).
            amortizacion.cuota_i = amortizacion.cuota_i + ABS(amortizacion.sdo_Capital).
        END.
        ELSE DO:
            amortizacion.sdo_capital = amortizacion.cuota_k + amortizacion.sdo_capital.
            amortizacion.cuota = amortizacion.cuota_i + amortizacion.cuota_k.
        END.

        amortizacion.sdo_Capital = 0.
    END.
END PROCEDURE.

PROCEDURE HallarPeriodo:
    /******************* HALLAR LA TASA PERIODICA  **************************
    Devuelve la tasa Periodica apartir de la efectiva anual.
    *************************************************************************/
    DEFINE INPUT PARAMETER P_PerPag AS INTEGER.
    DEFINE INPUT PARAMETER P_Plazo AS INTEGER.
    DEFINE OUTPUT PARAMETER P_NroDias AS INTEGER.
    DEFINE OUTPUT PARAMETER P_NroPer AS INTEGER.
    
    CASE P_PerPag:
        WHEN 0 THEN
            ASSIGN P_NroDias = 1
                   P_NroPer = 360.

        WHEN 1 THEN
            ASSIGN P_NroDias = 7
                   P_NroPer = 52.

        WHEN 2 THEN
            ASSIGN P_NroDias = 10
                   P_NroPer = 36.

        WHEN 3 THEN
            ASSIGN P_NroDias = 15
                   P_NroPer = 24.

        WHEN 4 THEN
            ASSIGN P_NroDias = 30
                   P_NroPer = 12.

        WHEN 5 THEN
            ASSIGN P_NroDias = 60
                   P_NroPer = 6.

        WHEN 6 THEN
            ASSIGN P_NroDias = 90
                   P_NroPer = 4.

        WHEN 7 THEN
            ASSIGN P_NroDias = 120
                   P_NroPer = 3.

        WHEN 8 THEN
            ASSIGN P_NroDias = 180
                   P_NroPer = 2.

        WHEN 9 THEN
            ASSIGN P_NroDias = 360
                   P_NroPer = 1.
    END CASE.

END PROCEDURE.
