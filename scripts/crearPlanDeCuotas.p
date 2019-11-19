DEFINE SHARED VAR W_ManFin AS HANDLE.

DEFINE VAR P_Monto AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR numExtras AS INTEGER.
DEFINE VAR fecDesembolso AS DATE.
DEFINE VAR fecInicial AS DATE.
DEFINE VAR pTasa AS DECIMAL.
DEFINE VAR pRazon AS INTEGER.
DEFINE VAR W_SdoCapTra AS DECIMAL.
DEFINE VAR W_PerTra AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR P_NMeses AS INTEGER.
DEFINE VAR w_pdoAno AS INTEGER.
DEFINE VAR W_NomPer AS CHARACTER.

DEFINE TEMP-TABLE W_TabExt
    FIELD W_Fecha AS DATE
    FIELD W_PlazExt AS INTEGER
    FIELD W_Cuota AS DECIMAL.

FOR EACH creditos WHERE creditos.nit = "8283838" AND num_credito = 15457 NO-LOCK:
    FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                            AND amortizacion.num_credito = creditos.num_credito:
        DELETE amortizacion.
    END.

    RUN crearPlanAmortizacion.
END.

PROCEDURE crearPlanAmortizacion:
    DEFINE VAR W_FecTra AS DATE.
    DEFINE VAR W_VlrExt AS DECIMAL.
    DEFINE VAR i AS INTEGER.

    EMPTY TEMP-TABLE W_TabExt.

    RUN HallarPeriodo (INPUT Creditos.Per_Pago,
                       INPUT Creditos.Plazo,
                       OUTPUT W_NroDias,
                       OUTPUT P_NMeses,
                       OUTPUT W_PdoAno,
                       OUTPUT W_NomPer).

    p_monto = creditos.monto.
    
    numExtras = 0.

    FOR EACH extras WHERE extras.agencia = creditos.agencia
                      AND extras.nit = creditos.nit
                      AND extras.cod_credito = creditos.cod_credito
                      AND extras.num_solicitud = creditos.num_solicitud NO-LOCK:
        numExtras = numExtras + 1.
    END.

    fecDesembolso = Creditos.fec_Desembolso.

    FIND FIRST mov_creditos WHERE mov_creditos.nit = creditos.nit
                              AND mov_creditos.num_credito = creditos.num_credito
                              AND mov_creditos.fecha >= creditos.fec_desembolso
                              AND mov_creditos.cod_operacion = 999999999 NO-LOCK NO-ERROR.
    IF AVAILABLE mov_creditos THEN
        fecDesembolso = creditos.fec_pagAnti - W_NroDias.

    IF Creditos.Fec_PagAnti <> ? THEN
        fecInicial = Creditos.Fec_PagAnti.
    ELSE
        fecInicial = Creditos.Fec_desembolso.

    pTasa = Creditos.Tasa / (W_PdoAno * 100).

    pRazon = (Creditos.Fec_PagAnti - creditos.Fec_Desembolso) - 1.

    CREATE amortizacion.
    ASSIGN amortizacion.nit = creditos.nit
           amortizacion.num_credito = creditos.num_credito
           amortizacion.sdo_Capital = P_Monto.

    W_FecTra = fecInicial.
    W_SdoCapTra = P_Monto.

    DO W_PerTra = 0 TO creditos.plazo BY 1:
        IF W_PerTra > 0 THEN DO:
            IF W_PerTra > 1 THEN
                RUN Halla_FecVcto.R (INPUT fecInicial,
                                     INPUT W_NroDias,
                                     INPUT W_FecTra,
                                     OUTPUT W_FecTra).

            amortizacion.fec_pago = W_FecTra.
        END.

        amortizacion.cuota = creditos.cuota.
        amortizacion.nro_cuota = W_PerTra.

        IF (W_PerTra = 0) THEN DO:
            amortizacion.cuota = 0.
            amortizacion.fec_pago = fecDesembolso.
        END.
        ELSE
            RUN P-AmortCuotaFija.

        IF W_PerTra <= creditos.plazo AND amortizacion.sdo_Capital > 0 THEN DO:
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

    IF W_SdoCapTra > 0 THEN DO:
        IF w_pertra = 1 THEN
            amortizacion.cuota_i = ((W_SdoCapTra * pTasa) / W_NroDias) * (fecInicial - fecDesembolso).
        ELSE
            amortizacion.cuota_i = (W_SdoCapTra * pTasa).
    END.

    W_SdoAnt = W_SdoCapTra.
    W_Interes = amortizacion.cuota_i.
    amortizacion.cuota_k = amortizacion.cuota - amortizacion.cuota_i.
    W_SdoCapTra = W_SdoCapTra - amortizacion.cuota_k.
    amortizacion.sdo_Capital = W_SdoCapTra.

    /* Ajuste al plan de pagos por finalizar antes */
    IF amortizacion.sdo_capital < 0 THEN DO:
        amortizacion.cuota = amortizacion.cuota_i + W_SdoAnt.
        amortizacion.cuota_k = W_SdoAnt.
        W_SdoCapTra = 0.
        amortizacion.sdo_Capital = 0.
    END.

    IF W_PerTra = creditos.plazo AND amortizacion.sdo_Capital <> 0 THEN DO:
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
    DEFINE OUTPUT PARAMETER P_NMeses AS INTEGER.
    DEFINE OUTPUT PARAMETER P_NroPer AS INTEGER.
    DEFINE OUTPUT PARAMETER P_NomPer AS CHARACTER FORMAT "X(15)".

    CASE P_PerPag:
        WHEN 0 THEN
            ASSIGN P_NomPer = "Diario"
                   P_NroDias = 1
                   P_NMeses = P_Plazo / 30
                   P_NroPer = 360.

        WHEN 1 THEN
            ASSIGN P_NomPer = "Semanal"
                   P_NroDias = 7
                   P_NMeses = P_Plazo / 4
                   P_NroPer = 52.

        WHEN 2 THEN
            ASSIGN P_NomPer = "Decadal"
                   P_NMeses = P_Plazo / 3
                   P_NroDias = 10
                   P_NroPer = 36.

        WHEN 3 THEN
            ASSIGN P_NomPer = "Quincenal"
                   P_NroDias = 15
                   P_NMeses = P_Plazo / 2
                   P_NroPer = 24.

        WHEN 4 THEN
            ASSIGN P_NomPer = "Mensual"
                   P_NroDias = 30
                   P_NMeses = P_Plazo
                   P_NroPer = 12.

        WHEN 5 THEN
            ASSIGN P_NomPer = "Bimensual"
                   P_NroDias = 60
                   P_NMeses = P_Plazo * 2
                   P_NroPer = 6.

        WHEN 6 THEN
            ASSIGN P_NomPer = "Trimestral"
                   P_NroDias = 90
                   P_NMeses = P_Plazo * 3
                   P_NroPer = 4.

        WHEN 7 THEN
            ASSIGN P_NomPer = "Cuamestral"
                   P_NroDias = 120
                   P_NMeses = P_Plazo * 4
                   P_NroPer = 3.

        WHEN 8 THEN
            ASSIGN P_NomPer = "Semestral"
                   P_NroDias = 180
                   P_NMeses = P_Plazo * 6
                   P_NroPer = 2.

        WHEN 9 THEN
            ASSIGN P_NomPer = "Anual"
                   P_NroDias = 360
                   P_NMeses = P_Plazo * 12
                   P_NroPer = 1.
    END CASE.

END PROCEDURE.
