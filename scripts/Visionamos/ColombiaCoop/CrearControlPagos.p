DEFINE INPUT PARAMETER nitCredito AS CHARACTER.
DEFINE input PARAMETER numeroCredito AS INTEGER.

DEFINE VAR p_monto AS DECIMAL.
DEFINE VAR p_plazo AS INTEGER.
DEFINE VAR p_totext AS DECIMAL.
DEFINE VAR p_fecha AS DATE.
DEFINE VAR p_tasa AS DECIMAL.
DEFINE VAR p_razon AS DECIMAL.
DEFINE VAR p_gracia AS INTEGER.
DEFINE VAR p_perded AS INTEGER.
DEFINE VAR p_TipInt AS INTEGER.
DEFINE VAR p_sistema AS INTEGER.
DEFINE VAR p_nit AS CHARACTER.
DEFINE VAR p_codPro AS INTEGER.
DEFINE VAR p_nroDcto AS INTEGER.
DEFINE VAR P_VrSoli AS DECIMAL.
DEFINE VAR deuda AS DECIMAL.
DEF SHARED VAR W_ManFin AS HANDLE.
DEF VAR W_PerTra AS INT INIT 0.
DEF VAR W_NroDias AS INT INIT 0.
DEF VAR W_SdoCapTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_CuoTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_VlrPte AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_TvrPte AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEF VAR W_NroPer AS INT INIT 0.
DEF VAR I AS INT INIT 0.
DEFI VAR INT_MGracia AS DECIMAL INIT 0.
DEF VAR W_NMeses AS INT INIT 0 NO-UNDO.
DEF VAR W_Primero AS LOG INIT YES.
DEFINE VAR flagFecPago AS LOGICAL.
DEFINE VAR flagProy AS LOGICAL.
DEFINE VAR vIntCorriente AS DECIMAL.
DEFINE VAR vIntCausado AS DECIMAL.
DEFINE VAR vToday AS DATE INITIAL 05/05/2016.

DEF TEMP-TABLE W_TabAmor
    FIELD W_Periodo AS INT INIT 0
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_FecIniP AS DATE
    FIELD W_Cuota AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboCap AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboInt AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_SdoCap AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

DEF TEMP-TABLE W_TabExt
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_PlazExt LIKE Solicitud.Plazo
    FIELD W_Cuota AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

RUN RUTFINAN.R PERSISTENT SET W_MANFIN.

FIND FIRST creditos WHERE creditos.nit = nitCredito
                      AND creditos.num_credito = numeroCredito NO-ERROR.

flagfecPago = FALSE.
flagProy = FALSE.

/* oakley */

/* 1. Borro los registros de PlanPagos y ControlPagos */
FOR EACH planpagos WHERE planPagos.nit = creditos.nit
                     AND planPagos.num_credito = creditos.num_credito:
    DELETE planPagos.
END.

FOR EACH controlPagos WHERE controlPagos.nit = creditos.nit
                        AND controlPagos.num_credito = creditos.num_credito:
    DELETE controlPagos.
END.

p_monto = creditos.sdo_capital.
p_plazo = creditos.plazo.

/*FOR EACH Extras WHERE Extras.Nit EQ Creditos.Nit
                  AND extras.cod_credito = creditos.cod_credito
                  AND Extras.Num_Solicitud EQ Creditos.Num_Solicitud NO-LOCK:
    IF Extras.Estado EQ 1 THEN
        ASSIGN p_totExt = p_totExt + Extras.Vr_CuoExtra.
END.*/

p_fecha = creditos.fec_pagAnti.
P_Razon = creditos.incremento.
p_gracia = 0.
p_perded = creditos.per_pago.
p_TipInt = creditos.FOR_interes.
p_sistema = creditos.sistema.
p_nit = creditos.nit.
p_codPro = creditos.cod_credito.
p_nroDcto = Creditos.Num_Credito.

RUN HallarPeriodo.

p_tasa = creditos.tasa / (W_NroPer * 100).

/* 2 - Inicia la creación de los registros */
RUN P-Amortizacion.

/* Revisamos el estado de cada registro en controlPagos para marcar las cuotas que ya estén canceladas */
deuda = creditos.sdo_capital.

creditos.val_atraso = 0.
creditos.cuo_atraso = 0.
creditos.sdo_proyectado = creditos.sdo_capital.

IF creditos.fec_pago < vToday THEN
    creditos.dias_atraso = vToday - creditos.fec_pago.
ELSE
    creditos.dias_atraso = 0.

vIntCorriente = creditos.INT_corriente.
vIntCausado = 0.

creditos.sdo_proyectado = creditos.sdo_capital.

FOR EACH controlPagos WHERE controlPagos.nit EQ Creditos.Nit
                        and controlPagos.Num_Credito EQ Creditos.Num_Credito
                        /*AND controlPagos.fec_pago < TODAY*/ BY controlPagos.Nro_Cuota:
    IF controlPagos.fec_pago < vToday THEN DO:
        creditos.val_atraso = creditos.val_atraso + controlPagos.cuota_capital.
        creditos.cuo_atraso = creditos.cuo_atraso + 1.
        creditos.sdo_proyectado = deuda - controlPagos.cuota_capital.
    END.

    IF vIntCorriente > 0 THEN DO:
        IF vIntCorriente <= controlPagos.cuota THEN DO:
            vIntCausado = vIntCorriente.
            /*controlPagos.cuota_Capital = controlPagos.cuota_capital - vIntCausado.
            controlPagos.cuota_interes = vIntCausado.*/
            
            vIntCorriente = 0.
        END.
        ELSE DO:
            vIntCausado = controlPagos.cuota.
            /*controlPagos.cuota_capital = 0.*/
            vIntCorriente = vIntCorriente - controlPagos.cuota.
        END.

        controlPagos.INT_causado = vIntCausado.
        controlPagos.pago_capital = controlPagos.cuota - controlPagos.INT_causado.
    END.

    IF controlPagos.fec_pago >= vToday AND vIntCorriente = 0 THEN
        LEAVE.
    ELSE DO:
        IF controlPagos.fec_pago < vToday THEN
            creditos.sdo_proyectado = creditos.sdo_proyectado - controlPagos.cuota_capital.
    END.
END.




PROCEDURE P-Amortizacion:
    DEFINE VAR fecIni AS DATE.

    fecIni = vToday.
    
    /* 3 - Proceso de creación de los registros de ControlPagos */
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = P_CodPro NO-LOCK NO-ERROR.
    IF AVAIL(Pro_Creditos) AND Pro_Creditos.Id_PerGracia AND Dia_Gracia GT 0 THEN
        INT_MGracia = ROUND((((P_Monto * P_Tasa) / W_NroDias) * Dia_Gracia) / P_Plazo,0).

    CREATE W_TabAmor.
    ASSIGN W_FecTra = creditos.fec_pago - w_nroDias
           W_TabAmor.W_AboInt = 0       
           W_TabAmor.W_Cuota = 0
           W_TabAmor.W_AboCap = 0
           W_TabAmor.W_SdoCap = P_Monto
           W_CuoTra = creditos.cuota
           W_SdoCapTra = P_Monto.

    DO W_PerTra = creditos.cuo_pagadas TO P_Plazo BY 1:
        IF P_Sistema <> 2 AND W_PerTra > 0 THEN DO:
            IF w_PerTra > 1 THEN
                RUN CambiarFecha.

            IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                w_fecTra = w_fecTra + 3.

            W_TabAmor.W_Fecha = W_FecTra.
        END.

        IF P_Sistema EQ 1 OR P_Sistema EQ 2 THEN
            W_TabAmor.W_Cuota = creditos.cuota.

        W_TabAmor.W_Periodo = W_PerTra.

        IF (W_PerTra = 0) THEN DO:
            ASSIGN W_TabAmor.W_Cuota = 0
                   W_TabAmor.W_Fecha = P_Fecha.

            IF P_TipInt = 2 THEN
                ASSIGN W_TabAmor.W_AboInt = P_Monto * P_Tasa.
        END.
        ELSE DO:
            IF P_Sistema EQ 1 THEN
                RUN P-AmortCuotaFija.
            ELSE
                IF P_Sistema EQ 2 THEN DO:
                    W_NroDias = (P_Plazo * W_NroDias).

                    IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                        w_fecTra = w_fecTra + 3.

                    W_TabAmor.W_Fecha = W_FecTra.

                    IF P_TipInt = 1 THEN
                        ASSIGN W_TabAmor.W_AboInt = creditos.cuota - P_Monto.

                    ASSIGN W_TabAmor.W_AboCap = P_Monto
                           W_TabAmor.W_Fecha = W_FecTra
                           W_TabAmor.W_SdoCap = 0
                           W_SdoCapTra = 0.

                    IF W_SdoCapTra = 0 THEN
                        LEAVE.
                END.
                ELSE
                    IF P_Sistema EQ 3 THEN DO:
                        ASSIGN W_TabAmor.W_AboInt = P_Monto * P_Tasa.

                        IF P_Plazo = W_PerTra THEN DO:
                            IF P_TipInt = 1 THEN
                                ASSIGN W_TabAmor.W_Cuota = W_TabAmor.W_Cuota + P_Monto.
                            ELSE
                                ASSIGN W_TabAmor.W_Cuota = P_Monto W_TabAmor.W_AboInt = 0.

                            ASSIGN W_TabAmor.W_AboCap = P_Monto
                                   W_TabAmor.W_SdoCap = 0
                                   W_SdoCapTra = 0.
                        END.
                    END.
                    ELSE
                        IF P_Sistema EQ 4 THEN
                            RUN P-AmortCuotaConstante.
                        ELSE
                            IF P_Sistema GE 5 AND P_Sistema LE 7 THEN
                                RUN P-AmortizacionGradiente.
                            ELSE
                                IF P_Sistema EQ 8 THEN
                                    RUN P-AmortPeriodoGracia.
                                ELSE
                                    IF P_Sistema EQ 9 THEN DO:
                                        ASSIGN W_TabAmor.W_AboCap = (P_Monto / P_Plazo)
                                               W_SdoCapTra = (W_SdoCapTra - W_TabAmor.W_AboCap)
                                               W_TabAmor.W_SdoCap = W_SdoCapTra.

                                        IF W_PerTra = 1 AND P_TipInt = 1 THEN
                                            ASSIGN W_TabAmor.W_Cuota = (P_Monto / P_Plazo) + (P_Monto * P_Tasa)
                                                   W_TabAmor.W_AboInt = (W_TabAmor.W_Cuota - W_TabAmor.W_AboCap).
                                        ELSE
                                            W_TabAmor.W_Cuota = (P_Monto / P_Plazo).
                                    END.
        END.

        IF W_PerTra < P_Plazo /*AND W_SdoCapTra > 0*/ THEN
            CREATE W_TabAmor.

        I = I + 1.
    END.

    fecIni = vToday - 1.

    FOR EACH W_TabAmor:
        RUN Crear_PLan(INPUT W_Periodo,
                       INPUT fecIni,
                       INPUT W_Fecha,
                       INPUT W_Cuota,
                       INPUT W_Abocap,
                       INPUT W_AboInt,
                       INPUT W_SdoCap).
        
        fecIni = w_fecha.
    END.
END PROCEDURE.

PROCEDURE P-AmortCuotaConstante:

    /*MESSAGE "P-AmortCuotaConstante"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    W_TabAmor.W_AboCap = (P_Monto / P_Plazo).

    IF P_TipInt = 1 THEN
        RUN HCCC IN W_ManFin (INPUT P_Monto,
                              INPUT P_Plazo,
                              INPUT W_PerTra,
                              INPUT P_Tasa,
                              OUTPUT W_TabAmor.W_Cuota).
    ELSE
        W_TabAmor.W_Cuota = ((W_SdoCapTra - W_TabAmor.W_AboCap) * P_Tasa) + W_TabAmor.W_AboCap.

    ASSIGN W_TabAmor.W_AboInt = W_TabAmor.W_Cuota  - W_TabAmor.W_AboCap
           W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.

    IF W_PerTra = P_Plazo AND W_SdoCapTra <> 0 THEN DO:
        IF W_SdoCapTra < 0 THEN
            W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - ABS(W_SdoCapTra).
        ELSE
            W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + W_SdoCapTra.

        W_TabAmor.W_SdoCap = 0.
    END.*/
END PROCEDURE.

PROCEDURE P-AmortCuotaFija:
    DEF VAR W_SdoAnt LIKE Creditos.Monto.
    DEF VAR W_Interes LIKE Creditos.Monto.
    DEFI VAR W_CuoExT LIKE Creditos.Cuota INIT 0.

    W_CuoExT = creditos.cuota.

    IF P_TotExt GT 0 THEN DO:
        /*FIND FIRST Extras WHERE Extras.Nit EQ P_Nit
                            AND Extras.Num_Solicitud EQ Creditos.Num_Solicitud
                            AND extras.cod_credito = creditos.cod_credito
                            /*AND Extras.Nro_Cuota EQ W_TabAmor.W_Periodo*/
                            AND extras.Fec_Vcto = w_tabAmor.w_fecha
                            AND Extras.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Extras) THEN
            W_CuoExT = creditos.cuota + Extras.Vr_CuoExtra.*/
    END.

    IF P_TipInt = 1 AND W_SdoCapTra GT 0 THEN DO:
        /*IF w_pertra = 1 THEN DO:
            W_TabAmor.W_AboInt = ((W_SdoCapTra * P_Tasa) / W_NroDias) * (creditos.fec_pagAnti - creditos.fec_desembolso).
        END.
        ELSE DO:*/
            W_TabAmor.W_AboInt = (W_SdoCapTra * P_Tasa).
        /*END.*/
    END.
    ELSE
        IF W_SdoCapTra GT 0 THEN
            W_TabAmor.W_AboInt = ((W_SdoCapTra - W_CuoExT) * P_Tasa).

    ASSIGN W_SdoAnt = W_SdoCapTra
           W_Interes = W_TabAmor.W_AboInt
           W_TabAmor.W_Cuota = W_CuoExT.

    IF W_SdoCapTra GT 0 THEN
        ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - (W_TabAmor.W_AboInt + INT_MGracia)
               W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
               W_TabAmor.W_SdoCap = W_SdoCapTra.

    /* Ajuste a la última cuota - 27/05/2010 - */
    IF W_PerTra = P_Plazo AND W_TabAmor.W_SdoCap <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap)*/.
        ELSE
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap*/.

        W_TabAmor.W_Cuota = W_TabAmor.W_AboCap + W_TabAmor.W_AboInt.
    END.
END PROCEDURE.

PROCEDURE P-AmortizacionGradiente:
    DEF VAR W_Dia AS INT INIT 0.

    /*MESSAGE "P-AmortizacionGradiente"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF W_PerTra  > 1 AND P_Sistema <> 7 THEN DO:
        IF P_Sistema EQ 5 THEN
            W_CuoTra  = W_CuoTra + P_Razon.
        ELSE
            IF P_Sistema EQ 6 THEN
                W_CuoTra  = W_CuoTra * P_Razon.
    END.

    ASSIGN W_TabAmor.W_Cuota = W_CuoTra
           W_TabAmor.W_AboInt = W_SdoCapTra * P_Tasa
           W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - W_TabAmor.W_AboInt
           W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.

    IF (P_Sistema = 7) THEN DO:
        IF (P_PerDed = 1 OR P_PerDed = 2 OR P_PerDed = 3) THEN DO:
            W_Dia = ((YEAR(W_FecTra) - YEAR(P_Fecha)) * 360 + (MONTH(W_FecTra) - MONTH(P_Fecha)) * 30).

            IF W_Dia > 0 AND W_Dia MODULO 360 = 0 AND I MODULO W_NroPer = 0 THEN
                ASSIGN W_CuoTra  = W_CuoTra * P_Razon.
        END.
        ELSE DO:
            W_Dia = ((YEAR(W_FecTra) - YEAR(P_Fecha)) * 360 + (MONTH(W_FecTra) - MONTH(P_Fecha)) * 30).

            IF W_Dia > 0 AND W_Dia MODULO 360 = 0 THEN
                ASSIGN W_CuoTra  = W_CuoTra * P_Razon.
        END.
    END.*/
END PROCEDURE.

PROCEDURE P-AmortPeriodoGracia:
    /*MESSAGE "P-AmortPeriodoGracia"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF P_Gracia = W_PerTra THEN DO:
        RUN HFDP IN W_ManFin (INPUT P_Monto,
                              INPUT P_Tasa,
                              INPUT P_Gracia,
                              OUTPUT W_SdoCapTra).

        P_Monto = W_SdoCapTra.
        RETURN.
    END.
    ELSE
        IF P_Gracia > W_PerTra THEN
            RETURN.

    W_TabAmor.W_Cuota = creditos.cuota.

    RUN HCCF IN W_ManFin (INPUT P_Monto,
                          INPUT creditos.cuota,
                          INPUT W_PerTra - P_Gracia,
                          INPUT P_Tasa,
                          OUTPUT W_TabAmor.W_AboCap).

    ASSIGN W_TabAmor.W_AboInt = W_TabAmor.W_Cuota - W_TabAmor.W_AboCap
           W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.

    IF W_PerTra = P_Plazo AND W_SdoCapTra <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap).
        ELSE
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap.

        W_TabAmor.W_SdoCap = 0.
    END.*/
END PROCEDURE.

PROCEDURE CambiarFecha:
    RUN Halla_FecVcto.R (INPUT creditos.fec_pagAnti,
                         W_NroDias,
                         W_FecTra,
                         OUTPUT W_FecTra).

END PROCEDURE.


PROCEDURE Crear_Plan:
    /*--------------------*/
    DEF INPUT PARAMETER W_Periodo AS INT FORMAT "99".
    DEF INPUT PARAMETER W_FecAnt AS DATE.
    DEF INPUT PARAMETER W_Fecha AS DATE.
    DEF INPUT PARAMETER W_Cuota LIKE Creditos.Cuota.
    DEF INPUT PARAMETER W_Abocap LIKE Creditos.Sdo_Capital.
    DEF INPUT PARAMETER W_AboInt LIKE Creditos.Sdo_Capital.
    DEF INPUT PARAMETER W_SdoCap LIKE Creditos.Sdo_Capital.

    IF P_TipInt EQ 1 AND W_Periodo EQ 0 THEN
        RETURN.

    CREATE controlPagos.
    ASSIGN controlPagos.Cuota = W_Cuota
           controlPagos.fec_ini = w_fecAnt + 1
           controlPagos.Fec_pago = W_Fecha
           controlPagos.Nit = P_Nit
           controlPagos.Nro_Cuota = W_Periodo
           controlPagos.Num_Credito = P_NroDcto
           controlPagos.estado = 0
           controlPagos.Tasa = P_Tasa
           controlPagos.cuota_capital = W_AboCap
           controlPagos.cuota_interes = W_AboInt
           controlPagos.INT_causado = vIntCausado
           controlPagos.pago_capital = w_cuota - vIntCausado.

    IF W_Primero THEN
        ASSIGN controlPagos.estado = 1
               W_Primero = NO.

END PROCEDURE.



PROCEDURE hallarPeriodo:
    CASE creditos.per_pago:
        WHEN 0 THEN
            ASSIGN W_NroDias = 1
                   W_NroPer = 360.

        WHEN 1 THEN
            ASSIGN W_NroDias = 7
                   W_NroPer = 52.

        WHEN 2 THEN
            ASSIGN W_NroDias = 10
                   W_NroPer = 36.

        WHEN 3 THEN
            ASSIGN W_NroDias = 15
                   W_NroPer = 24.

        WHEN 4 THEN
            ASSIGN W_NroDias = 30
                   W_NroPer = 12.

        WHEN 5 THEN
            ASSIGN W_NroDias = 60
                   W_NroPer = 6.

        WHEN 6 THEN
            ASSIGN W_NroDias = 90
                   W_NroPer = 4.

        WHEN 7 THEN
            ASSIGN W_NroDias = 120
                   W_NroPer = 3.

        WHEN 8 THEN
            ASSIGN W_NroDias = 180
                   W_NroPer = 2.

        WHEN 9 THEN
            ASSIGN W_NroDias = 360
                   W_NroPer = 1.
    END CASE.

END PROCEDURE.
