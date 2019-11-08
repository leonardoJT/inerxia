DEFINE INPUT PARAMETER nitCredito AS CHARACTER.
DEFINE input PARAMETER numeroCredito AS INTEGER.
DEFINE INPUT PARAMETER pTasa AS DECIMAL.
DEFINE INPUT PARAMETER pMotivo AS CHARACTER.

DEFINE VAR p_monto AS DECIMAL.
DEFINE VAR p_tasa AS DECIMAL.

/* oakley */

DEFINE VAR p_sistema AS INTEGER.
DEFINE VAR p_nit AS CHARACTER.
DEFINE VAR p_codPro AS INTEGER.
DEFINE VAR p_nroDcto AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR P_VrSoli AS DECIMAL.
DEFINE VAR deuda AS DECIMAL.
DEFINE SHARED VAR W_ManFin AS HANDLE.
DEFINE VAR W_PerTra AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR W_SdoCapTra AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_CuoTra AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_VlrPte AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_TvrPte AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEFINE VAR W_NroPer AS INTEGER.
DEFINE VAR I AS INTEGER.
DEFINE VAR INT_MGracia AS DECIMAL.
DEFINE VAR W_NMeses AS INTEGER.
DEFINE VAR W_Primero AS LOGICAL INITIAL YES.
DEFINE VAR flagFecPago AS LOGICAL.
DEFINE VAR flagProy AS LOGICAL.
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaAnterior AS DATE.

DEFINE TEMP-TABLE W_TabAmor
    FIELD W_Periodo AS INTEGER
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_FecIniP AS DATE
    FIELD W_Cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboCap AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboInt AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_SdoCap AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".

DEFINE TEMP-TABLE W_TabExt
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_PlazExt AS INTEGER
    FIELD W_Cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".

RUN RUTFINAN.R PERSISTENT SET W_MANFIN.

FIND FIRST creditos WHERE creditos.nit = nitCredito
                      AND creditos.num_credito = numeroCredito NO-ERROR.

FOR EACH planpagos WHERE planPagos.nit = creditos.nit
                     AND planPagos.num_credito = creditos.num_credito:
    DELETE planPagos.
END.

FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                         AND CONTROL_pagos.num_credito = creditos.num_credito:
    DELETE CONTROL_pagos.
END.

FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                        AND amortizacion.num_credito = creditos.num_Credito
                        AND amortizacion.fec_pago >= creditos.fec_pagAnti:
    DELETE amortizacion.
END.

IF creditos.cod_credito = 123 THEN
    p_monto = creditos.sdo_capital.
ELSE
    p_monto = creditos.monto.

fechaIni = creditos.fec_pagAnti.
p_sistema = creditos.sistema.
p_nit = creditos.nit.
p_codPro = creditos.cod_credito.
p_nroDcto = Creditos.Num_Credito.

IF creditos.cod_credito = 123 THEN
    p_vrsoli = creditos.sdo_capital.
ELSE
    p_vrsoli = creditos.monto.

    /* oakley */

RUN HallarPeriodo IN W_ManFin (INPUT creditos.per_pago,
                               INPUT creditos.plazo,
                               OUTPUT W_NroDias,
                               OUTPUT W_NMeses,
                               OUTPUT W_NroPer,
                               OUTPUT P_NomPer).

p_tasa = creditos.tasa / (W_NroPer * 100).

RUN P-Amortizacion.

/* oakley */

/* Revisamos el estado de cada registro en control_pagos para marcar las cuotas que ya estén canceladas */
IF creditos.cod_credito = 123 THEN
    deuda = creditos.sdo_capital.
ELSE
    deuda = Creditos.monto.

creditos.val_atraso = 0.
creditos.cuo_atraso = 0.
creditos.cuo_pagadas = 0.
creditos.dias_atraso = 0.

FOR EACH CONTROL_pagos WHERE control_pagos.Nit EQ Creditos.Nit
                         and control_pagos.Num_Credito EQ Creditos.Num_Credito BY control_pagos.Nro_Cuota:
    IF ROUND(control_pagos.pagos_capitalAcum,0) > 0 THEN
        deuda = deuda - ROUND(control_pagos.pagos_capitalAcum,0).

    IF deuda >= creditos.sdo_Capital AND creditos.monto <> creditos.sdo_capital THEN
        IF NOT CONTROL_pagos.fec_Vcto >= TODAY /*+ w_nroDias*/ THEN
            CONTROL_pagos.id_pdoMes = 2.
    ELSE DO:
        IF flagFecPago = FALSE THEN DO:
            ASSIGN creditos.fec_pago = CONTROL_pagos.fec_Vcto WHEN creditos.cod_credito <> 123.
            flagFecPago = TRUE.
            creditos.cuo_pagadas = CONTROL_pagos.nro_cuota - 1.
            CONTROL_pagos.id_PdoMes = 1.
        END.
        ELSE
            CONTROL_pagos.id_PdoMes = 0.

            /* Si está atrasado */
        IF creditos.fec_pago < TODAY AND control_pagos.fec_Vcto < TODAY THEN DO:
            creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
            creditos.cuo_atraso = creditos.cuo_atraso + 1.
            creditos.dias_atraso = TODAY - creditos.fec_pago.
        END.

        IF CONTROL_pagos.fec_Vcto >= TODAY AND flagProy = FALSE THEN DO:
            creditos.sdo_proyectado = deuda + CONTROL_pagos.pagos_capitalAcum.
            flagProy = TRUE.
        END.
    END.

    IF CONTROL_pagos.Id_PdoMes = 2 THEN DO:
        ASSIGN control_pagos.Cap_pagado = control_pagos.pagos_capitalAcum
               control_pagos.Int_pagado = control_pagos.Pagos_IntAcum.
    END.
END.

IF creditos.fec_pago >= TODAY /*+ W_NroDias*/ + 1 THEN DO:
    FOR EACH CONTROL_pagos WHERE control_pagos.Nit EQ Creditos.Nit
                             and control_pagos.Num_Credito EQ Creditos.Num_Credito
                             AND CONTROL_pagos.fec_Vcto >= TODAY /*+ W_NroDias*/ BY control_pagos.Nro_Cuota:
        ASSIGN creditos.fec_pago = control_pagos.fec_Vcto WHEN creditos.cod_credito <> 123.
        LEAVE.
    END.
END.

PROCEDURE P-Amortizacion:
    FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ P_CodPro NO-LOCK NO-ERROR.
    IF AVAIL(Pro_Creditos) AND Pro_Creditos.Id_PerGracia AND Dia_Gracia GT 0 THEN
        ASSIGN INT_MGracia = ROUND((((P_Monto * P_Tasa) / W_NroDias) * Dia_Gracia) / creditos.plazo,0).

    FIND FIRST Creditos WHERE Creditos.Nit EQ P_Nit
                          AND Creditos.Num_Credito EQ P_NroDcto NO-ERROR.

    CREATE W_TabAmor.
    ASSIGN W_FecTra = fechaIni
           W_TabAmor.W_AboInt = 0       
           W_TabAmor.W_Cuota = 0
           W_TabAmor.W_AboCap = 0
           W_TabAmor.W_SdoCap = P_Monto
           W_CuoTra = creditos.cuota
           W_SdoCapTra = P_Monto.

    DO W_PerTra = 0 TO creditos.plazo BY 1:
        IF P_Sistema <> 2 AND W_PerTra > 0 THEN DO:
            IF w_PerTra > 1 THEN
                RUN CambiarFecha.

            IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                w_fecTra = w_fecTra + 3.

            W_TabAmor.W_FecIniP = W_FecTra.
            W_TabAmor.W_Fecha = W_FecTra.
        END.

        IF P_Sistema EQ 1 OR P_Sistema EQ 2 THEN
            W_TabAmor.W_Cuota = creditos.cuota.
        ELSE
            IF P_Sistema EQ 3 THEN
                W_TabAmor.W_Cuota = P_Monto * P_Tasa.

        ASSIGN W_TabAmor.W_Periodo = W_PerTra.

        IF (W_PerTra = 0) THEN DO:
            ASSIGN W_TabAmor.W_Cuota = 0
                   W_TabAmor.W_Fecha = creditos.fec_pagAnti.

            IF creditos.FOR_interes = 2 THEN
                ASSIGN W_TabAmor.W_AboInt = P_Monto * P_Tasa.

            fechaAnterior = creditos.fec_pagAnti.
        END.
        ELSE DO:
            IF P_Sistema EQ 1 THEN
                RUN P-AmortCuotaFija.
            ELSE
                IF P_Sistema EQ 2 THEN DO:
                    W_NroDias = (creditos.plazo * W_NroDias).

                    IF W_PerTra NE 1 THEN
                        W_TabAmor.W_FecIniP = W_FecTra. /* ojo nh */

                    /*RUN CambiarFecha.*/

                    IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                        w_fecTra = w_fecTra + 3.

                    W_TabAmor.W_Fecha = W_FecTra.

                    IF creditos.FOR_interes = 1 THEN
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

                        IF creditos.plazo = W_PerTra THEN DO:
                            IF creditos.FOR_interes = 1 THEN
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
                                        ASSIGN W_TabAmor.W_AboCap = (P_Monto / creditos.plazo)
                                               W_SdoCapTra = (W_SdoCapTra - W_TabAmor.W_AboCap)
                                               W_TabAmor.W_SdoCap = W_SdoCapTra.

                                        IF W_PerTra = 1 AND creditos.FOR_interes = 1 THEN
                                            ASSIGN W_TabAmor.W_Cuota = (P_Monto / creditos.plazo) + (P_Monto * P_Tasa)
                                                   W_TabAmor.W_AboInt = (W_TabAmor.W_Cuota - W_TabAmor.W_AboCap).
                                        ELSE
                                            W_TabAmor.W_Cuota = (P_Monto / creditos.plazo).
                                    END.
        END.

        IF W_PerTra < creditos.plazo THEN
            CREATE W_TabAmor.

        I = I + 1.
    END.

    FOR EACH W_TabAmor:   /* WITH FRAME F-Amortizacion:*/
        RUN Crear_PLan(INPUT W_Periodo,
                       INPUT W_FecIniP,
                       INPUT W_Fecha,
                       INPUT W_Cuota,
                       INPUT W_Abocap,
                       INPUT W_AboInt,
                       INPUT W_SdoCap).
    END.
END PROCEDURE.

PROCEDURE P-AmortCuotaConstante:
    W_TabAmor.W_AboCap = (P_Monto / creditos.plazo).

    IF creditos.FOR_interes = 1 THEN
        RUN HCCC IN W_ManFin (INPUT P_Monto,
                              INPUT creditos.plazo,
                              INPUT W_PerTra,
                              INPUT P_Tasa,
                              OUTPUT W_TabAmor.W_Cuota).
    ELSE
        W_TabAmor.W_Cuota = ((W_SdoCapTra - W_TabAmor.W_AboCap) * P_Tasa) + W_TabAmor.W_AboCap.

    ASSIGN W_TabAmor.W_AboInt = W_TabAmor.W_Cuota  - W_TabAmor.W_AboCap
           W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.

    IF W_PerTra = creditos.plazo AND W_SdoCapTra <> 0 THEN DO:
        IF W_SdoCapTra < 0 THEN
            W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - ABS(W_SdoCapTra).
        ELSE
            W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + W_SdoCapTra.

        W_TabAmor.W_SdoCap = 0.
    END.
END PROCEDURE.

PROCEDURE P-AmortCuotaFija:
    DEF VAR W_SdoAnt LIKE Creditos.Monto.
    DEF VAR W_Interes LIKE Creditos.Monto.
    DEFI VAR W_CuoExT LIKE Creditos.Cuota INIT 0.

    W_CuoExT = creditos.cuota.

    FIND FIRST Extras WHERE Extras.Nit EQ P_Nit
                        AND Extras.Num_Solicitud EQ Creditos.Num_Solicitud
                        AND extras.cod_credito = creditos.cod_credito
                        AND extras.Fec_Vcto > fechaAnterior
                        AND extras.fec_Vcto <= w_tabAmor.w_fecha
                        AND Extras.Estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Extras) THEN
        W_CuoExT = creditos.cuota + Extras.Vr_CuoExtra.
    
    IF creditos.FOR_interes = 1 AND W_SdoCapTra GT 0 THEN DO:
        IF w_pertra = 1 THEN DO:
            W_TabAmor.W_AboInt = ((W_SdoCapTra * P_Tasa) / W_NroDias) * (creditos.fec_pagAnti - creditos.fec_desembolso).
        END.
        ELSE DO:
            W_TabAmor.W_AboInt = (W_SdoCapTra * P_Tasa).
        END.
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
    IF W_PerTra = creditos.plazo AND W_TabAmor.W_SdoCap <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                   W_TabAmor.W_SdoCap = 0
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap)*/.
        ELSE
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap
                   W_TabAmor.W_SdoCap = 0
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap*/.

        W_TabAmor.W_Cuota = W_TabAmor.W_AboCap + W_TabAmor.W_AboInt.
    END.

    fechaAnterior = W_TabAmor.w_fecha.
END PROCEDURE.

PROCEDURE P-AmortizacionGradiente:
    DEF VAR W_Dia AS INT INIT 0.

    IF W_PerTra  > 1 AND P_Sistema <> 7 THEN DO:
        IF P_Sistema EQ 5 THEN
            W_CuoTra  = W_CuoTra + creditos.incremento.
        ELSE
            IF P_Sistema EQ 6 THEN
                W_CuoTra  = W_CuoTra * creditos.incremento.
    END.

    ASSIGN W_TabAmor.W_Cuota = W_CuoTra
           W_TabAmor.W_AboInt = W_SdoCapTra * P_Tasa
           W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - W_TabAmor.W_AboInt
           W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.

    IF (P_Sistema = 7) THEN DO:
        IF (creditos.per_pago = 1 OR creditos.per_pago = 2 OR creditos.per_pago = 3) THEN DO:
            W_Dia = ((YEAR(W_FecTra) - YEAR(creditos.fec_pagAnti)) * 360 + (MONTH(W_FecTra) - MONTH(creditos.fec_pagAnti)) * 30).

            IF W_Dia > 0 AND W_Dia MODULO 360 = 0 AND I MODULO W_NroPer = 0 THEN
                ASSIGN W_CuoTra  = W_CuoTra * creditos.incremento.
        END.
        ELSE DO:
            W_Dia = ((YEAR(W_FecTra) - YEAR(creditos.fec_pagAnti)) * 360 + (MONTH(W_FecTra) - MONTH(creditos.fec_pagAnti)) * 30).

            IF W_Dia > 0 AND W_Dia MODULO 360 = 0 THEN
                ASSIGN W_CuoTra  = W_CuoTra * creditos.incremento.
        END.
    END.
END PROCEDURE.

PROCEDURE P-AmortPeriodoGracia:
    IF W_PerTra = 0 THEN DO:
        RUN HFDP IN W_ManFin (INPUT P_Monto,
                              INPUT P_Tasa,
                              INPUT 0,
                              OUTPUT W_SdoCapTra).

        P_Monto = W_SdoCapTra.
        RETURN.
    END.
    ELSE
        IF W_PerTra < 0 THEN
            RETURN.

    W_TabAmor.W_Cuota = creditos.cuota.

    RUN HCCF IN W_ManFin (INPUT P_Monto,
                          INPUT creditos.cuota,
                          INPUT W_PerTra,
                          INPUT P_Tasa,
                          OUTPUT W_TabAmor.W_AboCap).

    ASSIGN W_TabAmor.W_AboInt = W_TabAmor.W_Cuota - W_TabAmor.W_AboCap
           W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.

    IF W_PerTra = creditos.plazo AND W_SdoCapTra <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap).
        ELSE
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap.

        W_TabAmor.W_SdoCap = 0.
    END.
END PROCEDURE.

PROCEDURE CambiarFecha:
    RUN Halla_FecVcto.R (INPUT creditos.fec_pagAnti,
                         W_NroDias,
                         W_FecTra,
                         OUTPUT W_FecTra).

END PROCEDURE.

PROCEDURE Deduc_Cre:
    DEF VAR W_TotVal LIKE Deducible.Valor INIT 0.
    DEF VAR W_Vdd LIKE Deducible.Valor.
    DEF VAR W_Ndd AS INT FORMAT "99".
    DEF VAR W_Porcent LIKE Deducible.Valor.
    DEF VAR Linea1 AS CHAR FORMAT "X(120)" INIT "".

    W_TotVal = 0.

    FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ P_CodPro NO-LOCK NO-ERROR.
    IF AVAIL(Pro_Creditos) THEN
        DO W_Ndd = 1 TO 10:
            IF Pro_Creditos.Deducible[W_Ndd] GT "0000" THEN DO:
                FIND Deducible WHERE Deducible.Cod_Deducible EQ Pro_Creditos.Deducible[W_Ndd]
                                 AND Deducible.Estado EQ 1
                                 AND Deducible.Tip_Deducible EQ 1 NO-LOCK NO-ERROR.
                IF AVAIL(Deducible) THEN DO:
                    ASSIGN W_Vdd = Deducible.Valor W_Porcent = 0.
            
                    IF Deducible.Cla_Deducible = 1 THEN DO:
                        W_Porcent = Deducible.Valor.

                        ASSIGN W_Vdd = ROUND ((P_VrSoli * Deducible.Valor) / 100 ,0).
                    END.

                    ASSIGN W_TotVal  = W_TotVal + W_Vdd.

                    RUN Imp_Deducibles (INPUT W_Porcent,
                                        W_Vdd).
                END.
            END.
        END.
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

    IF W_Primero THEN
        W_FecAnt = Creditos.fec_pagAnti.

    IF creditos.FOR_interes EQ 1 AND W_Periodo EQ 0 THEN
        RETURN.

    CREATE PlanPagos.
    ASSIGN PlanPagos.Agencia = creditos.agencia
           PlanPAgos.Nit = P_Nit
           PlanPagos.Num_Credito = P_NroDcto
           PlanPagos.Cod_Credito = P_CodPro
           PlanPagos.Fec_Ini = W_FecAnt
           PlanPagos.Fec_Vcto = W_Fecha
           PlanPagos.Fec_ProxPago = W_Fecha
           PlanPagos.Nro_Cuota = W_Periodo
           PlanPagos.Cuota = W_Cuota
           PlanPagos.Tasa = P_Tasa
           PlanPagos.Plazo = creditos.plazo
           PlanPagos.Monto = P_Monto
           PlanPagos.Id_PdoMes = 0.

    CREATE CONTROL_pagos.
    ASSIGN control_pagos.Agencia = creditos.agencia
           control_pagos.Cod_Credito = P_CodPro
           control_pagos.Cuota = W_Cuota
           control_pagos.Fec_Inic = W_FecAnt
           control_pagos.Fec_Vcto = W_Fecha
           control_pagos.Nit = P_Nit
           control_pagos.Nro_Cuota = W_Periodo
           control_pagos.Num_Credito = P_NroDcto
           control_pagos.Id_PdoMes = 0
           control_pagos.Plazo = creditos.plazo
           control_pagos.Tasa = P_Tasa
           control_pagos.pagos_capitalAcum = W_AboCap
           control_pagos.pagos_IntAcum = W_AboInt.

    IF W_Primero THEN
        ASSIGN PlanPagos.Id_PdoMes = 1
               control_pagos.Id_PdoMes = 1
               PlanPagos.Fec_Ini = creditos.fec_pagAnti
               control_pagos.Fec_Inic = creditos.fec_pagAnti
               PlanPagos.Fec_ProxPago = W_Fecha
               W_Primero = NO.

    CREATE amortizacion.
    amortizacion.cuota = w_cuota.
    amortizacion.cuota_i = W_AboInt.
    amortizacion.cuota_k = W_AboCap.
    amortizacion.fec_pago = w_fecha.
    amortizacion.nit = p_nit.
    amortizacion.nro_cuota = w_periodo.
    amortizacion.num_credito = p_NroDcto.
    amortizacion.sdo_capital = W_SdoCap.

    IF amortizacion.sdo_capital < 0 THEN DO:
        amortizacion.cuota = amortizacion.cuota + W_SdoCap.
        amortizacion.cuota_k = amortizacion.cuota_k + W_SdoCap.
        amortizacion.sdo_capital = 0.
    END.

    IF w_periodo = 1 THEN DO:
        CREATE amortizacion.
        amortizacion.fec_pago = creditos.fec_desembolso.
        amortizacion.nit = p_nit.
        amortizacion.nro_cuota = 0.
        amortizacion.num_credito = p_NroDcto.
        amortizacion.sdo_capital = creditos.sdo_Capital.
        amortizacion.novedad = "Desembolso inicial".
    END.

END PROCEDURE.



