DEFINE TEMP-TABLE tabla LIKE CONTROL_pagos.

DEFINE VAR p_age AS INTEGER.
DEFINE VAR p_monto AS DECIMAL.
DEFINE VAR p_plazo AS INTEGER.
DEFINE VAR p_cuota AS DECIMAL.
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
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR P_VrSoli AS DECIMAL.
DEFINE VAR deuda AS DECIMAL.
DEF SHARED VAR W_ManFin AS HANDLE.
DEF VAR W_PerTra AS INT INIT 0.
DEF VAR W_NroDias AS INT INIT 0.
DEF VAR W_SdoCapTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_CuoTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

DEF VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEF VAR W_VlrExt AS DEC  INIT 0.
DEF VAR W_MonTra AS CHAR FORMAT "X(20)" INIT "".
DEF VAR W_NroPer AS INT INIT 0.
DEF VAR I AS INT INIT 0.
DEF VAR W_TitP AS CHAR FORMAT "X(16)" INIT "Nro.Solicitud  :".
DEFI VAR INT_MGracia LIKE Creditos.Cuota INIT 0.   /*Valor c/mes intereses del Pdo-Gracia*/
DEFI VAR W_TotGra LIKE Creditos.Cuota INIT 0.
DEF VAR W_NMeses AS INT INIT 0 NO-UNDO.
DEF VAR W_Primero AS LOG INIT YES.
DEF VAR W_RegPri AS INT FORMAT "9".
DEFI VAR W_FecIni LIKE Solicitud.Fec_Solicitud.
DEFI VAR W_FecChar AS CHAR FORM "X(10)".
DEFI VAR W_Si28 AS LOG INIT FALSE.
DEFI VAR W_Si29 AS LOG INIT FALSE.
DEFINE VAR tasaCredito AS DECIMAL.
DEFINE VAR flagFecPago AS LOGICAL.
DEFINE VAR flagProy AS LOGICAL.
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaDes AS DATE.

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

DEF VAR W_TotInt AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_TotCap AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_TDeduc LIKE Pro_Creditos.Deducible.
DEF VAR W_VDeduc LIKE Solicitud.Deducible.
DEF VAR W_IDeduc LIKE Solicitud.Id_Adicionales.
DEF VAR W_MDeduc LIKE Solicitud.Monto.
DEFINE VAR cuotaIni AS INTEGER.

/* Empieza el código */
RUN RUTFINAN.R PERSISTENT SET W_MANFIN.

OUTPUT TO d:\Leonardo\Planes.csv.
FOR EACH creditos WHERE creditos.cod_credito <> 123
                    AND creditos.sdo_capital > 0 NO-LOCK:
    flagfecPago = FALSE.
    flagProy = FALSE.
    
    cuotaIni = creditos.cuo_pagadas + 1.
    
    CASE creditos.per_pago:
        WHEN 1 THEN fechaIni = creditos.fec_pago - 7.
        WHEN 2 THEN fechaIni = creditos.fec_pago - 10.
        WHEN 3 THEN fechaIni = creditos.fec_pago - 15.
        WHEN 4 THEN fechaIni = ADD-INTERVAL(creditos.fec_pago,-1,"months").
        WHEN 5 THEN fechaIni = ADD-INTERVAL(creditos.fec_pago,-2,"months").
        WHEN 6 THEN fechaIni = ADD-INTERVAL(creditos.fec_pago,-3,"months").
        WHEN 7 THEN fechaIni = ADD-INTERVAL(creditos.fec_pago,-4,"months").
        WHEN 8 THEN fechaIni = ADD-INTERVAL(creditos.fec_pago,-6,"months").
        WHEN 9 THEN fechaIni = ADD-INTERVAL(creditos.fec_pago,-1,"year").
    END CASE.
    
    p_age = creditos.agencia.
    p_monto = creditos.sdo_capital.
    p_plazo = creditos.plazo - cuotaIni + 1.
    p_cuota = creditos.cuota.
    
    FOR EACH Extras WHERE Extras.Nit EQ Creditos.Nit
                      AND extras.cod_credito = creditos.cod_credito
                      AND Extras.Num_Solicitud EQ Creditos.Num_Solicitud
                      AND extras.estado = 1 NO-LOCK:
        p_totExt = p_totExt + Extras.Vr_CuoExtra.
    END.
    
    p_fecha = fechaIni.
    P_Razon = creditos.incremento.
    p_gracia = 0.
    p_perded = creditos.per_pago.
    p_TipInt = creditos.FOR_interes.
    p_sistema = creditos.sistema.
    p_nit = creditos.nit.
    p_codPro = creditos.cod_credito.
    p_nroDcto = Creditos.Num_Credito.
    p_vrsoli = creditos.sdo_capital.
    tasaCredito = creditos.tasa.
    
    RUN HallarPeriodo IN W_ManFin (INPUT P_Perded,
                                   INPUT P_Plazo,
                                   OUTPUT W_NroDias,
                                   OUTPUT W_NMeses,
                                   OUTPUT W_NroPer,
                                   OUTPUT P_NomPer).
    
    p_tasa = creditos.tasa / (W_NroPer * 100).
    
    RUN P-Amortizacion.
    
    /* Revisamos el estado de cada registro en tabla para marcar las cuotas que ya estén canceladas */
    deuda = creditos.sdo_capital.
    
    FOR EACH tabla WHERE tabla.Nit EQ Creditos.Nit
                     and tabla.Num_Credito EQ Creditos.Num_Credito BY tabla.Nro_Cuota:
        IF ROUND(tabla.pagos_capitalAcum,0) > 0 THEN
            deuda = deuda - ROUND(tabla.pagos_capitalAcum,0).
    
        IF deuda >= creditos.sdo_Capital AND creditos.monto <> creditos.sdo_capital THEN
            tabla.id_pdoMes = 2.
        ELSE DO:
            IF flagFecPago = FALSE THEN DO:
                flagFecPago = TRUE.
                tabla.id_PdoMes = 1.
            END.
            ELSE
                tabla.id_PdoMes = 0.
    
            /* Si está atrasado */
            IF tabla.fec_Vcto >= TODAY AND flagProy = FALSE THEN DO:
                flagProy = TRUE.
            END.
        END.
    
        IF tabla.Id_PdoMes = 2 THEN DO:
            ASSIGN tabla.Cap_pagado = tabla.pagos_capitalAcum
                   tabla.Int_pagado = tabla.Pagos_IntAcum.
        END.
    END.
    
    FOR EACH tabla WHERE tabla.Nit EQ Creditos.Nit
                     and tabla.Num_Credito EQ Creditos.Num_Credito
                     AND tabla.fec_vcto <= TODAY + w_nroDias:
        tabla.nro_cuota = tabla.nro_cuota + cuotaIni - 1.
        EXPORT DELIMITER ";" tabla.
    END.
END.
OUTPUT CLOSE.
    
PROCEDURE P-Amortizacion:
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = P_CodPro NO-LOCK NO-ERROR.
    IF AVAIL(Pro_Creditos) AND Pro_Creditos.Id_PerGracia AND Dia_Gracia GT 0 THEN
        INT_MGracia = ROUND((((P_Monto * P_Tasa) / W_NroDias) * Dia_Gracia) / P_Plazo,0).

    FIND FIRST Creditos WHERE Creditos.Nit EQ P_Nit
                          AND Creditos.Num_Credito EQ P_NroDcto NO-ERROR.

    CREATE W_TabAmor.
    ASSIGN W_TotInt = 0
           W_TotCap = 0
           W_FecTra = fechaIni
           W_FecIni = fechaIni
           W_TabAmor.W_AboInt = 0       
           W_TabAmor.W_Cuota = 0
           W_TabAmor.W_AboCap = 0
           W_TabAmor.W_SdoCap = P_Monto
           W_CuoTra = P_Cuota
           W_SdoCapTra = P_Monto.

    DO W_PerTra = 0 TO P_Plazo BY 1:
        IF P_Sistema <> 2 AND W_PerTra > 0 THEN DO:
            IF w_PerTra > 1 THEN
                RUN CambiarFecha.

            IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                w_fecTra = w_fecTra + 3.

            W_TabAmor.W_FecIniP = W_FecTra.
            W_TabAmor.W_Fecha = W_FecTra.
        END.

        IF P_Sistema = 1 AND P_TotExt > 0 THEN
            W_VlrExt = 0.

        IF P_Sistema EQ 1 OR P_Sistema EQ 2 THEN
            W_TabAmor.W_Cuota = P_Cuota.
        ELSE
            IF P_Sistema EQ 3 THEN
                W_TabAmor.W_Cuota = P_Monto * P_Tasa.

        ASSIGN W_TabAmor.W_Periodo = W_PerTra.

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

                    IF W_PerTra NE 1 THEN
                        W_TabAmor.W_FecIniP = W_FecTra. /* ojo nh */

                    /*RUN CambiarFecha.*/

                    IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                        w_fecTra = w_fecTra + 3.

                    W_TabAmor.W_Fecha = W_FecTra.

                    IF P_TipInt = 1 THEN
                        ASSIGN W_TabAmor.W_AboInt = P_Cuota - P_Monto
                               W_TotInt = W_TabAmor.W_AboInt.

                    ASSIGN W_TabAmor.W_AboCap = P_Monto
                           W_TabAmor.W_Fecha = W_FecTra
                           W_TotCap = P_Monto
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

        ASSIGN W_TotInt = W_TotInt + W_TabAmor.W_AboInt
               W_TotGra = W_TotGra + INT_MGracia WHEN W_PerTra NE 0
               W_TotCap = W_TotCap + W_TabAmor.W_AboCap.
                                                        
        IF W_PerTra < P_Plazo THEN
            CREATE W_TabAmor.

        I = I + 1.
    END.

    FOR EACH W_TabAmor NO-LOCK:
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
    END.
END PROCEDURE.

PROCEDURE P-AmortCuotaFija:
    DEF VAR W_SdoAnt LIKE Creditos.Monto.
    DEF VAR W_Interes LIKE Creditos.Monto.
    DEFI VAR W_CuoExT LIKE Creditos.Cuota INIT 0.

    W_CuoExT = P_Cuota.

    IF P_TotExt GT 0 THEN DO:
        FIND FIRST Extras WHERE Extras.Nit EQ P_Nit
                            AND Extras.Num_Solicitud EQ Creditos.Num_Solicitud
                            AND extras.cod_credito = creditos.cod_credito
                            AND Extras.Nro_Cuota EQ W_TabAmor.W_Periodo
                            AND Extras.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Extras) THEN
            W_CuoExT = P_Cuota + Extras.Vr_CuoExtra.
    END.

    IF P_TipInt = 1 AND W_SdoCapTra GT 0 THEN
            W_TabAmor.W_AboInt = (W_SdoCapTra * P_Tasa).
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

    IF W_PerTra = P_Plazo AND W_TabAmor.W_SdoCap <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap).
        ELSE
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap.

        W_TabAmor.W_Cuota = W_TabAmor.W_AboCap + W_TabAmor.W_AboInt.
    END.
END PROCEDURE.

PROCEDURE P-AmortizacionGradiente:
    DEF VAR W_Dia AS INT INIT 0.

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
    END.
END PROCEDURE.

PROCEDURE P-AmortPeriodoGracia:
    IF P_Gracia = W_PerTra THEN DO:
        RUN HFDP IN W_ManFin (INPUT P_Monto,
                              INPUT P_Tasa,
                              INPUT P_Gracia,
                              OUTPUT W_SdoCapTra).

        W_TotInt = W_SdoCapTra - P_Monto.
        P_Monto = W_SdoCapTra.
        RETURN.
    END.
    ELSE
        IF P_Gracia > W_PerTra THEN
            RETURN.

    W_TabAmor.W_Cuota = P_Cuota.

    RUN HCCF IN W_ManFin (INPUT P_Monto,
                          INPUT P_Cuota,
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
    END.
END PROCEDURE.

PROCEDURE CambiarFecha:
    RUN Halla_FecVcto.R (INPUT fechaIni,
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

                        IF W_IDeduc EQ 4 THEN
                            ASSIGN W_Vdd = ROUND ((P_VrSoli * Deducible.Valor) / 100 ,0).
                        ELSE
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
        W_FecAnt = fechaIni.

    IF P_TipInt EQ 1 AND W_Periodo EQ 0 THEN
        RETURN.

    CREATE tabla.
    ASSIGN tabla.Agencia = p_age
           tabla.Cod_Credito = P_CodPro
           tabla.Cuota = W_Cuota
           tabla.Fec_Inic = W_FecAnt
           tabla.Fec_Vcto = W_Fecha
           tabla.Nit = P_Nit
           tabla.Nro_Cuota = W_Periodo
           tabla.Num_Credito = P_NroDcto
           tabla.Id_PdoMes = 0
           tabla.Plazo = P_Plazo
           tabla.Tasa = P_Tasa
           tabla.pagos_capitalAcum = W_AboCap
           tabla.pagos_IntAcum = W_AboInt.

    IF W_Primero THEN
        ASSIGN tabla.Id_PdoMes = 1
               tabla.Fec_Inic = fechaIni
               W_Primero = NO.
END PROCEDURE.



