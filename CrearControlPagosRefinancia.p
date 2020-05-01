DEFINE INPUT PARAMETER nitCredito AS CHARACTER.
DEFINE INPUT PARAMETER numeroCredito AS INTEGER.
DEFINE INPUT PARAMETER pTasa AS DECIMAL.
DEFINE INPUT PARAMETER pMotivo AS CHARACTER.

DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR vMontoCredito AS DECIMAL.
DEFINE VAR vPlazo AS INTEGER.
DEFINE VAR vTasaPeriodica AS DECIMAL.
DEFINE VAR vTipoInteres AS INTEGER.
DEFINE VAR vSistema AS INTEGER.
DEFINE VAR vCodCredito AS INTEGER.
DEFINE VAR vNumCredito AS INTEGER.
DEFINE VAR vNombrePeriodicidad AS CHARACTER.
DEFINE VAR vSdoCapital AS DECIMAL.

DEFINE SHARED VAR W_ManFin AS HANDLE.
DEFINE VAR contCuota AS INTEGER.
DEFINE VAR diasPeriodo AS INTEGER.
DEFINE VAR W_SdoCapTra AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEFINE VAR numPeriodos AS INTEGER.
DEFINE VAR i AS INTEGER.
DEFINE VAR interesDeGracia AS DECIMAL.
DEFINE VAR numMeses AS INTEGER.
DEFINE VAR flagPrimeraCuota AS LOGICAL INITIAL YES.
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaAnterior AS DATE.
DEFINE VAR interesPendiente AS DECIMAL.
DEFINE VAR fechaDeTrabajo AS DATE.

DEFINE TEMP-TABLE ttAmortizacion
    FIELD nroPeriodo AS INTEGER
    FIELD fecPago AS DATE FORMAT "99/99/9999"
    FIELD fecIni AS DATE
    FIELD cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD cuotaCapital AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD cuotaInteres AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD saldoCapital AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

RUN rutfinan.r PERSISTENT SET w_manfin.

FIND FIRST creditos WHERE creditos.nit = nitCredito
                      AND creditos.num_credito = numeroCredito NO-ERROR.

IF creditos.fec_pago <= TODAY THEN
    fechaDeTrabajo = credito.fec_pago.
ELSE
    fechaDeTrabajo = TODAY.

RUN HallarPeriodo IN W_ManFin (INPUT creditos.per_pago,
                               INPUT creditos.plazo,
                               OUTPUT diasPeriodo,
                               OUTPUT numMeses,
                               OUTPUT numPeriodos,
                               OUTPUT vNombrePeriodicidad).

IF creditos.cod_credito <> 108 AND
   creditos.cod_credito <> 113 AND
   creditos.cod_credito <> 114 THEN DO:
    FOR EACH control_pagos WHERE control_pagos.nit = creditos.nit
                             AND control_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.fec_vcto >= fechaDeTrabajo
                             AND control_pagos.id_PdoMes < 2 NO-LOCK BY control_pagos.nro_cuota:
        fechaIni = control_pagos.fec_ini.
        LEAVE.
    END.
END.
ELSE
    fechaIni = creditos.fec_pago.

FOR EACH planpagos WHERE planPagos.nit = creditos.nit
                     AND planPagos.num_credito = creditos.num_credito:
    DELETE planPagos.
END.

FOR EACH control_pagos WHERE control_pagos.nit = creditos.nit
                         AND control_pagos.num_credito = creditos.num_credito:
    DELETE control_pagos.
END.

vAgencia = creditos.agencia.
vMontoCredito = creditos.sdo_capital.
vPlazo = creditos.plazo.
vTipoInteres = creditos.FOR_interes.
vSistema = creditos.sistema.
vCodCredito = creditos.cod_credito.
vNumCredito = Creditos.Num_Credito.
vSdoCapital = creditos.sdo_capital.

vTasaPeriodica = creditos.tasa / (numPeriodos * 100).


FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                        AND amortizacion.num_credito = creditos.num_Credito
                        AND amortizacion.fec_pago >= fechaDeTrabajo:
    DELETE amortizacion.
END.

RUN P-Amortizacion.

/* *********************** Fin *********************** */

PROCEDURE P-Amortizacion:
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = vCodCredito NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Creditos) AND Pro_Creditos.Id_PerGracia = YES AND Dia_Gracia > 0 THEN
        ASSIGN interesDeGracia = ROUND((((vMontoCredito * vTasaPeriodica) / diasPeriodo) * Dia_Gracia) / vPlazo,0).

    FIND FIRST Creditos WHERE Creditos.Nit = nitCredito
                          AND Creditos.Num_Credito = vNumCredito NO-ERROR.

    CREATE ttAmortizacion.
    ASSIGN W_FecTra = fechaIni
           ttAmortizacion.fecIni = fechaIni
           ttAmortizacion.cuotaInteres = 0       
           ttAmortizacion.cuota = 0
           ttAmortizacion.cuotaCapital = 0
           ttAmortizacion.saldoCapital = vMontoCredito
           W_SdoCapTra = vMontoCredito.

    DO contCuota = 0 TO vPlazo BY 1:
        IF vSistema <> 2 AND contCuota > 0 THEN DO:
            IF contCuota > 1 THEN
                RUN CambiarFecha.

            IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                w_fecTra = w_fecTra + 3.

            ttAmortizacion.fecIni = W_FecTra.
            ttAmortizacion.fecPago = W_FecTra.
        END.

        ttAmortizacion.cuota = creditos.cuota.
        ttAmortizacion.nroPeriodo = contCuota.

        IF (contCuota = 0) THEN DO:
            ttAmortizacion.cuota = 0.
            ttAmortizacion.fecPago = fechaIni.

            IF vTipoInteres = 2 THEN
                ttAmortizacion.cuotaInteres = vMontoCredito * vTasaPeriodica.

            fechaAnterior = fechaIni.
        END.
        ELSE DO:
            IF vSistema = 1 THEN
                RUN P-AmortCuotaFija.
            
            IF vSistema = 2 THEN DO:
                diasPeriodo = (vPlazo * diasPeriodo).

                IF contCuota <> 1 THEN
                    ttAmortizacion.fecIni = W_FecTra.

                IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                    w_fecTra = w_fecTra + 3.

                ttAmortizacion.fecPago = W_FecTra.

                IF vTipoInteres = 1 THEN
                    ttAmortizacion.cuotaInteres = creditos.cuota - vMontoCredito.

                ttAmortizacion.cuotaCapital = vMontoCredito.
                ttAmortizacion.fecPago = W_FecTra.
                ttAmortizacion.saldoCapital = 0.
                W_SdoCapTra = 0.
                
                LEAVE.
            END.
        END.

        IF contCuota < vPlazo THEN
            CREATE ttAmortizacion.

        I = I + 1.
    END.

    FOR EACH ttAmortizacion:
        RUN crear_plan(INPUT ttAmortizacion.nroPeriodo,
                       INPUT ttAmortizacion.fecIni,
                       INPUT ttAmortizacion.fecPago,
                       INPUT ttAmortizacion.cuota,
                       INPUT cuotaCapital,
                       INPUT cuotaInteres,
                       INPUT saldoCapital).
    END.
END PROCEDURE.


PROCEDURE P-AmortCuotaFija:
    DEFINE VAR cuotaPeriodo AS DECIMAL.

    cuotaPeriodo = creditos.cuota.

    FIND FIRST Extras WHERE extras.Nit = nitCredito
                        AND extras.num_solicitud = creditos.num_solicitud
                        AND extras.cod_credito = creditos.cod_credito
                        AND extras.fec_vcto > fechaAnterior
                        AND extras.fec_vcto <= ttAmortizacion.fecPago
                        AND extras.fec_vcto > ADD-INTERVAL(ttAmortizacion.fecPago,-1,"months")
                        AND extras.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Extras) THEN
        cuotaPeriodo = creditos.cuota + extras.vr_CuoExtra.

    IF W_SdoCapTra > 0 THEN DO:
        IF contCuota = 1 THEN
            ttAmortizacion.cuotaInteres = ((W_SdoCapTra * vTasaPeriodica) / diasPeriodo) * (creditos.fec_pagAnti - creditos.fec_desembolso).
        ELSE
            ttAmortizacion.cuotaInteres = (W_SdoCapTra * vTasaPeriodica).

        interesPendiente = interesPendiente + ttAmortizacion.cuotaInteres.
        ttAmortizacion.cuotaInteres = interesPendiente.

        IF ttAmortizacion.cuotaInteres > cuotaPeriodo THEN
            ttAmortizacion.cuotaInteres = cuotaPeriodo.

        interesPendiente = interesPendiente - ttAmortizacion.cuotaInteres.
    END.

    ttAmortizacion.cuota = cuotaPeriodo.

    IF W_SdoCapTra > 0 THEN DO:
        ttAmortizacion.cuotaCapital = ttAmortizacion.cuota - (ttAmortizacion.cuotaInteres + interesDeGracia).
        W_SdoCapTra = W_SdoCapTra - ttAmortizacion.cuotaCapital.
        ttAmortizacion.saldoCapital = W_SdoCapTra.
    END.

    IF contCuota = vPlazo AND ttAmortizacion.saldoCapital <> 0 THEN DO:
        IF ttAmortizacion.saldoCapital < 0 THEN
            ttAmortizacion.cuotaCapital = ttAmortizacion.cuotaCapital - ABS(ttAmortizacion.saldoCapital).
        ELSE
            ttAmortizacion.cuotaCapital = ttAmortizacion.cuotaCapital + ttAmortizacion.saldoCapital.

        ttAmortizacion.saldoCapital = 0.
        ttAmortizacion.cuota = ttAmortizacion.cuotaCapital + ttAmortizacion.cuotaInteres.
    END.

    fechaAnterior = ttAmortizacion.fecPago.

END PROCEDURE.


PROCEDURE CambiarFecha:
    RUN Halla_FecVcto.r(INPUT fechaIni,
                        INPUT diasPeriodo,
                        INPUT W_FecTra,
                        OUTPUT W_FecTra).
END PROCEDURE.


PROCEDURE Crear_Plan:
    DEFINE INPUT PARAMETER pNroPeriodo AS INTEGER.
    DEFINE INPUT PARAMETER pFecIni AS DATE.
    DEFINE INPUT PARAMETER pFecPago AS DATE.
    DEFINE INPUT PARAMETER pCuota AS DECIMAL.
    DEFINE INPUT PARAMETER vCuotaCapital AS DECIMAL.
    DEFINE INPUT PARAMETER vCuotaInteres AS DECIMAL.
    DEFINE INPUT PARAMETER vSaldoCapital AS DECIMAL.

    IF flagPrimeraCuota THEN
        pFecIni = fechaIni.

    IF vTipoInteres = 1 AND pNroPeriodo = 0 THEN
        RETURN.

    CREATE PlanPagos.
    PlanPagos.Agencia = vAgencia.
    PlanPAgos.Nit = nitCredito.
    PlanPagos.Num_Credito = vNumCredito.
    PlanPagos.Cod_Credito = vCodCredito.
    PlanPagos.Fec_Ini = pFecIni.
    PlanPagos.Fec_Vcto = pFecPago.
    PlanPagos.Fec_ProxPago = pFecPago.
    PlanPagos.Nro_Cuota = pNroPeriodo.
    PlanPagos.Cuota = pCuota.
    PlanPagos.Tasa = vTasaPeriodica.
    PlanPagos.Plazo = vPlazo.
    PlanPagos.Monto = vMontoCredito.
    PlanPagos.Id_PdoMes = 0.

    CREATE CONTROL_pagos.
    control_pagos.Agencia = vAgencia.
    control_pagos.Cod_Credito = vCodCredito.
    control_pagos.Cuota = pCuota.
    control_pagos.Fec_Inic = pFecIni.
    control_pagos.Fec_Vcto = pFecPago.
    control_pagos.Nit = nitCredito.
    control_pagos.Nro_Cuota = pNroPeriodo.
    control_pagos.Num_Credito = vNumCredito.
    control_pagos.Id_PdoMes = 0.
    control_pagos.Plazo = vPlazo.
    control_pagos.Tasa = vTasaPeriodica.
    control_pagos.pagos_capitalAcum = vCuotaCapital.
    control_pagos.pagos_IntAcum = vCuotaInteres.

    IF flagPrimeraCuota THEN DO:
        PlanPagos.Id_PdoMes = 1.
        control_pagos.Id_PdoMes = 1.
        PlanPagos.Fec_Ini = fechaIni.
        control_pagos.Fec_Inic = fechaIni.
        PlanPagos.Fec_ProxPago = pFecPago.
        flagPrimeraCuota = NO.
    END.

    CREATE amortizacion.
    amortizacion.cuota = pCuota.
    amortizacion.cuota_i = vCuotaInteres.
    amortizacion.cuota_k = vCuotaCapital.
    amortizacion.fec_pago = pFecPago.
    amortizacion.nit = nitCredito.
    amortizacion.nro_cuota = pNroPeriodo.
    amortizacion.num_credito = vNumCredito.
    amortizacion.sdo_capital = vSaldoCapital.

    /* Control a la última cuota */
    IF amortizacion.sdo_capital < 0 THEN DO:
        amortizacion.cuota = amortizacion.cuota + vSaldoCapital.
        amortizacion.cuota_k = amortizacion.cuota_k + vSaldoCapital.
        amortizacion.sdo_capital = 0.
    END.

    IF pNroPeriodo = 1 THEN DO:
        creditos.fec_pago = pfecPago.

        CREATE amortizacion.
        amortizacion.fec_pago = TODAY.
        amortizacion.nit = nitCredito.
        amortizacion.nro_cuota = 0.
        amortizacion.num_credito = vNumCredito.
        amortizacion.sdo_capital = creditos.sdo_Capital.
        amortizacion.novedad = pMotivo.
    END.

END PROCEDURE.



