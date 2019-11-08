DEFINE INPUT PARAMETER pNumId AS CHARACTER.
DEFINE INPUT PARAMETER pNumCredito AS INTEGER.
DEFINE INPUT PARAMETER pMontoCredito AS DECIMAL.
DEFINE INPUT PARAMETER pPlazo AS INTEGER.
DEFINE INPUT PARAMETER pTipoInteres AS INTEGER.
DEFINE INPUT PARAMETER pDiasPeriodicidad AS INTEGER.
DEFINE INPUT PARAMETER pTasaPeriodica AS DECIMAL.
DEFINE OUTPUT PARAMETER pNumUltimaCuota AS INTEGER.

DEFINE VAR vTasaPeriodica AS DECIMAL.
DEFINE VAR vSistema AS INTEGER.
DEFINE VAR vCodCredito AS INTEGER.
DEFINE VAR vNumCredito AS INTEGER.
DEFINE VAR vNombrePeriodicidad AS CHARACTER.
DEFINE VAR contCuota AS INTEGER.
DEFINE VAR diasPeriodo AS INTEGER.
DEFINE VAR vSdoProyectado AS DECIMAL.
DEFINE VAR fechaPagoCuota AS DATE.
DEFINE VAR numPeriodos AS INTEGER.
DEFINE VAR i AS INTEGER.
DEFINE VAR interesDeGracia AS DECIMAL.
DEFINE VAR numMeses AS INTEGER.
DEFINE VAR flagPrimeraCuota AS LOGICAL INITIAL YES.
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaAnterior AS DATE.
DEFINE VAR interesPendiente AS DECIMAL.

DEFINE TEMP-TABLE ttAmortizacion
    FIELD nroPeriodo AS INTEGER
    FIELD fecPago AS DATE FORMAT "99/99/9999"
    FIELD fecIni AS DATE
    FIELD cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD cuotaCapital AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD cuotaInteres AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD sdoProyectado AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

FOR EACH control_pagos WHERE control_pagos.nit = pNumId
                         AND control_pagos.num_credito = pNumCredito
                         AND CONTROL_pagos.fec_vcto >= TODAY /*+ diasPeriodo*/
                         AND control_pagos.id_PdoMes < 2 NO-LOCK BY control_pagos.nro_cuota:
    fechaIni = control_pagos.fec_ini.
    
    LEAVE.
END.

/* oakley */

vSistema = creditos.sistema.
vCodCredito = creditos.cod_credito.
vNumCredito = Creditos.Num_Credito.

vTasaPeriodica = creditos.tasa / (numPeriodos * 100).


FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                        AND amortizacion.num_credito = creditos.num_Credito
                        AND amortizacion.fec_pago > TODAY:
    DELETE amortizacion.
END.

/* oakley */

RUN P-Amortizacion.

/* *********************** Fin *********************** */


PROCEDURE P-Amortizacion:
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = vCodCredito NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Creditos) AND Pro_Creditos.Id_PerGracia = YES AND Dia_Gracia > 0 THEN
        ASSIGN interesDeGracia = ROUND((((pMontoCredito * vTasaPeriodica) / diasPeriodo) * Dia_Gracia) / pPlazo,0).

    FIND FIRST Creditos WHERE Creditos.Nit = pNumId
                          AND Creditos.Num_Credito = vNumCredito NO-ERROR.

    CREATE ttAmortizacion.
    ASSIGN fechaPagoCuota = fechaIni
           ttAmortizacion.fecIni = fechaIni
           ttAmortizacion.cuotaInteres = 0       
           ttAmortizacion.cuota = 0
           ttAmortizacion.cuotaCapital = 0
           ttAmortizacion.sdoProyectado = pMontoCredito
           vSdoProyectado = pMontoCredito.

    DO contCuota = 0 TO pPlazo BY 1:
        IF vSistema <> 2 AND contCuota > 0 THEN DO:
            IF contCuota > 1 THEN
                RUN CambiarFecha.

            IF MONTH(fechaPagoCuota) = 3 AND DAY(fechaPagoCuota) = 28 THEN
                fechaPagoCuota = fechaPagoCuota + 3.

            ttAmortizacion.fecIni = fechaPagoCuota.
            ttAmortizacion.fecPago = fechaPagoCuota.
        END.

        ttAmortizacion.cuota = creditos.cuota.
        ttAmortizacion.nroPeriodo = contCuota.

        IF (contCuota = 0) THEN DO:
            ttAmortizacion.cuota = 0.
            ttAmortizacion.fecPago = fechaIni.

            IF pTipoInteres = 2 THEN
                ttAmortizacion.cuotaInteres = pMontoCredito * vTasaPeriodica.

            fechaAnterior = fechaIni.
        END.
        ELSE DO:
            IF vSistema = 1 THEN
                RUN P-AmortCuotaFija.
            
            IF vSistema = 2 THEN DO:
                diasPeriodo = (pPlazo * diasPeriodo).

                IF contCuota <> 1 THEN
                    ttAmortizacion.fecIni = fechaPagoCuota.

                IF MONTH(fechaPagoCuota) = 3 AND DAY(fechaPagoCuota) = 28 THEN
                    fechaPagoCuota = fechaPagoCuota + 3.

                ttAmortizacion.fecPago = fechaPagoCuota.

                IF pTipoInteres = 1 THEN
                    ttAmortizacion.cuotaInteres = creditos.cuota - pMontoCredito.

                ttAmortizacion.cuotaCapital = pMontoCredito.
                ttAmortizacion.fecPago = fechaPagoCuota.
                ttAmortizacion.sdoProyectado = 0.
                vSdoProyectado = 0.
                
                LEAVE.
            END.
        END.

        IF contCuota < pPlazo THEN
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
                       INPUT sdoProyectado).
    END.
END PROCEDURE.


PROCEDURE P-AmortCuotaFija:
    DEFINE VAR cuotaPeriodo AS DECIMAL.

    cuotaPeriodo = creditos.cuota.

    FIND FIRST Extras WHERE extras.Nit = pNumId
                        AND extras.num_solicitud = creditos.num_solicitud
                        AND extras.cod_credito = creditos.cod_credito
                        AND extras.fec_vcto > fechaAnterior
                        AND extras.fec_vcto <= ttAmortizacion.fecPago
                        AND extras.fec_vcto > ADD-INTERVAL(ttAmortizacion.fecPago,-1,"months")
                        AND extras.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Extras) THEN
        cuotaPeriodo = creditos.cuota + extras.vr_CuoExtra.

    IF vSdoProyectado > 0 THEN DO:
        IF contCuota = 1 THEN
            ttAmortizacion.cuotaInteres = ((vSdoProyectado * vTasaPeriodica) / diasPeriodo) * (creditos.fec_pagAnti - creditos.fec_desembolso).
        ELSE
            ttAmortizacion.cuotaInteres = (vSdoProyectado * vTasaPeriodica).

        interesPendiente = interesPendiente + ttAmortizacion.cuotaInteres.
        ttAmortizacion.cuotaInteres = interesPendiente.

    END.

    ttAmortizacion.cuota = cuotaPeriodo.

    IF vSdoProyectado > 0 THEN DO:
        ttAmortizacion.cuotaCapital = ttAmortizacion.cuota - (ttAmortizacion.cuotaInteres + interesDeGracia).
        vSdoProyectado = vSdoProyectado - ttAmortizacion.cuotaCapital.
        ttAmortizacion.sdoProyectado = vSdoProyectado.
    END.

    /* Ajuste a la última cuota - 27/05/2010 - */
    IF contCuota = pPlazo AND ttAmortizacion.sdoProyectado <> 0 THEN DO:
        IF ttAmortizacion.sdoProyectado < 0 THEN
            ttAmortizacion.cuotaCapital = ttAmortizacion.cuotaCapital - ABS(ttAmortizacion.sdoProyectado).
        ELSE
            ttAmortizacion.cuotaCapital = ttAmortizacion.cuotaCapital + ttAmortizacion.sdoProyectado.

        ttAmortizacion.sdoProyectado = 0.
        ttAmortizacion.cuota = ttAmortizacion.cuotaCapital + ttAmortizacion.cuotaInteres.
    END.

    fechaAnterior = ttAmortizacion.fecPago.

END PROCEDURE.


PROCEDURE CambiarFecha:
    RUN Halla_FecVcto.r(INPUT fechaIni,
                        INPUT diasPeriodo,
                        INPUT fechaPagoCuota,
                        OUTPUT fechaPagoCuota).
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

    IF pTipoInteres = 1 AND pNroPeriodo = 0 THEN
        RETURN.

    CREATE PlanPagos.
    PlanPAgos.Nit = pNumId.
    PlanPagos.Num_Credito = vNumCredito.
    PlanPagos.Cod_Credito = vCodCredito.
    PlanPagos.Fec_Ini = pFecIni.
    PlanPagos.Fec_Vcto = pFecPago.
    PlanPagos.Fec_ProxPago = pFecPago.
    PlanPagos.Nro_Cuota = pNroPeriodo.
    PlanPagos.Cuota = pCuota.
    PlanPagos.Tasa = vTasaPeriodica.
    PlanPagos.Id_PdoMes = 0.

    CREATE CONTROL_pagos.
    control_pagos.Agencia = vAgencia.
    control_pagos.Cod_Credito = vCodCredito.
    control_pagos.Cuota = pCuota.
    control_pagos.Fec_Inic = pFecIni.
    control_pagos.Fec_Vcto = pFecPago.
    control_pagos.Nit = pNumId.
    control_pagos.Nro_Cuota = pNroPeriodo.
    control_pagos.Num_Credito = vNumCredito.
    control_pagos.Id_PdoMes = 0.
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
    amortizacion.nit = pNumId.
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
        amortizacion.fec_pago = creditos.fec_desembolso.
        amortizacion.nit = pNumId.
        amortizacion.nro_cuota = 0.
        amortizacion.num_credito = vNumCredito.
        amortizacion.sdo_capital = creditos.sdo_Capital.
    END.

END PROCEDURE.



