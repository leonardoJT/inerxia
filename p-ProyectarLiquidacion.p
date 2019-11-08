DEFINE INPUT PARAMETER pId AS CHARACTER.
DEFINE INPUT PARAMETER pCodCredito AS INTEGER.
DEFINE INPUT PARAMETER pNumCredito AS INTEGER.
DEFINE INPUT PARAMETER pTasaNA AS DECIMAL.
DEFINE INPUT PARAMETER pSdoCapital AS DECIMAL.
DEFINE INPUT PARAMETER pIntCorriente AS DECIMAL.
DEFINE INPUT PARAMETER pIntMora AS DECIMAL.
DEFINE INPUT PARAMETER pFecPago AS DATE.
DEFINE INPUT PARAMETER pFec2 AS DATE.
/* ----------------------- */
DEFINE OUTPUT PARAMETER pIntCorrienteCausado AS DECIMAL.
DEFINE OUTPUT PARAMETER pIntMoraCausada AS DECIMAL.


/* Para pruebas */
/*DEFINE VAR pId AS CHARACTER INITIAL "17196647".
DEFINE VAR pCodCredito AS INTEGER INITIAL 17.
DEFINE VAR pNumCredito AS INTEGER INITIAL 24802.
DEFINE VAR pTasaNA AS DECIMAL.
DEFINE VAR pSdoCapital AS DECIMAL.
DEFINE VAR pIntCorriente AS DECIMAL.
DEFINE VAR pIntMora AS DECIMAL.
DEFINE VAR pFecPago AS DATE.
DEFINE VAR pFec2 AS DATE INITIAL 07/27/2019.
/* ----------------------- */
DEFINE VAR pIntCorrienteCausado AS DECIMAL.
DEFINE VAR pIntMoraCausada AS DECIMAL.
/* ----------------------- */

FIND FIRST creditos WHERE creditos.nit = pId
                      AND creditos.cod_credito = pCodCredito
                      AND creditos.num_credito = pNumCredito NO-LOCK NO-ERROR.

pTasaNA = creditos.tasa.
pSdoCapital = creditos.sdo_capital.
pIntCorriente = creditos.INT_corriente.
pIntMora = creditos.INT_morCobrar.
pFecPago = creditos.fec_pago.
/* ----------------------- */
*/

DEFINE VAR tasaDiariaMora AS DECIMAL.
DEFINE VAR baseLiquidarProyeccion AS DECIMAL.
DEFINE VAR baseLiquidarMora AS DECIMAL.
DEFINE VAR fecIter AS DATE.

{Incluido\VARIABLE.I "SHARED"}

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = pCodCredito NO-LOCK NO-ERROR.
IF AVAILABLE pro_creditos THEN DO:
    FIND FIRST Indicadores WHERE Indicadores.Indicador = pro_creditos.Cod_TasaMax
                             AND Indicadores.Estado = 1
                             AND Indicadores.FecVcto >= TODAY NO-LOCK NO-ERROR.
    IF AVAILABLE Indicadores THEN
        tasaDiariaMora = Indicadores.Tasa.
    ELSE DO:
        FIND FIRST entidad NO-LOCK NO-ERROR.

        FIND LAST Indicadores WHERE Indicadores.Indicador = Entidad.Ind_Usura
                                AND Indicadores.Estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Indicadores THEN
            tasaDiariaMora = Indicadores.Tasa.
    END.

    RUN TEA_to_TNP.r (INPUT tasaDiariaMora,
                      INPUT 12,
                      OUTPUT tasaDiariaMora).

    tasaDiariaMora = (tasaDiariaMora * 12) / 360.
END.

IF pCodCredito = 108 OR pCodCredito = 113 OR pCodCredito = 114 THEN
    baseLiquidarProyeccion = pSdoCapital + pIntCorriente.
ELSE
    baseLiquidarProyeccion = pSdoCapital.

pIntCorrienteCausado = pIntCorriente.
pIntMoraCausada = pIntMora.

DO fecIter = TODAY TO pFec2 - 1:
    /* Revisar si se debe liquidar interés de mora */
    IF pFecPago <= fecIter THEN DO:
        baseLiquidarMora = 0.

        IF pCodCredito <> 123 THEN DO:
            FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = pId
                                     AND CONTROL_pagos.num_credito = pNumCredito
                                     AND CONTROL_pagos.id_pdoMes < 2
                                     AND CONTROL_pagos.fec_Vcto <= fecIter NO-LOCK:
                baseLiquidarMora = baseLiquidarMora + CONTROL_pagos.pagos_capitalAcum.
            END.
        END.
        ELSE DO: /* Rotativos */
            FOR EACH facturacion WHERE facturacion.nit = pId
                                   AND facturacion.num_credito = pNumCredito
                                   AND facturacion.fec_pago <= fecIter
                                   AND facturacion.estado = 1 NO-LOCK BY facturacion.fec_pago DESCENDING:
                baseLiquidarMora = baseLiquidarMora + facturacion.capital - facturacion.pago_capital.
            END.
        END.

        pIntMoraCausada = pIntMoraCausada + ROUND(baseLiquidarMora * (tasaDiariaMora / 100),0).
    END.

    
    
    IF  pCodCredito = 108 OR pCodCredito = 113 OR pCodCredito = 114 THEN
        pIntCorrienteCausado = pIntCorrienteCausado + ROUND((baseLiquidarProyeccion - baseLiquidarMora) * (pTasaNA / 100 / 360),0).
    ELSE DO:
        IF MONTH(fecIter) = 2 AND DAY(fecIter) = 28 AND DAY(fecIter + 1) = 1 THEN
            pIntCorrienteCausado = pIntCorrienteCausado + ROUND((baseLiquidarProyeccion - baseLiquidarMora) * (pTasaNA / 100 / 360) * 3,0).
        ELSE DO:
            IF MONTH(fecIter) = 2 AND DAY(fecIter) = 29 THEN
                pIntCorrienteCausado = pIntCorrienteCausado + ROUND((baseLiquidarProyeccion - baseLiquidarMora) * (pTasaNA / 100 / 360) * 2,0).
            ELSE DO:
                IF DAY(fecIter) <> 31 THEN
                    pIntCorrienteCausado = pIntCorrienteCausado + ROUND((baseLiquidarProyeccion - baseLiquidarMora) * (pTasaNA / 100 / 360),0).
            END.
        END.
    END.

    IF pCodCredito = 108 OR pCodCredito = 113 OR pCodCredito = 114 THEN
        baseLiquidarProyeccion = pSdoCapital + pIntCorrienteCausado.
END.
