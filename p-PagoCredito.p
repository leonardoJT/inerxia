/*
- Se elimina el último parámetro
- Se agrega el parámetro de entrada fechaDePago
*/

DEFINE INPUT PARAMETER pAplicar AS LOGICAL. /*False NO Actualiza, True SI*/
DEFINE INPUT PARAMETER pCodCredito AS INTEGER.
DEFINE INPUT PARAMETER pClienteId AS CHARACTER.
DEFINE INPUT PARAMETER pNumCredito AS INTEGER.
DEFINE INPUT PARAMETER pValorAbono AS DECIMAL.
DEFINE INPUT PARAMETER pComprobante AS INTEGER.
DEFINE INPUT PARAMETER pNumDocumento AS INTEGER.
DEFINE INPUT PARAMETER pEfectivoCheque AS INTEGER FORMAT "9".   /*0 Efectivo, 1 Cheque*/
DEFINE INPUT PARAMETER pNumCuotas AS INTEGER.
DEFINE INPUT PARAMETER pfechaDePago AS DATE.
DEFINE INPUT PARAMETER pAtendido AS LOGICAL.
DEFINE OUTPUT PARAMETER pPoliza AS DECIMAL.
DEFINE OUTPUT PARAMETER pHonorarios AS DECIMAL.
DEFINE OUTPUT PARAMETER pCostas AS DECIMAL.
DEFINE OUTPUT PARAMETER pSeguroDeVida AS DECIMAL.
DEFINE OUTPUT PARAMETER pSeguroDeudor AS DECIMAL.
DEFINE OUTPUT PARAMETER pIntMoraDifCobro AS DECIMAL.
DEFINE OUTPUT PARAMETER pIntMora AS DECIMAL.
DEFINE OUTPUT PARAMETER pIntDifCobro AS DECIMAL.
DEFINE OUTPUT PARAMETER pIntCorriente AS DECIMAL.
DEFINE OUTPUT PARAMETER pIntAnticipado AS DECIMAL. /*Si P_IAntic(-) Neg.son cargos*/
DEFINE OUTPUT PARAMETER pCapital AS DECIMAL.
DEFINE OUTPUT PARAMETER pSobrante AS DECIMAL.
DEFINE OUTPUT PARAMETER pError AS LOGICAL.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFINE VAR cuentaCapital AS CHARACTER.
DEFINE VAR cuentaHonorarios AS CHARACTER.
DEFINE VAR cuentaPoliza AS CHARACTER.
DEFINE VAR cuentaCostas AS CHARACTER.
DEFINE VAR cuentaIntCorriente AS CHARACTER.
DEFINE VAR cuentaIngresoInteres AS CHARACTER.
DEFINE VAR cuentaIntMora AS CHARACTER.
DEFINE VAR cuentaIntMora_Ingreso AS CHARACTER.
DEFINE VAR cuentaIntDifCobro AS CHARACTER.
DEFINE VAR cuentaIntDifCobro_contrapartida AS CHARACTER.
DEFINE VAR diasPeriodo AS DECIMAL.
DEFINE VAR nroMeses AS INTEGER.
DEFINE VAR nroPeriodos AS INTEGER.
DEFINE VAR nombrePeriodicidad as CHARACTER.
DEFINE VAR vComentario AS CHARACTER.
DEFINE VAR saldoTotalDeuda AS DECIMAL.
DEFINE VAR flagMora AS LOGICAL.
DEFINE VAR intPeriodo AS DECIMAL.
DEFINE VAR diasDescontar AS INTEGER.
DEFINE VAR aCapitalTemp AS DECIMAL.
DEFINE VAR PorDist AS DECIMAL.
DEFINE VAR valContabilizar AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR codOperacion AS INTEGER.
DEFINE VAR reclasificaDifCobro AS LOGICAL.
DEFINE VAR devolucionIntereses AS LOGICAL.
DEFINE VAR i AS INTEGER.
DEFINE VAR INT_corrienteDev AS DECIMAL.
DEFINE VAR INT_difCobroDev AS DECIMAL.
DEFINE VAR INT_moraDev AS DECIMAL.
DEFINE VAR INT_moraDifCobroDev AS DECIMAL.

DEFINE TEMP-TABLE ttCuotasExtra
    FIELD nro_cuota AS INTEGER
    FIELD Vr_CuoExtra AS DECIMAL
    FIELD Fec_Vcto AS DATE
    FIELD Estado AS INTEGER INITIAL 1.

IF pNumCuotas = 0 THEN
    pNumCuotas = 1.

FIND FIRST Creditos WHERE Creditos.Nit = pClienteId
                      AND Creditos.Cod_Credito = pCodCredito
                      AND Creditos.Num_Credito = pNumCredito NO-ERROR.

RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                               INPUT Creditos.Plazo,
                               OUTPUT diasPeriodo,
                               OUTPUT nroMeses,
                               OUTPUT nroPeriodos,
                               OUTPUT nombrePeriodicidad).

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.

RUN ConfigCtas NO-ERROR. /* oakley */
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Error en configuración de Cuentas (p-pagCredito.p/ConfigCtas)"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    pSobrante = pValorAbono.
    RETURN ERROR.
END.

IF NOT pAplicar THEN
    FIND CURRENT Creditos NO-LOCK NO-ERROR.

Abono:
DO TRANSACTION ON ERROR UNDO Abono:
    IF pFechaDePago <> w_fecha THEN DO:
        FIND FIRST historico_saldos_cr WHERE historico_saldos_cr.cliente_id = pClienteId
                                         AND historico_saldos_cr.cod_credito = pCodCredito
                                         AND historico_saldos_cr.num_credito = pNumCredito
                                         AND historico_saldos_cr.fecha = pFechaDePago NO-LOCK NO-ERROR.
        IF AVAILABLE historico_saldos_cr THEN DO:
            IF historico_saldos_cr.INT_corriente < creditos.INT_corriente THEN
                INT_corrienteDev = creditos.INT_corriente - historico_saldos_cr.INT_corriente.

            IF historico_saldos_cr.INT_difCobro < creditos.INT_difCobro THEN
                INT_difCobroDev = creditos.INT_difCobro - historico_saldos_cr.INT_difCobro.

            IF historico_saldos_cr.INT_mora < creditos.INT_morCobrar THEN
                INT_moraDev = creditos.INT_morCobrar - historico_saldos_cr.INT_mora.

            IF historico_saldos_cr.INT_moraDifCobro < creditos.INT_moraDifCob THEN
                INT_moraDifCobroDev = creditos.INT_moraDifCob - historico_saldos_cr.INT_moraDifCobro.
        END.
    END.

    saldoTotalDeuda = Creditos.Honorarios +
                      Creditos.Costas +
                      Creditos.Polizas +
                      Creditos.Int_MorCobrar +
                      Creditos.Int_MoraDifCob +
                      Creditos.Int_Corrientes +
                      Creditos.Int_DifCobro +
                      Creditos.Sdo_Capital -
                      Creditos.Int_Anticipado -
                      INT_corrienteDev -
                      INT_moraDev -
                      INT_difCobroDev -
                      INT_moraDifCobroDev.

    PorDist = pValorAbono.

    /* Si el pago supera el valor adeudado, cancelamos completamente el crédito */

    IF PorDist >= saldoTotalDeuda THEN DO:
        ASSIGN pSobrante = PorDist - saldoTotalDeuda
               pPoliza = Creditos.Polizas
               pHonorarios = Creditos.Honorarios
               pCostas = Creditos.Costas
               pIntMora = Creditos.Int_MorCobrar - INT_moraDev
               pIntMoraDifCobro = Creditos.Int_MoraDifCob - INT_moraDifCobroDev
               pIntDifCobro = Creditos.Int_DifCobro - INT_difCobroDev
               pIntCorriente = Creditos.Int_Corrientes - INT_corrienteDev
               pIntAnticipado = Creditos.Int_Anticipado * -1   /*para cargar por cancelac.*/
               pCapital = Creditos.Sdo_Capital.

        IF NOT pAplicar THEN
            RETURN.

        /* Si el proceso es atendido y el pago supera el saldo total la transacción se rechaza */
        IF pAtendido = TRUE AND PorDist > saldoTotalDeuda THEN DO:
            MESSAGE "El pago supera el valor de la deuda (" + STRING(saldoTotalDeuda,"$>>>,>>>,>>>,>>9.99") + ")" SKIP
                    "No se permite la operación..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            pError = TRUE.

            RETURN ERROR.
        END.
        
        ASSIGN Creditos.Polizas = 0
               Creditos.Honorarios = 0
               Creditos.Costas = 0
               Creditos.Int_MorCobrar = 0
               Creditos.Int_MoraDifCob = 0
               Creditos.Int_DifCobro = 0
               Creditos.Int_Corrientes = 0
               Creditos.Int_Anticipado = 0
               Creditos.Sdo_Capital = 0
               Creditos.Fec_CanceTot = W_Fecha
               Creditos.Fec_UltPag = W_Fecha
               Creditos.Cuo_Pagadas = Creditos.Plazo
               creditos.seg_vida = 0
               creditos.seg_cartera = 0
               creditos.val_atraso = 0
               creditos.Dias_Atraso = 0
               creditos.cuo_atraso = 0.

        /* Para créditos normales lo marco como cancelado. Si se trata de un cupo rotativo lo dejo activo y reinicio las variables */
        IF creditos.cod_credito <> 123 THEN
            Creditos.Estado = 3.
        ELSE DO:
            creditos.cuota = 0.
            creditos.fec_pago = ?.
            creditos.val_atraso = 0.
            creditos.dias_atraso = 0.

            FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                                   AND facturacion.num_credito = creditos.num_credito
                                   AND facturacion.estado = 1 BY facturacion.fec_pago:
                facturacion.estado = 2.
            END.
        END.

        RUN MovCreditos NO-ERROR.

        RETURN.
    END.
    ELSE DO:
        INT_corrienteDev = 0.
        
        IF creditos.cod_credito = 123 AND creditos.fec_pago < TODAY AND creditos.dias_atraso = 0 THEN
            INT_moraDev = INT_moraDev.
        ELSE
            INT_moraDev = 0.

        INT_difCobroDev = 0.
        INT_moraDifCobroDev = 0.
    END.

    /* Si no es un pago total se continua con el proceso normal de distribución */

    IF pAtendido = TRUE AND (creditos.cod_credito = 17 OR creditos.cod_credito = 22 OR creditos.cod_credito = 27 OR creditos.cod_credito = 57) AND creditos.dias_atraso = 0 AND creditos.detalle_estado <> 2 /* Para Créditos Congelados no debe hacer esta consulta */ THEN DO:
        FIND FIRST cfg_creditos NO-LOCK NO-ERROR.
        IF AVAILABLE cfg_creditos AND pAplicar = TRUE THEN DO:
            IF PorDist >= cfg_creditos.montoMinimoRefinanciacion THEN DO:
                MESSAGE "Usted está abonando al crédito un valor superior al monto" skip
                        "mínimo estipulado (" + string(cfg_creditos.montoMinimoRefinanciacion,"$>>>,>>>,>>9") + ")." skip
                        "Desea recalcular la cuota?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE recalculaCuota AS LOGICAL.
            END.
        END.
    END.

    /* Honorarios */
    IF PorDist > 0 THEN DO:
        IF PorDist >= Creditos.Honorarios AND creditos.honorarios > 0 THEN
            ASSIGN pHonorarios = Creditos.Honorarios
                   PorDist = PorDist - pHonorarios.
        ELSE
            IF Creditos.Honorarios > 0 THEN
                ASSIGN pHonorarios = PorDist
                       PorDist = 0.
    END.

    /* Costas */
    IF PorDist > 0 THEN DO:
        IF PorDist >= Creditos.Costas AND creditos.costas > 0 THEN
            ASSIGN pCostas = Creditos.Costas
                   PorDist = PorDist - pCostas.
        ELSE
            IF Creditos.Costas > 0 THEN
                ASSIGN pCostas = PorDist
                       PorDist = 0.
    END.

    /* Pólizas */
    IF PorDist GT 0 THEN DO:
        IF PorDist >= Creditos.Polizas AND Creditos.Polizas > 0 THEN
            ASSIGN pPoliza = Creditos.Polizas
                   PorDist = PorDist - pPoliza.
        ELSE
            IF Creditos.Polizas GT 0 THEN
                ASSIGN pPoliza = PorDist
                       PorDist = 0.
    END.

    IF PorDist <= 0 THEN DO:
        IF NOT pAplicar THEN
            RETURN.

        ASSIGN Creditos.Polizas = Creditos.Polizas - pPoliza
               Creditos.Honorarios = Creditos.Honorarios - pHonorarios
               Creditos.Costas = Creditos.Costas - pCostas.

        RUN MovCreditos  NO-ERROR.

        RETURN.
    END.

    IF creditos.cod_credito = 123 THEN
        FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                                 AND facturacion.num_credito = creditos.num_credito
                                 AND facturacion.estado = 1 NO-LOCK NO-ERROR.

    IF creditos.cod_credito <> 123 THEN DO: /* Para los créditos distintos al Rotativo */
        /* Mora de difícil cobro */
        IF PorDist > 0 AND creditos.int_moraDifCob > 0 THEN DO:
            flagMora = TRUE.

            IF porDist > creditos.INT_moraDifCob THEN
                ASSIGN pIntMoraDifCobro = creditos.INT_moraDifCob
                       PorDist = PorDist - pIntMoraDifCobro.
            ELSE
                ASSIGN pIntMoraDifCobro = PorDist
                       PorDist = 0.
        END.
        
        /* Mora */
        IF PorDist > 0 AND creditos.int_morCobrar > 0 THEN DO:
            flagMora = TRUE.

            IF porDist > creditos.INT_morCobrar THEN
                ASSIGN pIntMora = creditos.INT_morCobrar
                       PorDist = PorDist - pIntMora.
            ELSE
                ASSIGN pIntMora = PorDist
                       PorDist = 0.
        END.

        /* Interés de difícil cobro */
        IF PorDist >= creditos.INT_difCobro THEN
            ASSIGN pIntDifCobro = creditos.INT_difCobro
                   PorDist = PorDist - pIntDifCobro.
        ELSE
            ASSIGN pIntDifCobro = PorDist
                   PorDist = 0.
        
        /* Interés corriente */
        IF PorDist >= creditos.INT_corriente THEN
            ASSIGN pIntCorriente = creditos.INT_corriente
                   PorDist = PorDist - pIntCorriente.
        ELSE
            ASSIGN pIntCorriente = PorDist
                   PorDist = 0.

        /* Si es un crédito en mora, no se cobran los intereses corrientes del periodo que está corriendo (excepto los rotativos) */
        IF flagMora = TRUE AND creditos.cod_credito <> 123 THEN DO:
            FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                       AND CONTROL_pagos.num_credito = creditos.num_credito
                                       AND CONTROL_pagos.fec_Vcto >= w_fecha USE-INDEX ppal4 NO-LOCK NO-ERROR.
            IF AVAILABLE CONTROL_pagos THEN DO:
                diasDescontar = diasPeriodo - (CONTROL_pagos.Fec_Vcto - w_fecha).

                IF diasDescontar < 0 THEN
                    diasDescontar = 0.

                intPeriodo = ROUND((creditos.sdo_Capital / 100) * (creditos.tasa / 360) * diasDescontar,0).
                
                IF pIntCorriente >= intPeriodo AND intPeriodo > 0 THEN DO:
                    pIntCorriente = pIntCorriente - intPeriodo.
                    PorDist = PorDist + intPeriodo.
                END.
            END.
        END.
    END.
    ELSE DO: /* Para los créditos Rotativos */
        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito
                               AND facturacion.estado = 1 BY facturacion.fec_pago:
            /* Mora */
            IF PorDist > 0 AND facturacion.INT_mora - facturacion.pago_mora > 0 THEN DO:
                IF porDist > facturacion.INT_mora - facturacion.pago_mora THEN
                    ASSIGN pIntMora = pIntMora + (facturacion.INT_mora - facturacion.pago_mora)
                           PorDist = PorDist - (facturacion.INT_mora - facturacion.pago_mora).
                ELSE
                    ASSIGN pIntMora = pIntMora + PorDist
                           PorDist = 0.

                /* Revisamos si al crédito se le puede o no cobrar la mora */
                IF pIntMora > 0 AND creditos.for_pago = 2 THEN DO:
                    FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
                    FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.

                    IF creditos.dias_atraso <= empresas.dias_gracia AND empresas.dias_gracia > 0 THEN
                        ASSIGN porDist = porDist + pIntMora
                               pIntMora = 0.
                END.
            END.

            /* Interés difícil cobro */
            IF PorDist >= facturacion.INT_difCobro - facturacion.pago_intDifCobro THEN
                ASSIGN pIntDifCobro = pIntDifCobro + (facturacion.INT_difCobro - facturacion.pago_intDifCobro)
                       PorDist = PorDist - (facturacion.INT_difCobro - facturacion.pago_intDifCobro).
            ELSE
                ASSIGN pIntDifCobro = pIntDifCobro + PorDist
                       PorDist = 0.

            /* Revisamos que el interés de difícil cobro sea mayor que el valor que se va a distribuir */
            IF creditos.INT_difCobro < pIntDifCobro THEN DO:
                /* Devolvemos el excedente para que se siga distribuyendo */
                PorDist = PorDist + (pIntDifCobro - creditos.INT_difCobro).

                /* Ajustamos el interés corriente que se pide en la factura, porque el crédito no tiene todo causado */
                IF pAplicar = TRUE THEN
                    facturacion.INT_difCobro = facturacion.INT_difCobro - (pIntDifCobro - creditos.INT_difCobro).

                pIntDifCobro = creditos.INT_difCobro.
            END.

            /* Interés corriente */
            IF PorDist >= facturacion.INT_corriente - facturacion.pago_intCorriente THEN
                ASSIGN pIntCorriente = pIntCorriente + (facturacion.INT_corriente - facturacion.pago_intCorriente)
                       PorDist = PorDist - (facturacion.INT_corriente - facturacion.pago_intCorriente).
            ELSE
                ASSIGN pIntCorriente = pIntCorriente + PorDist
                       PorDist = 0.

            /* Revisamos que el interés corriente sea mayor que el valor que se va a distribuir */
            IF creditos.INT_corriente < pIntCorriente THEN DO:

                /* Devolvemos el excedente para que se siga distribuyendo */
                PorDist = PorDist + (pIntCorriente - creditos.INT_corriente).

                /* Ajustamos el interés corriente que se pide en la factura, porque el crédito no tiene todo causado */
                IF pAplicar = TRUE THEN
                    facturacion.INT_corriente = facturacion.INT_corriente - (pIntCorriente - creditos.INT_corriente).

                pIntCorriente = creditos.INT_corriente.
            END.

            /* Capital */
            IF porDist > 0 THEN DO:
                IF PorDist >= facturacion.capital - facturacion.pago_capital THEN
                    ASSIGN pCapital = pCapital + (facturacion.capital - facturacion.pago_capital)
                           PorDist = PorDist - (facturacion.capital - facturacion.pago_capital).
                ELSE
                    ASSIGN pCapital = pCapital + PorDist
                           PorDist = 0.
            END.
        END.
    END.

    /* Capital */
    IF porDist > 0 THEN DO:
        aCapitalTemp = pCapital.

        IF PorDist >= (creditos.sdo_capital - aCapitalTemp) THEN
            ASSIGN pCapital = pCapital + (creditos.sdo_capital - aCapitalTemp)
                   PorDist = PorDist - (creditos.sdo_capital - aCapitalTemp).
        ELSE
            ASSIGN pCapital = pCapital + PorDist
                   PorDist = 0.
    END.

    /* Si es un rotativo y luego de abonar el pago adicional al capital aun queda dinero para distribuir, continúo distribuyendo los demás conceptos */
    IF creditos.cod_credito = 123 AND porDist > 0 THEN DO:
        /* Mora */
        IF creditos.INT_morCobrar - pIntMora > 0 THEN DO:
            IF porDist > creditos.INT_morCobrar - pIntMora THEN
                ASSIGN PorDist = PorDist - (creditos.INT_morCobrar - pIntMora)
                       pIntMora = pIntMora + (creditos.INT_morCobrar - pIntMora).
            ELSE
                ASSIGN pIntMora = pIntMora + PorDist
                       PorDist = 0.

            /* Revisamos si al crédito se le puede o no cobrar la mora */
            IF pIntMora > 0 AND creditos.for_pago = 2 THEN DO:
                FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
                FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.

                IF creditos.dias_atraso <= empresas.dias_gracia AND empresas.dias_gracia > 0 THEN
                    ASSIGN porDist = porDist + pIntMora
                           pIntMora = 0.
            END.
        END.

        /* Interés difícil cobro */
        IF PorDist >= creditos.INT_difCobro - pIntDifCobro THEN
            ASSIGN PorDist = PorDist - (creditos.INT_difCobro - pIntDifCobro)
                   pIntDifCobro = pIntDifCobro + (creditos.INT_difCobro - pIntDifCobro).
        ELSE
            ASSIGN pIntDifCobro = pIntDifCobro + PorDist
                   PorDist = 0.

        /* Interés corriente */
        IF PorDist >= creditos.INT_corriente - pIntCorriente THEN
            ASSIGN PorDist = PorDist - (creditos.INT_corriente - pIntCorriente)
                   pIntCorriente = pIntCorriente + (creditos.INT_corriente - pIntCorriente).
        ELSE
            ASSIGN pIntCorriente = pIntCorriente + PorDist
                   PorDist = 0.
    END.

    /* Si luego de distribuir todo el valor aun sobra dinero, se devuelve como un sobrante para que se tome la decisión desde el programa fuente */
    IF porDist > 0 THEN
        pSobrante = PorDist.
    ELSE
        pSobrante = 0.

    IF NOT pAplicar THEN
        RETURN.

    ASSIGN Creditos.Polizas = Creditos.Polizas - pPoliza
           Creditos.Honorarios = Creditos.Honorarios - pHonorarios
           Creditos.Costas = Creditos.Costas - pCostas
           Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - pIntMora
           Creditos.Int_MoraDifCob = Creditos.Int_MoraDifCob - pIntMoraDifCobro
           Creditos.Sdo_Capital = Creditos.Sdo_Capital - pCapital
           creditos.INT_difCobro = creditos.INT_difCobro - pIntDifCobro
           creditos.INT_corriente = creditos.INT_corriente - pIntCorriente
           creditos.INT_anticipado = creditos.INT_anticipado + pIntAnticipado
           creditos.seg_Vida = creditos.seg_Vida - pSeguroDeVida
           creditos.seg_Cartera = creditos.seg_cartera - pSeguroDeudor.

    RUN MovCreditos NO-ERROR.

    IF creditos.cod_credito <> 123 THEN DO:
        IF creditos.detalle_estado <> 2 THEN /* Si el crédito no está congelado */
            RUN A_controlpagos NO-ERROR.
    END.
    ELSE
        RUN ActualizaFacturacion.

    RETURN.
END.

PROCEDURE MovCreditos:
    /* ---------------------------------------------------------------------------- */
    Creditos.Sdo_CapPag = Creditos.Sdo_CapPag + pCapital.
    Creditos.Sdo_IntMor = Creditos.Sdo_IntMor + (pIntMora + pIntMoraDifCobro).
    Creditos.Sdo_IntPag = Creditos.Sdo_IntPag + (pIntDifCobro + pIntCorriente + pIntAnticipado).
    Creditos.Fec_UltPag = W_Fecha.

    IF Creditos.Sdo_Capital > 0 THEN DO:
        IF Creditos.Cuo_Pagadas >= Creditos.Plazo THEN
            Creditos.Cuo_Pagadas = Creditos.Plazo - 1.
    END.
    /* ---------------------------------------------------------------------------- */

    IF pPoliza > 0 THEN DO:
        vCuenta = cuentaPoliza.
        valContabilizar = pPoliza.
        codOperacion = 020101006.
        vComentario = "Abono Pólizas".
        RUN GrabaMov NO-ERROR.
    END.

    IF pCostas > 0 THEN DO:
        vCuenta = cuentaCostas.
        valContabilizar = pCostas.
        codOperacion = 020101008.
        vComentario = "Abono CostasJ".
        RUN GrabaMov NO-ERROR.
    END.

    IF pHonorarios > 0 THEN DO:
        vCuenta = cuentaHonorarios.
        valContabilizar = pHonorarios.
        codOperacion = 020101007.
        vComentario = "Abono Honorarios".
        RUN GrabaMov NO-ERROR.    
    END.

    IF INT_moraDev > 0 THEN DO:
        vCuenta = cuentaIntMora.
        valContabilizar = INT_moraDev.
        vComentario = "RevIntMora por pago en fecha anterior".
        devolucionIntereses = TRUE.
        RUN grabaMov NO-ERROR.

        vCuenta = cuentaIntMora_Ingreso.
        devolucionIntereses = TRUE.
        RUN grabaMov NO-ERROR.
        mov_contable.cr = 0.
        mov_contable.db = valContabilizar.
    END.
    
    IF pIntMora > 0 THEN DO:
        vCuenta = cuentaIntMora.
        valContabilizar = pIntMora.
        vComentario = "Abono IntMora".
        RUN GrabaMov NO-ERROR.
    END.

    IF INT_moraDifCobroDev > 0 THEN DO:
        vCuenta = cuentaIntDifCobro.
        valContabilizar = INT_moraDifCobroDev.
        vComentario = "RevIntMoraDifCobro por pago en fecha anterior".
        devolucionIntereses = TRUE.
        RUN grabaMov NO-ERROR.

        vCuenta = cuentaIntDifCobro_contrapartida.
        devolucionIntereses = TRUE.
        RUN grabaMov NO-ERROR.
        mov_contable.cr = 0.
        mov_contable.db = valContabilizar.
    END.


    IF pIntMoraDifCobro > 0 THEN DO:
        vCuenta = cuentaIntMora_Ingreso.
        valContabilizar = pIntMoraDifCobro.
        vComentario = "Abono IntMora-DifCobro". 
        RUN GrabaMov NO-ERROR.

        vCuenta = cuentaIntDifCobro.
        reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.

        vCuenta = cuentaIntDifCobro_contrapartida.
        reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.
        Mov_Contable.Cr = 0.
        Mov_Contable.Db = valContabilizar.
    END.

    IF INT_CorrienteDev > 0 THEN DO:
        vCuenta = cuentaIntCorriente.
        valContabilizar = INT_CorrienteDev.
        vComentario = "RevIntCorriente por pago en fecha anterior".
        devolucionIntereses = TRUE.
        
        RUN grabaMov NO-ERROR.

        vCuenta = cuentaIngresoInteres.
        devolucionIntereses = TRUE.
        
        RUN grabaMov NO-ERROR.
        mov_contable.cr = 0.
        mov_contable.db = valContabilizar.
    END.

    IF pIntCorriente > 0 THEN DO:
        vCuenta = cuentaIntCorriente.
        valContabilizar = pIntCorriente.
        codOperacion = 020101003.
        vComentario = "Abono IntCorriente".
        RUN GrabaMov NO-ERROR.
    END.

    IF INT_difCobroDev > 0 THEN DO:
        vCuenta = cuentaIntDifCobro.
        valContabilizar = INT_difCobroDev.
        vComentario = "RevIntDifCobro por pago en fecha anterior".
        devolucionIntereses = TRUE.
        
        RUN grabaMov NO-ERROR.

        vCuenta = cuentaIntDifCobro_contrapartida.
        devolucionIntereses = TRUE.
        
        RUN grabaMov NO-ERROR.
        mov_contable.cr = 0.
        mov_contable.db = valContabilizar.
    END.

    IF pIntDifCobro > 0 THEN DO:
        valContabilizar = pIntDifCobro.
        
        vCuenta = cuentaIntDifCobro.
        codOperacion = 020101004.
        vComentario = "Reclasif-Contingente".
        reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.

        vCuenta = cuentaIntDifCobro_contrapartida.
        reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.
        Mov_Contable.Cr = 0.
        Mov_Contable.Db = valContabilizar.

        valContabilizar = pIntDifCobro.

        vCuenta = cuentaIntCorriente.
        vComentario = "Reclasif-Contingente".
        reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.
        Mov_Contable.Cr = 0.
        Mov_Contable.Db = valContabilizar.

        vCuenta = cuentaIngresoInteres.
        vComentario = "Reclasif-Contingente".
        reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.

        vCuenta = cuentaIntCorriente.
        codOperacion = 020101003.
        vComentario = "Abono IntCorriente".
        RUN GrabaMov NO-ERROR.
    END.

    IF pCapital > 0 THEN DO:
        vCuenta = cuentaCapital.
        valContabilizar = pCapital.
        codOperacion = 020101001.
        vComentario = "Abono Capital".
        RUN GrabaMov NO-ERROR.
    END.

END PROCEDURE.


PROCEDURE GrabaMov:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Creditos.Agencia
           Mov_Contable.Cuenta = vCuenta
           Mov_Contable.Nit = Creditos.Nit
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = vComentario
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Cen_Costos = W_Cencosgral
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Comprobante = pComprobante
           Mov_Contable.Num_Documento = INTEGER(pNumDocumento)
           Mov_Contable.Doc_Refer = STRING(Creditos.Pagare)
           Mov_Contable.Enlace = STRING(Creditos.Num_Credito)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.Cr = valContabilizar NO-ERROR.

    IF reclasificaDifCobro = TRUE THEN DO:
        reclasificaDifCobro = FALSE.
        RETURN.
    END.

    IF devolucionIntereses = TRUE THEN DO:
        devolucionIntereses = FALSE.
        RETURN.
    END.

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Ofi_Destino = Creditos.agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = TIME
           Mov_Creditos.Num_Documento = string(pNumDocumento)
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Val_Efectivo = valContabilizar
           Mov_Creditos.Cpte = pComprobante
           Mov_Creditos.Cod_Operacion = codOperacion
           Mov_Creditos.Descrip = vComentario + "-Efectivo".

    IF pEfectivoCheque = 1 THEN DO:
        Mov_Creditos.Val_Efectivo = 0.
        Mov_Creditos.Val_Cheque = valContabilizar.
        Mov_Creditos.Descrip = vComentario + "-Cheque".
    END.

    mov_creditos.descrip = mov_creditos.descrip.

END PROCEDURE.


PROCEDURE ConfigCtas:
    FIND FIRST CortoLargo WHERE CortoLargo.Agencia = Creditos.Agencia
                            AND CortoLargo.Clase_Producto = 2
                            AND CortoLargo.Cod_Producto = Creditos.Cod_Credito
                            AND CortoLargo.Plazo_Inicial >= 0 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(CortoLargo) THEN DO:
        MESSAGE "No se encontró la configuración contable para la línea" Creditos.Cod_Credito SKIP
                "en la agencia" STRING(Creditos.Agencia) + ". No se permite el recaudo..."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.

        pSobrante = pValorAbono.
        RETURN ERROR.
    END.

    
    /* oakley */

    FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto = 2
                           AND Liqui_Int.Cod_Producto = CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Liqui_Int) THEN DO:
        MESSAGE "No existe la configuración en Liqui_Int para la línea de crédito" Creditos.Cod_Credito SKIP
                "No se permite el recaudo..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        pSobrante = pValorAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización del capital */
    IF CortoLargo.Cta_AsoAd  <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_AsoAd."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        pSobrante = pValorAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de honorarios */
    IF CortoLargo.Cta_HonorariosDB  <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_HonorariosDB."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        pSobrante = pValorAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de Pólizas */
    IF CortoLargo.Cta_PolizasDB  <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_PolizasDB."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        pSobrante = pValorAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de costas */
    IF CortoLargo.Cta_CostasDB <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_CostasDB."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        pSobrante = pValorAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de los intereses corriente */
    IF AVAILABLE Liqui_Int THEN DO:
        IF Liqui_Int.CtaDb_LiqAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaDb_LiqAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            pSobrante = pValorAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaCr_LiqAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaCr_LiqAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            pSobrante = pValorAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaDb_MoraAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaDb_MoraAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            pSobrante = pValorAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaCr_MoraAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaCr_MoraAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            pSobrante = pValorAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaDb_DifCobAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaDb_DifCobAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            pSobrante = pValorAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaCr_DifCobAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaCr_DifCobAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            pSobrante = pValorAbono.
            RETURN ERROR.
        END.

        IF creditos.FOR_pago = 2 THEN /* Nómina */
            ASSIGN cuentaIntCorriente = Liqui_Int.CtaDb_LiqAso
                   cuentaIngresoInteres = Liqui_Int.CtaCr_LiqAso
                   cuentaIntMora = Liqui_Int.CtaDb_MoraAso
                   cuentaIntMora_Ingreso = Liqui_Int.CtaCr_MoraAso
                   cuentaIntDifCobro = Liqui_Int.CtaDb_DifCobAso
                   cuentaIntDifCobro_contrapartida = Liqui_Int.CtaCr_DifCobAso.
        ELSE /* Caja */
            ASSIGN cuentaIntCorriente = Liqui_Int.CtaDb_Liq
                   cuentaIngresoInteres = Liqui_Int.CtaCr_Liq
                   cuentaIntMora = Liqui_Int.CtaDb_Mora
                   cuentaIntMora_Ingreso = Liqui_Int.CtaCr_Mora
                   cuentaIntDifCobro = Liqui_Int.CtaDb_DifCob
                   cuentaIntDifCobro_contrapartida = Liqui_Int.CtaCr_DifCob.
    END.

    IF creditos.FOR_pago = 2 THEN
        cuentaCapital = CortoLargo.Cta_AsoAd.
    ELSE
        cuentaCapital = CortoLargo.Cta_NoaAd.

    ASSIGN cuentaHonorarios = CortoLargo.Cta_HonorariosDB
           cuentaPoliza = CortoLargo.Cta_PolizasDB
           cuentaCostas = CortoLargo.Cta_CostasDB.

END PROCEDURE.

PROCEDURE A_controlpagos:
    DEFINE VAR nrocuo AS INTEGER.
    DEFINE VAR fechaAux AS DATE.
    DEFINE VAR diasAsumar AS INTEGER.
    DEFINE VAR diasAtrasoAux AS INTEGER.
    DEFINE VAR ValorCredito AS DECIMAL.
    DEFINE VAR flagfecPago AS LOGICAL.
    DEFINE VAR valorAbono AS DECIMAL.
    DEFINE VAR saldoProyectado AS DECIMAL.
    DEFINE VAR moraAux AS DECIMAL.
    DEFINE VAR cuotas_aAdelantar AS INTEGER.
    DEFINE VAR flagCierraCuota AS LOGICAL INITIAL YES.

    cuotas_aAdelantar = pNumCuotas - 1.
    
    valorAbono = pValorAbono.
    moraAux = pIntMora.

    /* Se calculan los campos creditos.dias_atraso, creditos.cuo_atraso, creditos.cuo_pagadas y creditos.val_atraso */
    creditos.val_atraso = 0.
    creditos.cuo_atraso = 0.
    creditos.dias_atraso = 0.
        
    /* Cerramos las cuotas a que haya lugar */

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
        IF pAtendido = TRUE AND CONTROL_pagos.fec_vcto >= w_fecha AND valorAbono > CONTROL_pagos.cuota - CONTROL_pagos.cap_pagado THEN DO:
            MESSAGE "Se está pagando un monto superior al valor de la cuota" SKIP
                    "corriente (" + STRING(CONTROL_pagos.fec_vcto,"99/99/9999") + ") para este crédito." SKIP
                    "Desea pagar la cuota actual...? (seleccione NO si se trata de un pago extra)"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE flagCierraCuota.

            IF flagCierraCuota = NO THEN DO:
                CREATE Mov_Creditos.
                Mov_Creditos.Agencia = Creditos.Agencia.
                Mov_Creditos.Cod_Credito = Creditos.Cod_Credito.
                Mov_Creditos.Nit = Creditos.Nit.
                Mov_Creditos.Num_Credito = Creditos.Num_Credito.
                Mov_Creditos.Ofi_Destino = Creditos.agencia.
                Mov_Creditos.Ofi_Fuente = W_Agencia.
                Mov_Creditos.Pagare = Creditos.Pagare.
                Mov_Creditos.Fecha = W_Fecha.
                Mov_Creditos.Hora = TIME.
                Mov_Creditos.Num_Documento = string(pNumDocumento).
                Mov_Creditos.Usuario = W_Usuario.
                Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro.
                Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob.
                Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital.
                Mov_Creditos.Cpte = pComprobante.
                Mov_Creditos.Cod_Operacion = codOperacion.
                Mov_Creditos.Descrip = "Pago extra ($" + REPLACE(STRING(valorAbono,">>>,>>>,>>9")," ","") + ")".

                RUN excribirLog IN w_manija (INPUT w_usuario,
                                             INPUT "Pago extra ($" + REPLACE(STRING(valorAbono,">>>,>>>,>>9")," ","") + ")" + " / cliente_id: " + creditos.nit + " / num_credito: " + string(creditos.num_credito)) NO-ERROR.

                LEAVE.
            END.
        END.

        IF valorAbono >= CONTROL_pagos.cuota - CONTROL_pagos.cap_pagado THEN DO:
            valorAbono = valorAbono - (CONTROL_pagos.cuota - CONTROL_pagos.cap_pagado).
            CONTROL_pagos.cap_pagado = CONTROL_pagos.cap_pagado + (CONTROL_pagos.cuota - CONTROL_pagos.cap_pagado).
            CONTROL_pagos.id_pdoMes = 2.
        END.
        ELSE DO:
            CONTROL_pagos.cap_pagado = control_pagos.cap_pagado + valorAbono.
            valorAbono = 0.
            LEAVE.
        END.

        IF CONTROL_pagos.fec_vcto >= w_fecha THEN DO:
            IF cuotas_aAdelantar = 0 THEN
                LEAVE.
            ELSE
                cuotas_aAdelantar = cuotas_aAdelantar - 1.
        END.
    END.

    /* Fecha de próximo pago */
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
        creditos.fec_pago = CONTROL_pagos.fec_Vcto.
        LEAVE.
    END.


    /* Cuotas pagas */
    IF recalculaCuota = NO THEN DO:
        valorCredito = 0.
        creditos.cuo_pagadas = 0.

        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito BY CONTROL_pagos.nro_cuota DESC:
            valorCredito = valorCredito + control_pagos.pagos_capitalAcum.

            IF valorCredito > creditos.sdo_Capital THEN DO:
                creditos.cuo_pagadas = CONTROL_pagos.nro_cuota - 1.

                IF creditos.cuo_pagadas < 0 THEN
                    creditos.cuo_pagadas = 0.

                LEAVE.
            END.
        END.
    END.

    /* Días de atraso */
    IF creditos.fec_pago < w_fecha THEN
        creditos.dias_atraso = w_fecha - creditos.fec_pago.
    ELSE
        creditos.dias_atraso = 0.

    /* Cuotas atrasadas - Valor del atraso */
    IF creditos.dias_atraso > 0 THEN DO:
        FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                 AND CONTROL_pagos.num_credito = creditos.num_credito
                                 AND CONTROL_pagos.id_pdoMes < 2
                                 AND CONTROL_pagos.fec_Vcto <= w_fecha NO-LOCK:
            creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
            creditos.cuo_atraso = creditos.cuo_atraso + 1.
        END.
    END.

    /* Saldo proyectado */
    saldoProyectado = creditos.monto.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.fec_vcto <= w_fecha NO-LOCK BY CONTROL_pagos.fec_vcto:
        saldoProyectado = saldoProyectado - CONTROL_pagos.pagos_capitalAcum.
    END.

    IF saldoProyectado < creditos.sdo_capital OR creditos.dias_atraso > 0 THEN
        creditos.sdo_proyectado = saldoProyectado.
    ELSE
        creditos.sdo_proyectado = creditos.sdo_capital.

    IF creditos.val_atraso LE 0 THEN
        Creditos.Cod_Califica = 1.
    
    /* Se refinancia en caso que se haya escogido la opción */
    IF recalculaCuota = TRUE THEN DO:
        DEFINE VAR sdoCapital AS DECIMAL.
        DEFINE VAR vPlazo AS INTEGER.
        DEFINE VAR vCuota AS DECIMAL.
        DEFINE VAR vInteres AS DECIMAL.
        DEFINE VAR vTasa AS DECIMAL.
        DEFINE VAR diasPreinicio AS INTEGER.
        DEFINE VAR interesPreinicio AS DECIMAL.
        DEFINE VAR vMonto AS DECIMAL.
        DEFINE VAR cuotaAnterior AS DECIMAL.
        DEFINE VAR valorPresente AS DECIMAL.
        DEFINE VAR totalValorPresente AS DECIMAL.
        DEFINE VAR totalExtras AS DECIMAL.
        DEFINE VAR plazoAnterior AS INTEGER.
        DEFINE VAR pCuotasPagas AS INTEGER.

        /* Capturamos el número de cuotas pagas para saber la nueva posición de las cuotas extra */
        FIND FIRST amortizacion WHERE amortizacion.nit = creditos.nit
                                  AND amortizacion.num_credito = creditos.num_credito NO-LOCK NO-ERROR.
        IF AVAILABLE amortizacion THEN DO:
            FOR EACH amortizacion WHERE amortizacion.nit = creditos.nit
                                    AND amortizacion.num_credito = creditos.num_credito
                                    AND amortizacion.fec_pago < creditos.fec_pago NO-LOCK BY amortizacion.fec_pago DESC:
                pCuotasPagas = amortizacion.nro_cuota.
                LEAVE.
            END.
        END.
        ELSE DO:
            FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                     AND CONTROL_pagos.num_credito = creditos.num_credito
                                     AND CONTROL_pagos.fec_vcto < creditos.fec_pago NO-LOCK BY CONTROL_pagos.fec_vcto DESC:
                pCuotasPagas = CONTROL_pagos.nro_cuota.
                LEAVE.
            END.
        END.
        /* ----------------------- */

        FOR EACH Extras WHERE Extras.Nit = creditos.nit
                          AND Extras.Num_Solicitud = creditos.num_Solicitud
                          AND extras.fec_vcto >= creditos.fec_pago BY extras.fec_vcto:
            RUN HPDF IN W_ManFin (INPUT Extras.Vr_CuoExtra,
                                  INPUT (creditos.tasa / (nroPeriodos * 100)),
                                  INPUT Extras.Nro_Cuota - pCuotasPagas,
                                  OUTPUT valorPresente).

            totalValorPresente = totalValorPresente + valorPresente.
            extras.nro_cuota = extras.nro_cuota - pCuotasPagas.

            CREATE ttCuotasExtra.
            ttCuotasExtra.nro_cuota = extras.nro_cuota.
            ttCuotasExtra.vr_cuoExtra = Extras.Vr_CuoExtra.
            ttCuotasExtra.fec_vcto = extras.fec_vcto.
        END.

        vMonto = creditos.sdo_capital - totalValorPresente.
        vCuota = creditos.cuota.
        cuotaAnterior = creditos.cuota.
        plazoAnterior = creditos.plazo.
        
        RUN NVEF IN w_manfin (INPUT creditos.tasa / 100,
                              INPUT nroPeriodos,
                              OUTPUT vTasa).
        
        vTasa = vTasa * 100.
        
        vPlazo = creditos.plazo - creditos.cuo_pagadas.

        RUN Calculo_Cuota.R (INPUT-OUTPUT vMonto,
                             INPUT-OUTPUT vPlazo,
                             INPUT-OUTPUT vCuota,
                             INPUT-OUTPUT vInteres,
                             INPUT-OUTPUT vTasa,
                             INPUT 0,
                             INPUT 0,
                             INPUT creditos.per_Pago,
                             INPUT 3,
                             INPUT 1,
                             INPUT Creditos.Sistema).

        IF creditos.FOR_interes <> 2 AND
           creditos.cod_credito <> 108 AND
           creditos.cod_credito <> 113 THEN DO:

            RUN devuelveUltimaCuota2.r(INPUT vMonto,
                                       INPUT vPlazo,
                                       INPUT-OUTPUT vCuota,
                                       INPUT w_fecha,
                                       INPUT creditos.fec_pago,
                                       INPUT (creditos.tasa / nroPeriodos) / 100,
                                       INPUT creditos.per_pago,
                                       INPUT creditos.cod_credito,
                                       INPUT TABLE ttCuotasExtra).
        END.


        vCuota = ROUND((vCuota / 100) + 1,0) * 100.
        creditos.cuota = vCuota.
        creditos.fec_desembolso = w_fecha.
        creditos.fec_pagAnti = creditos.fec_pago.
        creditos.monto = creditos.sdo_capital.
        creditos.cuo_pagadas = 0.
        creditos.plazo = vPlazo.

        RUN crearControlPagosRefinancia.r(INPUT creditos.nit,
                                          INPUT creditos.num_credito,
                                          INPUT creditos.tasa,
                                          INPUT "Reliquidación por Pago Extraordinario").

        CREATE Mov_Creditos.
        ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
               Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
               Mov_Creditos.Nit = Creditos.Nit
               Mov_Creditos.Num_Credito = Creditos.Num_Credito
               Mov_Creditos.Cod_Operacion = 999999999
               Mov_Creditos.Ofi_Destino = Creditos.Agencia
               Mov_Creditos.Ofi_Fuente = W_Agencia
               Mov_Creditos.Pagare = Creditos.Pagare
               Mov_Creditos.Fecha = W_Fecha
               Mov_Creditos.Hora = TIME
               Mov_Creditos.Usuario = W_Usuario
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
               Mov_Creditos.Cpte = pComprobante
               Mov_Creditos.Descrip = "Disminuye cuota x Abono: " + STRING(cuotaAnterior,"$>>>,>>>,>>9.99") + " -> " + STRING(creditos.cuota,"$>>>,>>>,>>9.99").

        CREATE Mov_Creditos.
        ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
               Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
               Mov_Creditos.Nit = Creditos.Nit
               Mov_Creditos.Num_Credito = Creditos.Num_Credito
               Mov_Creditos.Cod_Operacion = 999999999
               Mov_Creditos.Ofi_Destino = Creditos.Agencia
               Mov_Creditos.Ofi_Fuente = W_Agencia
               Mov_Creditos.Pagare = Creditos.Pagare
               Mov_Creditos.Fecha = W_Fecha
               Mov_Creditos.Hora = TIME
               Mov_Creditos.Usuario = W_Usuario
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
               Mov_Creditos.Cpte = pComprobante
               Mov_Creditos.Descrip = "Cambia plazo x Abono: " + STRING(plazoAnterior) + " -> " + STRING(creditos.plazo).

    END.
    
END PROCEDURE.

PROCEDURE actualizaInteresContingente:
    DEFINE VAR contingenteAntes AS DECIMAL.
    DEFINE VAR pTasaMaxima AS DECIMAL.
    DEFINE VAR pDiasAtraso AS INTEGER.
    DEFINE VAR pFlagContingentes AS LOGICAL.
    DEFINE VAR pInteresPeriodo AS DECIMAL.
    DEFINE VAR pDiasParaContingente AS INTEGER.
    DEFINE VAR pNumCuota AS INTEGER.
    DEFINE VAR pFechaAux AS DATE.
    DEFINE VAR aReclasificar AS DECIMAL.

    contingenteAntes = creditos.INT_difCobro.

    pDiasAtraso = creditos.dias_Atraso.
    pFlagContingentes = FALSE.
    pDiasParaContingente = 0.
    pNumCuota = 0.
    creditos.INT_difCobro = 0.
    
    /* Calculo los intereses causados por las cuotas pendientes */
    FOR EACH CONTROL_pagos WHERE control_pagos.Nit = creditos.nit
                             AND control_pagos.Num_Credito = creditos.num_credito
                             AND control_pagos.Fec_Vcto <= TODAY
                             AND control_pagos.Id_PdoMes < 2 BREAK BY control_pagos.Num_Credito BY control_pagos.Fec_Vcto:
        IF FIRST-OF(CONTROL_pagos.num_credito) THEN DO:
            IF pDiasAtraso > Pro_Creditos.per_garPer THEN
                pFlagContingentes = TRUE.
        END.

        IF pFlagContingentes = TRUE THEN DO:
            pDiasParaContingente = pDiasParaContingente + diasPeriodo.

            IF pDiasParaContingente > pro_creditos.per_GarPer + 30 THEN
                creditos.INT_difCobro = creditos.INT_difCobro + CONTROL_pagos.contingente.
            ELSE DO:
                IF CONTROL_pagos.contingente > 0 /*AND control_pagos.causacion = 0*/ THEN DO:
                    CONTROL_pagos.causacion = control_pagos.causacion + CONTROL_pagos.contingente.
                    CONTROL_pagos.contingente = 0.
                END.
            END.
        END.
        ELSE DO:
            IF CONTROL_pagos.contingente > 0 /*AND control_pagos.causacion = 0*/ THEN DO:
                CONTROL_pagos.causacion = control_pagos.causacion + CONTROL_pagos.contingente.
                CONTROL_pagos.contingente = 0.
            END.
        END.
        
        pFechaAux = CONTROL_pagos.fec_vcto.
        pNumCuota = CONTROL_pagos.Nro_Cuota.
    END.

    IF creditos.dias_atraso = 0 THEN DO:
        FIND LAST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                  AND CONTROL_pagos.num_credito = creditos.num_credito
                                  AND CONTROL_pagos.fec_Vcto <= TODAY USE-INDEX ppal4 NO-LOCK NO-ERROR.
        IF AVAILABLE CONTROL_pagos THEN
            pNumCuota = CONTROL_pagos.nro_Cuota.
    END.

    /* Calculo los intereses pendientes de la última fecha de vencimiento a hoy */
    FIND FIRST CONTROL_pagos WHERE control_pagos.Nit = creditos.nit
                               AND control_pagos.Num_Credito = creditos.num_credito
                               AND CONTROL_pagos.nro_cuota = pNumCuota + 1 NO-ERROR.
    IF AVAILABLE CONTROL_pagos THEN DO:
        IF pFlagContingentes = TRUE THEN
            creditos.INT_difCobro = creditos.INT_difCobro + CONTROL_pagos.contingente.
        ELSE DO:
            IF CONTROL_pagos.contingente > 0 /*AND CONTROL_pagos.causacion = 0*/ THEN DO:
                CONTROL_pagos.causacion = CONTROL_pagos.causacion + CONTROL_pagos.contingente.
                CONTROL_pagos.contingente = 0.
            END.
        END.
    END.

    /* Realizo la contabilización */
    aReclasificar = contingenteAntes - creditos.INT_difCobro.

    IF aReclasificar > 0 THEN DO:
        creditos.INT_corriente = creditos.INT_corriente + aReclasificar.

        ASSIGN vCuenta = cuentaIntDifCobro            /*Sigue el traslado de ctas-orden*/
               valContabilizar = aReclasificar
               codOperacion = 020101004
               vComentario = "Reclasif-Contingente"
               reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.

        ASSIGN reclasificaDifCobro = TRUE
               vCuenta = cuentaIntDifCobro_contrapartida.
        RUN GrabaMov NO-ERROR.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = valContabilizar.

        ASSIGN vCuenta = cuentaIntCorriente
               valContabilizar = valContabilizar
               vComentario = "Reclasif-Contingente".
            RUN GrabaMov NO-ERROR.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = valContabilizar.

        ASSIGN vCuenta = cuentaIngresoInteres
               valContabilizar = valContabilizar
               vComentario = "Reclasif-Contingente".
            RUN GrabaMov NO-ERROR.
    END.

END PROCEDURE.

PROCEDURE ActualizaFacturacion:
    DEFINE VAR pagaCuotas AS INTEGER.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.estado = 1 BY facturacion.fec_pago:
        IF pIntMora >= facturacion.INT_mora - facturacion.pago_mora THEN DO:
            /*facturacion.pago_mora = facturacion.pago_mora + (facturacion.INT_mora - facturacion.pago_mora).*/
            pIntMora = pIntMora - (facturacion.INT_mora - facturacion.pago_mora).
            facturacion.pago_mora = facturacion.INT_mora.
        END.
        ELSE DO:
            facturacion.pago_mora = facturacion.pago_mora + pIntMora.
            pIntMora = 0.
        END.

        IF pIntCorriente >= facturacion.INT_corriente - facturacion.pago_intCorriente THEN DO:
            /*facturacion.pago_intCorriente = facturacion.pago_intCorriente + (facturacion.INT_Corriente - facturacion.pago_intCorriente).*/
            pIntCorriente = pIntCorriente - (facturacion.INT_corriente - facturacion.pago_intCorriente).
            facturacion.pago_intCorriente = facturacion.INT_corriente.
        END.
        ELSE DO:
            facturacion.pago_intCorriente = facturacion.pago_intCorriente + pIntCorriente.
            pIntCorriente = 0.
        END.

        IF pIntDifCobro >= facturacion.INT_difCobro - facturacion.pago_intdifCobro THEN DO:
            pIntDifCobro = pIntDifCobro - (facturacion.INT_difCobro - facturacion.pago_intDifCob).
            facturacion.pago_intDifCob = facturacion.INT_difCob.
        END.
        ELSE DO:
            facturacion.pago_intDifCob = facturacion.pago_intDifCob + pIntDifCobro.
            pIntDifCobro = 0.
        END.

        IF pCapital >= facturacion.capital - facturacion.pago_capital THEN DO:
            /*facturacion.pago_capital = facturacion.pago_capital + (facturacion.capital - facturacion.pago_capital).*/
            pCapital = pCapital - (facturacion.capital - facturacion.pago_capital).
            facturacion.pago_capital = facturacion.capital.
        END.
        ELSE DO:
            facturacion.pago_capital = facturacion.pago_capital + pCapital.
            pCapital = 0.
        END.

        IF facturacion.int_mora - facturacion.pago_mora <= 0 AND
           facturacion.INT_corriente - facturacion.pago_intCorriente <= 0 AND
           facturacion.INT_difCobro - facturacion.pago_intDifCobro <= 0 AND
           facturacion.capital - facturacion.pago_capital <= 0 THEN DO:
            facturacion.estado = 2.
            creditos.fec_pago = ADD-INTERVAL(facturacion.fec_pago,1,"months").
            creditos.fec_pago = DATE(MONTH(creditos.fec_pago),5,YEAR(creditos.fec_pago)).
            creditos.cuo_pagadas = creditos.cuo_pagadas + 1.
        END.
    END.

    creditos.val_atraso = 0.
    creditos.cuo_atraso = 0.
    creditos.dias_atraso = 0.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.fec_pago <= w_fecha
                           AND facturacion.estado = 1 NO-LOCK BY facturacion.fec_pago DESCENDING:
        creditos.fec_pago = facturacion.fec_pago.
        creditos.val_atraso = creditos.val_atraso + facturacion.capital - facturacion.pago_capital.

        IF creditos.val_atraso < 0 THEN
            creditos.val_atraso = 0.

        creditos.cuo_atraso = creditos.cuo_atraso + 1.
        creditos.sdo_proyectado = creditos.sdo_Capital - creditos.val_atraso.
    END.

    IF creditos.fec_pago <= w_fecha THEN
        creditos.dias_atraso = w_fecha - creditos.fec_pago.
    ELSE
        creditos.sdo_proyectado = creditos.sdo_capital.

END PROCEDURE.
