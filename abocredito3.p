/*

- Se elimina el último parámetro
- Se agrega el parámetro de entrada fechaDePago

*/

DEFINE INPUT PARAMETER P_Grabar AS LOGICAL. /*False NO Actualiza, True SI*/
DEFINE INPUT PARAMETER p_agencia AS INTEGER.
DEFINE INPUT PARAMETER p_codProducto AS INTEGER.
DEFINE INPUT PARAMETER p_cedula AS CHARACTER.
DEFINE INPUT PARAMETER p_numcredito AS INTEGER.
DEFINE INPUT PARAMETER p_vlrAbono AS DECIMAL.
DEFINE INPUT PARAMETER P_Cpte AS INTEGER.
DEFINE INPUT PARAMETER P_NroDoc AS INTEGER.
DEFINE INPUT PARAMETER P_EfeCheq AS INTEGER FORMAT "9".   /*0 Efectivo, 1 Cheque*/
DEFINE INPUT PARAMETER P_NumCuo AS INTEGER.
DEFINE INPUT PARAMETER pfechaDelPago AS DATE.
DEFINE OUTPUT PARAMETER P_Poliza AS DECIMAL.
DEFINE OUTPUT PARAMETER P_Honora AS DECIMAL.
DEFINE OUTPUT PARAMETER P_Costas AS DECIMAL.
DEFINE OUTPUT PARAMETER p_SeguroVida AS DECIMAL.
DEFINE OUTPUT PARAMETER p_SeguroDeudor AS DECIMAL.
DEFINE OUTPUT PARAMETER P_IMorDifC AS DECIMAL.
DEFINE OUTPUT PARAMETER P_IMora AS DECIMAL.
DEFINE OUTPUT PARAMETER P_IDifCob AS DECIMAL.
DEFINE OUTPUT PARAMETER P_ICte AS DECIMAL.
DEFINE OUTPUT PARAMETER P_IAntic AS DECIMAL. /*Si P_IAntic(-) Neg.son cargos*/
DEFINE OUTPUT PARAMETER P_Capit AS DECIMAL.
DEFINE OUTPUT PARAMETER P_VlrNoDist AS DECIMAL.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFINE VAR Ctas_CtaPro AS CHARACTER.
DEFINE VAR Ctas_CtaHon AS CHARACTER.
DEFINE VAR Ctas_CtaPol AS CHARACTER.
DEFINE VAR Ctas_CtaCos AS CHARACTER.
DEFINE VAR Ctas_CtaLiq AS CHARACTER.
DEFINE VAR Ctas_CtaIng AS CHARACTER.
DEFINE VAR Ctas_IntMor AS CHARACTER.
DEFINE VAR Ctas_MorIng AS CHARACTER.
DEFINE VAR Ctas_DifCoD AS CHARACTER.
DEFINE VAR Ctas_DifCoH AS CHARACTER.
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
DEFINE VAR i AS INTEGER.

IF p_numCuo = 0 THEN
    p_numCuo = 1.

FIND FIRST Creditos WHERE Creditos.Agencia = p_agencia
                      AND Creditos.Nit = p_cedula
                      AND Creditos.Cod_Credito = p_codProducto
                      AND Creditos.Num_Credito = p_numCredito NO-ERROR.

RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                               INPUT Creditos.Plazo,
                               OUTPUT diasPeriodo,
                               OUTPUT nroMeses,
                               OUTPUT nroPeriodos,
                               OUTPUT nombrePeriodicidad).

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.

RUN ConfigCtas NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Error en configuración de Cuentas (abocredito3.p/ConfigCtas)"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    P_VlrNoDist = P_VlrAbono.
    RETURN ERROR.
END.

IF NOT P_Grabar THEN
    FIND CURRENT Creditos NO-LOCK NO-ERROR.

Abono:
DO TRANSACTION ON ERROR UNDO Abono:
    saldoTotalDeuda = Creditos.Honorarios +
                      Creditos.Costas +
                      Creditos.Polizas +
                      Creditos.Int_MorCobrar +
                      Creditos.Int_MoraDifCob +
                      Creditos.Int_Corrientes +
                      Creditos.Int_DifCobro +
                      Creditos.Sdo_Capital -
                      Creditos.Int_Anticipado.

    PorDist = p_VlrAbono.

    /* Si el pago supera el valor adeudado, cancelamos completamente el crédito */

    /* oakley
    - Agregar tabla de detalle de liquidación al proceso de liquidación de créditos
    - Agregar un parámetro que me indique si se trata de un proceso en back para que se pague el crédito aun cuando el valor enviado sea mayor, y si es un proceso atendido que rechace el pago */
    IF PorDist >= saldoTotalDeuda THEN DO:
        ASSIGN P_VlrNoDist = PorDist - saldoTotalDeuda
               P_Poliza = Creditos.Polizas
               P_Honora = Creditos.Honorarios
               P_Costas = Creditos.Costas
               P_IMora = Creditos.Int_MorCobrar
               P_IMorDifC = Creditos.Int_MoraDifCob
               P_IDifCob = Creditos.Int_DifCobro
               P_ICte = Creditos.Int_Corrientes
               P_IAntic = Creditos.Int_Anticipado * -1   /*para cargar por cancelac.*/
               P_Capit = Creditos.Sdo_Capital.

        /* Revisamos si al crédito se le puede o no cobrar la mora */
        IF P_IMora > 0 AND creditos.for_pago = 2 THEN DO:
            FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
            FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.
            IF AVAILABLE empresas THEN
                IF creditos.dias_atraso <= empresas.dias_gracia THEN
                    P_IMora = 0.
        END.

        IF NOT P_Grabar THEN
            RETURN.

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

        IF Creditos.Sistema EQ 2 OR Creditos.Plazo EQ 1 THEN
            Creditos.Cuo_Pagadas = 1.

        IF creditos.cod_credito <> 123 THEN /* Si no es Cupo Rotativo */
            Creditos.Estado = 3.
        ELSE DO:
            Creditos.Estado = 2.
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

        FIND FIRST Creditos WHERE Creditos.Nit EQ p_cedula
                              AND Creditos.Sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro > 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Creditos) THEN DO:
            FOR EACH Ahorros WHERE Ahorros.Nit EQ p_cedula
                               AND Ahorros.Tip_Ahorro EQ 4
                               AND Ahorros.Detalle_Estado = 8:
                Ahorros.Detalle_Estado = 2.
            END.
        END.

        RETURN.
    END.

    /* Se revisan los parámetros para verificar si se quiere refinanciar (plazo o cuota) */
    IF (creditos.cod_credito = 17 OR creditos.cod_credito = 22 OR creditos.cod_credito = 27 OR creditos.cod_credito = 57) AND creditos.dias_atraso = 0 THEN DO:
        FIND FIRST cfg_creditos NO-LOCK NO-ERROR.
        IF AVAILABLE cfg_creditos AND p_grabar = TRUE THEN DO:
            IF PorDist >= cfg_creditos.montoMinimoRefinanciacion THEN DO:
                MESSAGE "Usted está abonando al crédito un valor superior al monto" skip
                        "mínimo estipulado (" string(cfg_creditos.montoMinimoRefinanciacion,"$>>>,>>>,>>9") "." skip
                        "Desea recalcular la cuota?" VIEW-AS ALERT-BOX 
                    QUESTION BUTTONS YES-NO UPDATE recalculaCuota AS LOGICAL.
            END.
        END.
    END.

    /* Honorarios */
    IF PorDist > 0 THEN DO:
        IF PorDist >= Creditos.Honorarios AND creditos.honorarios > 0 THEN
            ASSIGN P_Honora = Creditos.Honorarios
                   PorDist = PorDist - P_Honora.
        ELSE
            IF Creditos.Honorarios > 0 THEN
                ASSIGN P_Honora = PorDist
                       PorDist = 0.
    END.

    /* Costas */
    IF PorDist > 0 THEN DO:
        IF PorDist >= Creditos.Costas AND creditos.costas > 0 THEN
            ASSIGN P_Costas = Creditos.Costas
                   PorDist = PorDist - P_Costas.
        ELSE
            IF Creditos.Costas > 0 THEN
                ASSIGN P_Costas = PorDist
                       PorDist = 0.
    END.

    /* Pólizas */
    IF PorDist GT 0 THEN DO:
        IF PorDist >= Creditos.Polizas AND Creditos.Polizas > 0 THEN
            ASSIGN P_Poliza = Creditos.Polizas
                   PorDist = PorDist - P_Poliza.
        ELSE
            IF Creditos.Polizas GT 0 THEN
                ASSIGN P_Poliza = PorDist
                       PorDist = 0.
    END.

    IF PorDist <= 0 THEN DO:
        IF NOT P_Grabar THEN
            RETURN.

        ASSIGN Creditos.Polizas = Creditos.Polizas - P_Poliza
               Creditos.Honorarios = Creditos.Honorarios - P_Honora
               Creditos.Costas = Creditos.Costas - P_Costas.

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
                ASSIGN P_IMorDifC = creditos.INT_moraDifCob
                       PorDist = PorDist - P_IMorDifC.
            ELSE
                ASSIGN P_IMorDifC = PorDist
                       PorDist = 0.
        END.
        
        /* Mora */
        IF PorDist > 0 AND creditos.int_morCobrar > 0 THEN DO:
            flagMora = TRUE.

            IF porDist > creditos.INT_morCobrar THEN
                ASSIGN P_IMora = creditos.INT_morCobrar
                       PorDist = PorDist - P_IMora.
            ELSE
                ASSIGN P_IMora = PorDist
                       PorDist = 0.
        END.

        /* Interés de difícil cobro */
        IF PorDist >= creditos.INT_difCobro THEN
            ASSIGN P_IDifCob = creditos.INT_difCobro
                   PorDist = PorDist - P_IDifCob.
        ELSE
            ASSIGN P_IDifCob = PorDist
                   PorDist = 0.
        
        /* Interés corriente */
        IF PorDist >= creditos.INT_corriente THEN
            ASSIGN P_ICte = creditos.INT_corriente
                   PorDist = PorDist - P_ICte.
        ELSE
            ASSIGN P_ICte = PorDist
                   PorDist = 0.

        /* Si es un crédito en mora, no se cobran los intereses del periodo que está corriendo (excepto los rotativos) */
        IF flagMora = TRUE AND creditos.cod_credito <> 123 THEN DO:
            FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                       AND CONTROL_pagos.num_credito = creditos.num_credito
                                       AND CONTROL_pagos.fec_Vcto >= w_fecha USE-INDEX ppal4 NO-LOCK NO-ERROR.
            IF AVAILABLE CONTROL_pagos THEN DO:
                diasDescontar = diasPeriodo - (CONTROL_pagos.Fec_Vcto - w_fecha).

                IF diasDescontar < 0 THEN
                    diasDescontar = 0.

                intPeriodo = ROUND((creditos.sdo_Capital / 100) * (creditos.tasa / 360) * diasDescontar,0).
                
                IF P_ICte >= intPeriodo AND intPeriodo > 0 THEN DO:
                    P_ICte = P_ICte - intPeriodo.
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
                    ASSIGN P_IMora = P_IMora + (facturacion.INT_mora - facturacion.pago_mora)
                           PorDist = PorDist - (facturacion.INT_mora - facturacion.pago_mora).
                ELSE
                    ASSIGN P_IMora = P_IMora + PorDist
                           PorDist = 0.

                /* Revisamos si al crédito se le puede o no cobrar la mora */
                IF P_IMora > 0 AND creditos.for_pago = 2 THEN DO:
                    FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
                    FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.

                    IF creditos.dias_atraso <= empresas.dias_gracia AND empresas.dias_gracia > 0 THEN
                        ASSIGN porDist = porDist + P_IMora
                               P_IMora = 0.
                END.
            END.

            /* Interés difícil cobro */
            IF PorDist >= facturacion.INT_difCobro - facturacion.pago_intDifCobro THEN
                ASSIGN P_IDifCob = P_IDifCob + (facturacion.INT_difCobro - facturacion.pago_intDifCobro)
                       PorDist = PorDist - (facturacion.INT_difCobro - facturacion.pago_intDifCobro).
            ELSE
                ASSIGN P_IDifCob = P_IDifCob + PorDist
                       PorDist = 0.

            /* Revisamos que el interés de difícil cobro sea mayor que el valor que se va a distribuir */
            IF creditos.INT_difCobro < P_IDifCob THEN DO:
                /* Devolvemos el excedente para que se siga distribuyendo */
                PorDist = PorDist + (P_IDifCob - creditos.INT_difCobro).

                /* Ajustamos el interés corriente que se pide en la factura, porque el crédito no tiene todo causado */
                IF P_Grabar = TRUE THEN
                    facturacion.INT_difCobro = facturacion.INT_difCobro - (P_IDifCob - creditos.INT_difCobro).

                P_IDifCob = creditos.INT_difCobro.
            END.

            /* Interés corriente */
            IF PorDist >= facturacion.INT_corriente - facturacion.pago_intCorriente THEN
                ASSIGN P_ICte = P_ICte + (facturacion.INT_corriente - facturacion.pago_intCorriente)
                       PorDist = PorDist - (facturacion.INT_corriente - facturacion.pago_intCorriente).
            ELSE
                ASSIGN P_ICte = P_ICte + PorDist
                       PorDist = 0.

            /* Revisamos que el interés corriente sea mayor que el valor que se va a distribuir */
            IF creditos.INT_corriente < P_ICte THEN DO:

                /* Devolvemos el excedente para que se siga distribuyendo */
                PorDist = PorDist + (P_Icte - creditos.INT_corriente).

                /* Ajustamos el interés corriente que se pide en la factura, porque el crédito no tiene todo causado */
                IF P_Grabar = TRUE THEN
                    facturacion.INT_corriente = facturacion.INT_corriente - (P_Icte - creditos.INT_corriente).

                P_ICte = creditos.INT_corriente.
            END.

            /* Capital */
            IF porDist > 0 THEN DO:
                IF PorDist >= facturacion.capital - facturacion.pago_capital THEN
                    ASSIGN P_Capit = P_Capit + (facturacion.capital - facturacion.pago_capital)
                           PorDist = PorDist - (facturacion.capital - facturacion.pago_capital).
                ELSE
                    ASSIGN P_Capit = P_Capit + PorDist
                           PorDist = 0.
            END.
        END.
    END.

    /* Capital */
    IF porDist > 0 THEN DO:
        aCapitalTemp = P_Capit.

        IF PorDist >= (creditos.sdo_capital - aCapitalTemp) THEN
            ASSIGN P_Capit = P_Capit + (creditos.sdo_capital - aCapitalTemp)
                   PorDist = PorDist - (creditos.sdo_capital - aCapitalTemp).
        ELSE
            ASSIGN P_Capit = P_Capit + PorDist
                   PorDist = 0.
    END.

    /* Si es un rotativo y luego de abonar el pago adicional al capital aunqueda dinero para distribuir, continúo distribuyendo los demás conceptos */
    IF creditos.cod_credito = 123 AND porDist > 0 THEN DO:
        /* Mora */
        IF creditos.INT_morCobrar - P_IMora > 0 THEN DO:
            IF porDist > creditos.INT_morCobrar - P_IMora THEN
                ASSIGN PorDist = PorDist - (creditos.INT_morCobrar - P_IMora)
                       P_IMora = P_IMora + (creditos.INT_morCobrar - P_IMora).
            ELSE
                ASSIGN P_IMora = P_IMora + PorDist
                       PorDist = 0.

            /* Revisamos si al crédito se le puede o no cobrar la mora */
            IF P_IMora > 0 AND creditos.for_pago = 2 THEN DO:
                FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
                FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.

                IF creditos.dias_atraso <= empresas.dias_gracia AND empresas.dias_gracia > 0 THEN
                    ASSIGN porDist = porDist + P_IMora
                           P_IMora = 0.
            END.
        END.

        /* Interés difícil cobro */
        IF PorDist >= creditos.INT_difCobro - P_IdifCob THEN
            ASSIGN PorDist = PorDist - (creditos.INT_difCobro - P_IdifCob)
                   P_IDifCob = P_IDifCob + (creditos.INT_difCobro - P_IdifCob).
        ELSE
            ASSIGN P_IDifCob = P_IDifCob + PorDist
                   PorDist = 0.

        /* Interés corriente */
        IF PorDist >= creditos.INT_corriente - P_ICte THEN
            ASSIGN PorDist = PorDist - (creditos.INT_corriente - P_ICte)
                   P_ICte = P_ICte + (creditos.INT_corriente - P_ICte).
        ELSE
            ASSIGN P_ICte = P_ICte + PorDist
                   PorDist = 0.
    END.

    /* Si luego de distribuir todo el valor aun sobra dinero, se devuelve como un sobrante para que se tome la decisión desde el programa fuente */
    IF porDist > 0 THEN
        P_VlrNoDist = PorDist.
    ELSE
        P_VlrNoDist = 0.

    IF NOT P_Grabar THEN
        RETURN.

    ASSIGN Creditos.Polizas = Creditos.Polizas - P_Poliza
           Creditos.Honorarios = Creditos.Honorarios - P_Honora
           Creditos.Costas = Creditos.Costas - P_Costas
           Creditos.Int_MorCobrar = Creditos.Int_MorCobrar - P_IMora
           Creditos.Int_MoraDifCob = Creditos.Int_MoraDifCob - P_IMorDifC
           Creditos.Sdo_Capital = Creditos.Sdo_Capital - P_Capit
           creditos.INT_difCobro = creditos.INT_difCobro - P_IDifCob
           creditos.INT_corriente = creditos.INT_corriente - P_ICte
           creditos.INT_anticipado = creditos.INT_anticipado + P_IAntic
           creditos.seg_Vida = creditos.seg_Vida - P_SeguroVida
           creditos.seg_Cartera = creditos.seg_cartera - P_SeguroDeudor.

    /*IF creditos.INT_corriente < 0 THEN
        creditos.INT_corriente = 0.*/

    RUN MovCreditos NO-ERROR.

    IF creditos.cod_credito <> 123 THEN
        RUN A_controlpagos NO-ERROR.
    ELSE
        RUN ActualizaFacturacion.

    RETURN.
END.

PROCEDURE MovCreditos:
    /* ---------------------------------------------------------------------------- */
    ASSIGN Creditos.Sdo_CapPag = Creditos.Sdo_CapPag + P_Capit
           Creditos.Sdo_IntMor = Creditos.Sdo_IntMor + (P_IMora + P_IMorDifC)
           Creditos.Sdo_IntPag = Creditos.Sdo_IntPag + (P_IDifCob + P_ICte + P_IAntic)
           Creditos.Fec_UltPag = W_Fecha.
           
    IF Creditos.Sdo_Capital GT 0 THEN DO:
        IF Creditos.Cuo_Pagadas GE Creditos.Plazo THEN
            Creditos.Cuo_Pagadas = Creditos.Plazo - 1.
    END.
    /* ---------------------------------------------------------------------------- */

    IF P_Poliza > 0 THEN DO:
        ASSIGN vCuenta = Ctas_CtaPol
               valContabilizar = P_Poliza
               codOperacion = 020101006
               vComentario = "Abono para Polizas".
        RUN GrabaMov NO-ERROR.
    END.

    IF P_Costas > 0 THEN DO:
        ASSIGN vCuenta = Ctas_CtaCos
               valContabilizar = P_Costas
               codOperacion = 020101008
               vComentario = "Abono para CostasJ".
        RUN GrabaMov NO-ERROR.
    END.

    IF P_Honora > 0 THEN DO:
        ASSIGN vCuenta = Ctas_CtaHon
               valContabilizar = P_Honora
               codOperacion = 020101007
               vComentario = "Abono para Honorarios".
        RUN GrabaMov NO-ERROR.    
    END.
    
    IF P_IMora > 0 THEN DO:
        ASSIGN vCuenta = Ctas_IntMor
               valContabilizar = P_IMora 
               codOperacion = 020101002
               vComentario = "Abono para Int-X-Mora". 
        RUN GrabaMov NO-ERROR.
    END.

    IF P_IMorDifC > 0 THEN DO:
        ASSIGN vCuenta = Ctas_MorIng
               valContabilizar = P_IMorDifC
               codOperacion = 020101002
               vComentario = "Abono IntMora-DifCobro". 
        RUN GrabaMov NO-ERROR.

        ASSIGN vCuenta = Ctas_DifCoD            /*Sigue el traslado de ctas-orden*/
               reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.

        ASSIGN reclasificaDifCobro = TRUE
               vCuenta = Ctas_DifCoH.
        RUN GrabaMov NO-ERROR.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = valContabilizar.
    END.

    IF P_ICte > 0 THEN DO:
        ASSIGN vCuenta = Ctas_CtaLiq
               valContabilizar = P_ICte
               codOperacion = 020101003
               vComentario = "Abono para Int-Corriente".
        RUN GrabaMov NO-ERROR.
    END.

    IF P_IDifCob > 0 THEN DO:
        ASSIGN vCuenta = Ctas_DifCoD            /*Sigue el traslado de ctas-orden*/
               valContabilizar = P_IDifCob
               codOperacion = 020101004
               vComentario = "Reclasif-Contingente"
               reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.

        ASSIGN reclasificaDifCobro = TRUE
               vCuenta = Ctas_DifCoH.
        
        RUN GrabaMov NO-ERROR.
        
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = valContabilizar.

        ASSIGN vCuenta = Ctas_CtaLiq
               valContabilizar = valContabilizar
               vComentario = "Reclasif-Contingente".
        
        RUN GrabaMov NO-ERROR.
        
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = valContabilizar.

        ASSIGN vCuenta = Ctas_CtaIng
               valContabilizar = valContabilizar
               vComentario = "Reclasif-Contingente".
            
        RUN GrabaMov NO-ERROR.

        ASSIGN vCuenta = Ctas_CtaLiq
               valContabilizar = P_IDifCob
               codOperacion = 020101003
               vComentario = "Abono para Int-Corriente".
        
        RUN GrabaMov NO-ERROR.
    END.

    IF P_Capit > 0 THEN DO:
        ASSIGN vCuenta = Ctas_CtaPro
               valContabilizar = P_Capit
               codOperacion = 020101001
               vComentario = "Abono para Capital". 
        RUN GrabaMov NO-ERROR.
    END.

    /*IF P_IAntic <> 0 THEN DO:
        ASSIGN W_Cta = Ctas_IntAnt
               W_Valor = P_IAntic
               W_Oper = W_OpAboCred[5]
               W_Desc = "Abono para Int-Anticipado".

        RUN GrabaMov NO-ERROR.

        IF P_IAntic < 0 THEN
            ASSIGN W_Desc = "Abono para Int-Anticipado"
                   Mov_Contable.Cr = 0
                   Mov_Contable.Db = W_Valor * -1.
    END.*/
END PROCEDURE.


/* --- Crea Mov_Creditos y Mov_Contables --- */
PROCEDURE GrabaMov:
    /*IF P_RecNOInd EQ 1 THEN DO:   /*Abril 1/06 GAER Solo Si es Rec-Individual, Cuando es Rec-Nòmina lo*/*/
        CREATE Mov_Contable. /* Realiza el Prog.Contab_RecLibr.P */
        ASSIGN Mov_Contable.Agencia = Creditos.Agencia
               Mov_Contable.Cuenta = vCuenta
               Mov_Contable.Nit = Creditos.Nit
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Comentario = vComentario
               Mov_Contable.Usuario = W_Usuario
               Mov_Contable.Cen_Costos = W_Cencosgral
               Mov_Contable.Destino = W_Agencia
               Mov_Contable.Comprobante = P_Cpte
               Mov_Contable.Num_Documento = INTEG(P_NroDoc)
               Mov_Contable.Doc_Refer = STRING(Creditos.Pagare)
               Mov_Contable.Enlace = STRING(Creditos.Num_Credito)
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Hora = TIME
               Mov_Contable.Estacion = W_Estacion
               Mov_Contable.Cr = valContabilizar NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE Creditos.Agencia  vCuenta Creditos.Nit W_Fecha vComentario W_Usuario W_Cencosgral W_Agencia P_Cpte INTEG(P_NroDoc)
                    STRING(Creditos.Num_Credito) STRING(Creditos.Num_Credito) TODAY TIME W_Estacion valContabilizar SKIP
                    "Error al grabar Mov_Contable, rectifique la Cuenta : " vCuenta SKIP
                    "Debe existir activa... No se permite la operaciòn..." SKIP
                    "                    Programa AboCredito.P."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.

            RETURN ERROR.
        END.
    /*END.*/
    

    IF reclasificaDifCobro THEN DO: /*Solo trasl DifCob y Anticip*/
        reclasificaDifCobro = FALSE.
        RETURN.
    END.
    
    IF vComentario <> "Reclasif-Contingente" THEN DO:
        CREATE Mov_Creditos.
        ASSIGN Mov_Creditos.Agencia       = Creditos.Agencia
               Mov_Creditos.Cod_Credito   = Creditos.Cod_Credito
               Mov_Creditos.Nit           = Creditos.Nit
               Mov_Creditos.Num_Credito   = Creditos.Num_Credito
               Mov_Creditos.Ofi_Destino   = Creditos.agencia
               Mov_Creditos.Ofi_Fuente    = W_Agencia
               Mov_Creditos.Pagare        = Creditos.Pagare
               Mov_Creditos.Fecha         = W_Fecha
               Mov_Creditos.Hora          = TIME
               Mov_Creditos.Num_Documento = string(P_NroDoc)
               Mov_Creditos.Usuario       = W_Usuario
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital   = Creditos.Sdo_Capital
               Mov_Creditos.Val_Efectivo  = valContabilizar
               Mov_Creditos.Cpte          = P_Cpte
               Mov_Creditos.Cod_Operacion = codOperacion
               Mov_Creditos.Descrip       = vComentario + "-Efectivo".
    
        IF P_EfeCheq EQ 1 THEN
           ASSIGN Mov_Creditos.Val_Efectivo = 0
                  Mov_Creditos.Val_Cheque   = valContabilizar
                  Mov_Creditos.Descrip      = vComentario + "-Cheque".

        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error al grabar Mov_Creditos, rectifique la operación : " codOperacion SKIP
                    "Debe existir activa... No se permite la operaciòn..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.

            RETURN ERROR.
        END.
    END.

END PROCEDURE.


PROCEDURE ConfigCtas:
    FIND FIRST CortoLargo WHERE CortoLargo.Agencia = Creditos.Agencia
                            AND CortoLargo.Clase_Producto = 2
                            AND CortoLargo.Cod_Producto = Creditos.Cod_Credito
                            AND CortoLargo.Plazo_Inicial >= 0 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(CortoLargo) THEN DO:
        MESSAGE "No existe la configuración en CortoLargo para la línea de crédito" Creditos.Cod_Credito "para la agencia" Creditos.Agencia SKIP
                "No se permite el recaudo..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        P_VlrNoDist = P_VlrAbono.
        RETURN ERROR.
    END.

    FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto = 2
                           AND Liqui_Int.Cod_Producto = CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Liqui_Int) THEN DO:
        MESSAGE "No existe la configuración en Liqui_Int para la línea de crédito" Creditos.Cod_Credito SKIP
                "No se permite el recaudo..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        P_VlrNoDist = P_VlrAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización del capital */
    IF CortoLargo.Cta_AsoAd  <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_AsoAd."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        P_VlrNoDist = P_VlrAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de honorarios */
    IF CortoLargo.Cta_HonorariosDB  <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_HonorariosDB."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        P_VlrNoDist = P_VlrAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de Pólizas */
    IF CortoLargo.Cta_PolizasDB  <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_PolizasDB."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        P_VlrNoDist = P_VlrAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de costas */
    IF CortoLargo.Cta_CostasDB <= "0" THEN DO:
        MESSAGE "No se encuentra configurada la cuenta en CortoLargo.Cta_CostasDB."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        P_VlrNoDist = P_VlrAbono.
        RETURN ERROR.
    END.

    /* Cuenta para la contabilización de los intereses corriente */
    IF AVAILABLE Liqui_Int THEN DO:
        IF Liqui_Int.CtaDb_LiqAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaDb_LiqAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaCr_LiqAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaCr_LiqAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaDb_MoraAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaDb_MoraAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaCr_MoraAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaCr_MoraAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaDb_DifCobAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaDb_DifCobAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.
            RETURN ERROR.
        END.

        IF Liqui_Int.CtaCr_DifCobAso <= "0" THEN DO:
            MESSAGE "No se encuentra configurada la cuenta en Liqui_Int.CtaCr_DifCobAso."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            P_VlrNoDist = P_VlrAbono.
            RETURN ERROR.
        END.

        /* oakley */

        IF creditos.FOR_pago = 2 THEN /* Nómina */
            ASSIGN Ctas_CtaLiq = Liqui_Int.CtaDb_LiqAso
                   Ctas_CtaIng = Liqui_Int.CtaCr_LiqAso
                   Ctas_IntMor = Liqui_Int.CtaDb_MoraAso
                   Ctas_MorIng = Liqui_Int.CtaCr_MoraAso
                   Ctas_DifCoD = Liqui_Int.CtaDb_DifCobAso
                   Ctas_DifCoH = Liqui_Int.CtaCr_DifCobAso.
        ELSE /* Caja */
            ASSIGN Ctas_CtaLiq = Liqui_Int.CtaDb_Liq
                   Ctas_CtaIng = Liqui_Int.CtaCr_Liq
                   Ctas_IntMor = Liqui_Int.CtaDb_Mora
                   Ctas_MorIng = Liqui_Int.CtaCr_Mora
                   Ctas_DifCoD = Liqui_Int.CtaDb_DifCob
                   Ctas_DifCoH = Liqui_Int.CtaCr_DifCob.
    END.

    IF creditos.FOR_pago = 2 THEN
        Ctas_CtaPro = CortoLargo.Cta_AsoAd.
    ELSE
        Ctas_CtaPro = CortoLargo.Cta_NoaAd.

    ASSIGN Ctas_CtaHon = CortoLargo.Cta_HonorariosDB
           Ctas_CtaPol = CortoLargo.Cta_PolizasDB
           Ctas_CtaCos = CortoLargo.Cta_CostasDB.

END PROCEDURE.

PROCEDURE A_controlpagos:
    DEFINE VARIABLE nrocuo AS INTEGER.
    DEFINE VAR fechaAux AS DATE.
    DEFINE VAR diasAsumar AS INTEGER.
    DEFINE VAR diasAtrasoAux AS INTEGER.
    DEFINE VAR ValorCredito AS DECIMAL.
    DEFINE VAR flagfecPago AS LOGICAL.
    DEFINE VAR valorAbono AS DECIMAL.
    DEFINE VAR saldoProyectado AS DECIMAL.
    DEFINE VAR moraAux AS DECIMAL.
    
    valorAbono = P_VlrAbono.
    moraAux = P_IMora.

    /* Se calculan los campos creditos.dias_atraso, creditos.cuo_atraso, creditos.cuo_pagadas y creditos.val_atraso */
    creditos.val_atraso = 0.
    creditos.cuo_atraso = 0.
    creditos.dias_atraso = 0.
    /*creditos.cuo_pagadas = 0.*/

    /* Cerramos las cuotas a que haya lugar */
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2 BY CONTROL_pagos.nro_cuota:
        creditos.fec_pago = CONTROL_pagos.fec_Vcto.
        
        /* Para correr la fecha de pago sin adelantar el crédito más de 1 cuota o de las cuotas que esté pagando */
        /*IF CONTROL_pagos.fec_Vcto <= w_fecha + (diasPeriodo * (1 + p_NumCuo)) THEN DO:
            
            
        END.*/
        /* ----------------------------------------------------------------------------------------------------- */

        IF valorAbono < CONTROL_pagos.cuota /*+ CONTROL_pagos.INT_mora*/ - CONTROL_pagos.cap_pagado OR control_pagos.fec_Vcto - (diasPeriodo * (1 + p_numCuo)) > w_fecha THEN DO:
            IF control_pagos.fec_Vcto - diasPeriodo < w_fecha THEN DO:
                CONTROL_pagos.cap_pagado = CONTROL_pagos.cap_pagado + valorAbono.
                CONTROL_pagos.INT_mora = CONTROL_pagos.INT_mora - moraAux.

                IF CONTROL_pagos.INT_mora < 0 THEN
                    CONTROL_pagos.INT_mora = 0.
            END.

            LEAVE.
        END.

        IF valorAbono >= CONTROL_pagos.cuota /*+ CONTROL_pagos.INT_mora*/ - CONTROL_pagos.cap_pagado THEN DO:
            valorAbono = valorAbono - (CONTROL_pagos.cuota /*+ CONTROL_pagos.INT_mora*/ - CONTROL_pagos.cap_pagado).
            CONTROL_pagos.cap_pagado = CONTROL_pagos.cap_pagado + (CONTROL_pagos.cuota /*+ CONTROL_pagos.INT_mora*/ - CONTROL_pagos.cap_pagado).
            CONTROL_pagos.id_pdoMes = 2.
        END.
        ELSE DO:
            CONTROL_pagos.cap_pagado = control_pagos.cap_pagado + valorAbono.
            valorAbono = 0.
        END.
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

        FOR EACH Extras WHERE Extras.Nit = creditos.nit
                          AND Extras.Num_Solicitud = creditos.num_Solicitud
                          AND extras.fec_vcto >= creditos.fec_pago NO-LOCK BY extras.fec_vcto:
            RUN HPDF IN W_ManFin (INPUT Extras.Vr_CuoExtra,
                                  INPUT (creditos.tasa / (nroPeriodos * 100)),
                                  INPUT Extras.Nro_Cuota,
                                  OUTPUT valorPresente).

            totalValorPresente = totalValorPresente + valorPresente.
        END.

        sdoCapital = creditos.sdo_capital - totalValorPresente.
        vCuota = creditos.cuota.
        cuotaAnterior = creditos.cuota.
        plazoAnterior = creditos.plazo.
        
        RUN NVEF IN w_manfin (INPUT creditos.tasa / 100,
                              INPUT nroPeriodos,
                              OUTPUT vTasa).
        
        vTasa = vTasa * 100.
        
        vPlazo = creditos.plazo - creditos.cuo_pagadas.

        /* Esto lo hacemos con el fin de distribuir los intereses adicionales a un periodo de forma que aumentando la cuota se puedan amortizar en el transcurso del crédito */
        diasPreinicio = ADD-INTERVAL(creditos.fec_pago, -1,"months") - w_fecha.
        interesPreInicio = (creditos.sdo_Capital / 100) * (((creditos.tasa / nroPeriodos) / 30) * diasPreinicio).
        vMonto = sdoCapital + interesPreinicio.
        
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
               Mov_Creditos.Cpte = p_Cpte
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
               Mov_Creditos.Cpte = p_Cpte
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

        ASSIGN vCuenta = Ctas_DifCoD            /*Sigue el traslado de ctas-orden*/
               valContabilizar = aReclasificar
               codOperacion = 020101004
               vComentario = "Reclasif-Contingente"
               reclasificaDifCobro = TRUE.
        RUN GrabaMov NO-ERROR.

        ASSIGN reclasificaDifCobro = TRUE
               vCuenta = Ctas_DifCoH.
        RUN GrabaMov NO-ERROR.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = valContabilizar.

        ASSIGN vCuenta = Ctas_CtaLiq
               valContabilizar = valContabilizar
               vComentario = "Reclasif-Contingente".
            RUN GrabaMov NO-ERROR.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = valContabilizar.

        ASSIGN vCuenta = Ctas_CtaIng
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
        IF P_IMora >= facturacion.INT_mora - facturacion.pago_mora THEN DO:
            /*facturacion.pago_mora = facturacion.pago_mora + (facturacion.INT_mora - facturacion.pago_mora).*/
            P_IMora = P_Imora - (facturacion.INT_mora - facturacion.pago_mora).
            facturacion.pago_mora = facturacion.INT_mora.
        END.
        ELSE DO:
            facturacion.pago_mora = facturacion.pago_mora + P_Imora.
            P_Imora = 0.
        END.

        IF P_ICte >= facturacion.INT_corriente - facturacion.pago_intCorriente THEN DO:
            /*facturacion.pago_intCorriente = facturacion.pago_intCorriente + (facturacion.INT_Corriente - facturacion.pago_intCorriente).*/
            P_ICte = P_ICte - (facturacion.INT_corriente - facturacion.pago_intCorriente).
            facturacion.pago_intCorriente = facturacion.INT_corriente.
        END.
        ELSE DO:
            facturacion.pago_intCorriente = facturacion.pago_intCorriente + P_ICte.
            P_ICte = 0.
        END.

        IF P_IDifCob >= facturacion.INT_difCobro - facturacion.pago_intdifCobro THEN DO:
            P_IDifCob = P_IDifCob - (facturacion.INT_difCobro - facturacion.pago_intDifCob).
            facturacion.pago_intDifCob = facturacion.INT_difCob.
        END.
        ELSE DO:
            facturacion.pago_intDifCob = facturacion.pago_intDifCob + P_IDifCob.
            P_IDifCob = 0.
        END.

        IF P_Capit >= facturacion.capital - facturacion.pago_capital THEN DO:
            /*facturacion.pago_capital = facturacion.pago_capital + (facturacion.capital - facturacion.pago_capital).*/
            P_Capit = P_Capit - (facturacion.capital - facturacion.pago_capital).
            facturacion.pago_capital = facturacion.capital.
        END.
        ELSE DO:
            facturacion.pago_capital = facturacion.pago_capital + P_Capit.
            P_Capit = 0.
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
