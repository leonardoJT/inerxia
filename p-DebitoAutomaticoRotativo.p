DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pCodCredito AS INTEGER.
DEFINE INPUT PARAMETER pNumcredito AS INTEGER.
DEFINE INPUT PARAMETER debitaAhPerm AS LOGICAL.
DEFINE INPUT PARAMETER pFechaProceso AS DATE.
DEFINE INPUT PARAMETER pUsuario AS CHARACTER.
/* ----------------------- */

DEFINE VAR pCuota AS DECIMAL.
DEFINE VAR valDebitar AS DECIMAL.
DEFINE VAR valAbono AS DECIMAL.
DEFINE VAR pOperacion AS INTEGER INITIAL 010102001.
DEFINE VAR pSecuencia AS INTEGER.
DEFINE VAR pComprobante AS INTEGER.
DEFINE VAR p_poliza AS DECIMAL.
DEFINE VAR p_Honora AS DECIMAL.
DEFINE VAR p_Costas AS DECIMAL.
DEFINE VAR p_SeguroVida AS DECIMAL.
DEFINE VAR p_SeguroDeudor AS DECIMAL.
DEFINE VAR p_IMorDifC AS DECIMAL.
DEFINE VAR p_IMora AS DECIMAL.
DEFINE VAR p_IDifCob AS DECIMAL.
DEFINE VAR p_ICte AS DECIMAL.
DEFINE VAR p_IAntic AS DECIMAL.
DEFINE VAR p_Capit AS DECIMAL.
DEFINE VAR p_VlrNoDist AS DECIMAL.
DEFINE VAR pResult AS LOGICAL.
DEFINE VAR flagContabiliza AS LOGICAL.

DEFINE TEMP-TABLE TempCtas
    FIELD agencia AS INTEGER
    FIELD cod_ahorro AS INTEGER
    FIELD cuenta AS CHARACTER

    /* oakley */

    FIELD CtaIng LIKE Cuentas.Cuenta
    FIELD CtaLiq LIKE Cuentas.Cuenta
    FIELD IntAnt LIKE Cuentas.Cuenta
    FIELD IntMor LIKE Cuentas.Cuenta
    FIELD DifCoD LIKE Cuentas.Cuenta
    FIELD DifCoH LIKE Cuentas.Cuenta
    FIELD CtaPol LIKE Cuentas.Cuenta
    FIELD CtaHon LIKE Cuentas.Cuenta
    FIELD CtaCos LIKE Cuentas.Cuenta
    FIELD CtaGar LIKE CortoLargo.Cta_VigGarAd
    FIELD CtaCGa LIKE CortoLargo.Cta_ContrapartidaGar
    FIELD Oper LIKE Liqui_Int.Cod_Operacion
    FIELD CtaSyA LIKE Cuentas.Cuenta.

DEFINE VAR saldoMinimo AS DECIMAL.

RUN CargarCuentas.

FIND FIRST creditos WHERE creditos.nit = pNit
                      AND creditos.cod_credito = pCodCredito
                      AND creditos.num_credito = pNumCredito NO-ERROR.
IF AVAILABLE creditos THEN DO:
    FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ creditos.agencia
                              AND Comprobantes.Comprobante EQ 21
                              AND Comprobantes.Estado EQ 1 NO-ERROR.
    IF AVAILABLE comprobantes THEN DO:
        pComprobante = comprobantes.comprobante.
        pSecuencia = comprobantes.secuencia + 1.
    END.

    /* 1. Determinamos el valor de la cuota a debitar */
    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.fec_pago <= pFechaProceso
                           AND facturacion.estado = 1 NO-LOCK:
        pCuota = pcuota + (facturacion.cuota - facturacion.pago_mora - facturacion.pago_intCorriente - facturacion.pago_capital).
    END.

    IF pCuota > creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.INT_moraDifCob + creditos.costas + creditos.polizas + creditos.honorarios -
                creditos.INT_anticipado THEN

        pCuota = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.INT_moraDifCob + creditos.costas + creditos.polizas + creditos.honorarios -
                 creditos.INT_anticipado.

    IF pCuota < 0 THEN
        pCuota = 0.

    /* 2. Debitamos las cuentas de ahorro */
    IF pCuota > 0 THEN DO:

        /* oakley */

        valDebitar = pCuota.

        FIND FIRST ahorros WHERE ahorros.agencia = creditos.agencia
                             AND ahorros.nit = creditos.nit
                             AND ahorros.tip_ahorro = 1
                             AND ahorros.cod_ahorro = 9
                             AND ahorros.sdo_disponible > 0 NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE pro_ahorros THEN DO:
                IF pro_ahorros.id_salMinimo = YES THEN
                    saldoMinimo = pro_ahorros.val_sdoMinimo.
                ELSE
                    saldoMinimo = 0.

                IF ahorros.sdo_disponible - saldoMinimo < valDebitar * 1.004 THEN
                    valDebitar = TRUNCATE(((ahorros.sdo_disponible - saldoMinimo) * 100) / 100.4,0).
            END.

            FIND FIRST TempCtas WHERE TempCtas.Agencia EQ Ahorros.Agencia
                                  AND TempCtas.cod_ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
            IF NOT AVAIL(TempCtas) THEN DO:
                MESSAGE "Falta configuración con Producto_Ahorro:" Ahorros.Cod_Ahorro SKIP
                        "para la agencia:" Ahorros.Agencia
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            IF valDebitar > 0 THEN DO:
                ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - valDebitar
                       Ahorros.Fec_UltTrans = pFechaProceso
                       Ahorros.Num_RetMes = Ahorros.Num_RetMes + 1
                       Ahorros.Num_RetDia = Ahorros.Num_RetDia + 1
                       Ahorros.Val_RetDia = Ahorros.Val_RetDia + valDebitar
                       Ahorros.Val_RetMes = Ahorros.Val_RetMes + valDebitar.

                IF flagContabiliza = FALSE THEN
                    flagContabiliza = TRUE.

                RUN Movimientos.
                RUN GMF.

                valAbono = valAbono + valDebitar.
                pCuota = pCuota - valDebitar.
            END.
        END.
    END.

    IF pCuota > 0 THEN DO:
        valDebitar = pCuota.

        FIND FIRST ahorros WHERE ahorros.agencia = creditos.agencia
                             AND ahorros.nit = creditos.nit
                             AND ahorros.tip_ahorro = 1
                             AND ahorros.cod_ahorro = 4
                             AND ahorros.sdo_disponible > 0 NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE pro_ahorros THEN DO:
                IF pro_ahorros.id_salMinimo = YES THEN
                    saldoMinimo = pro_ahorros.val_sdoMinimo.
                ELSE
                    saldoMinimo = 0.

                IF ahorros.sdo_disponible - saldoMinimo < valDebitar * 1.004 THEN
                    valDebitar = TRUNCATE(((ahorros.sdo_disponible - saldoMinimo) * 100) / 100.4,0).
            END.

            FIND FIRST TempCtas WHERE TempCtas.Agencia EQ Ahorros.Agencia
                                  AND TempCtas.cod_ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
            IF NOT AVAIL(TempCtas) THEN DO:
                MESSAGE "Falta configuración con Producto_Ahorro:" Ahorros.Cod_Ahorro SKIP
                        "para la agencia:" Ahorros.Agencia
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            IF valDebitar > 0 THEN DO:
                ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - valDebitar
                       Ahorros.Fec_UltTrans = pFechaProceso
                       Ahorros.Num_RetMes = Ahorros.Num_RetMes + 1
                       Ahorros.Num_RetDia = Ahorros.Num_RetDia + 1
                       Ahorros.Val_RetDia = Ahorros.Val_RetDia + valDebitar
                       Ahorros.Val_RetMes = Ahorros.Val_RetMes + valDebitar.

                IF flagContabiliza = FALSE THEN
                    flagContabiliza = TRUE.
                
                RUN Movimientos.
                RUN GMF.

                valAbono = valAbono + valDebitar.
                pCuota = pCuota - valDebitar.
            END.
        END.
    END.

    IF valAbono > 0 THEN DO:
        RUN p-pagoCredito.R(INPUT YES,
                            INPUT Creditos.Cod_Credito,
                            INPUT Creditos.Nit,
                            INPUT Creditos.Num_Credito,
                            INPUT valAbono,
                            INPUT pComprobante,
                            INPUT pSecuencia,
                            INPUT 0,
                            INPUT 1,
                            INPUT pFechaProceso,
                            INPUT FALSE,
                            OUTPUT P_Poliza,
                            OUTPUT P_Honora,
                            OUTPUT P_Costas,
                            OUTPUT P_SeguroVida,
                            OUTPUT P_SeguroDeudor,
                            OUTPUT P_IMorDifC,
                            OUTPUT P_IMora,
                            OUTPUT P_IDifCob,
                            OUTPUT P_ICte,
                            OUTPUT P_IAntic,
                            OUTPUT P_Capit,
                            OUTPUT P_VlrNoDist,
                            OUTPUT pResult).

        IF /*ERROR-STATUS:ERROR OR*/ P_VlrNoDist GT 0 OR P_VlrNoDist LT 0 THEN DO:
            MESSAGE "El proceso de distribución del pago retornó valor no distribuido:" P_VlrNoDist SKIP
                    "o retornó ERROR..." SKIP
                    "para el Nit:" Creditos.Nit ", Cod_producto:" Creditos.Cod_Credito ", Nro-Crédito:" Creditos.Num_Credito SKIP
                    "Revise por favor... Distribución cancelada."
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.

    IF flagContabiliza = TRUE THEN
        comprobantes.secuencia = comprobantes.secuencia + 1.

    RELEASE comprobantes.
END.


PROCEDURE GMF:
    DEFINE VAR gmfAplicado AS DECIMAL.

    RUN RutGMF.R (INPUT TRUE,
                  INPUT creditos.agencia,
                  INPUT Ahorros.Agencia,
                  INPUT 1,
                  INPUT Ahorros.Cod_Ahorro,
                  INPUT Ahorros.Nit,
                  INPUT Ahorros.Cue_Ahorros,
                  INPUT pOperacion,
                  INPUT valDebitar,
                  INPUT pComprobante,
                  INPUT STRING(pSecuencia),
                  INPUT "Débito Automático",
                  INPUT 0,
                  INPUT 0,
                  OUTPUT gmfAplicado) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "La rutina RutGMF retornó ERROR. No se permite la operación." SKIP
                "Los datos son los siguientes:" SKIP
                "Cédula:" ahorros.nit SKIP
                "Cuenta:" ahorros.cue_ahorros SKIP
                "Saldo disponible:" ahorros.sdo_disponible SKIP
                "Valor a debitar:" valDebitar SKIP
                "# de crédito:" creditos.num_Credito SKIP(2)
                "Por favor, reportar este error con los datos descritos a Leonardo G. Ocampo," SKIP
                "para hacer el debido seguimiento y la debida corrección. Muchas gracias."
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.
END PROCEDURE.


PROCEDURE CargarCuentas:
    FOR EACH Pro_Ahorros WHERE (pro_ahorros.tip_ahorro = 1 AND (pro_ahorros.cod_ahorro = 4 OR pro_ahorros.cod_ahorro = 9))
                            OR (pro_ahorros.tip_ahorro = 2 AND pro_ahorros.cod_ahorro = 3) NO-LOCK BY Pro_Ahorros.Cod_Ahorro:
        FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto = 1
                              AND CortoLargo.Cod_Producto = Pro_Ahorros.Cod_Ahorro
                              AND CortoLargo.Plazo_Inicial >= 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                              BY CortoLargo.Cod_Producto
                                                                              BY CortoLargo.Plazo_Inicial:
            IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta = CortoLargo.Cta_AsoAd
                                     AND Cuentas.Tipo = 2
                                     AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.
                IF AVAIL(Cuentas) THEN
                    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                         AND Cuentas.Tipo EQ 2
                                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                IF NOT AVAIL(Cuentas) THEN DO:
                    MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir activas en Cuentas..." SKIP
                            "para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro SKIP
                            "de la Agencia:" CortoLargo.Agencia
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                CREATE TempCtas.
                ASSIGN TempCtas.Agencia = CortoLargo.Agencia
                       TempCtas.cod_ahorro = CortoLargo.Cod_Producto
                       TempCtas.cuenta = CortoLargo.Cta_AsoAd
                       TempCtas.CtaSyA = CortoLargo.Cta_SyA.

                FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                                       AND Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
                IF NOT AVAIL(Liqui_Int) THEN DO:
                    MESSAGE "Falta Liqui_Int para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro SKIP
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                ASSIGN TempCtas.CtaLiq = Liqui_Int.Cta_CauCr         /*Los Causados*/
                       TempCtas.CtaIng = Liqui_Int.CtaCr_LiqAso      /*Los Por Pagar*/
                       TempCtas.IntAnt = Liqui_Int.CtaCr_Ret.        /*Ret-Fuente*/
            END.
        END.
    END.

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.cod_credito = pCodCredito NO-LOCK NO-ERROR.
    IF AVAILABLE pro_creditos THEN DO:
        FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto EQ 2
                              AND CortoLargo.Cod_Producto EQ Pro_Creditos.Cod_Credito
                              AND CortoLargo.Plazo_Inicial  GE 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                               BY CortoLargo.Cod_Producto
                                                                               BY CortoLargo.Plazo_Inicial:
            IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                     AND Cuentas.Tipo EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAIL(Cuentas) THEN DO:
                    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                         AND Cuentas.Tipo EQ 2
                                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                    IF AVAIL(Cuentas) THEN DO:
                        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_CostasDB
                                             AND Cuentas.Tipo EQ 2
                                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                        IF AVAIL(Cuentas) THEN DO:
                            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_HonorariosDB
                                                 AND Cuentas.Tipo EQ 2
                                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                            IF AVAIL(Cuentas) THEN
                                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_PolizasDB
                                                     AND Cuentas.Tipo EQ 2
                                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                        END.
                    END.
                END.
                ELSE DO:
                    MESSAGE "En CortoLargo.Cta_AsoAd, Cta_SyA, Cta_CostasDB, Cta_HonorariosDB, Cta_PolizasDB..." SKIP
                            "deben existir activas en Cuentas..." SKIP
                            "para el Pro_Creditos.Cod_Credito:" Pro_Creditos.Cod_Credito SKIP
                            "de la Agencia:" CortoLargo.Agencia
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                CREATE TempCtas.
                ASSIGN TempCtas.Agencia = CortoLargo.Agencia
                       TempCtas.cod_ahorro = CortoLargo.Cod_Producto
                       TempCtas.cuenta = CortoLargo.Cta_AsoAd
                       TempCtas.CtaSyA = CortoLargo.Cta_SyA
                       TempCtas.CtaHon = CortoLargo.Cta_HonorariosDB
                       TempCtas.CtaPol = CortoLargo.Cta_PolizasDB
                       TempCtas.CtaCos = CortoLargo.Cta_CostasDB
                       TempCtas.CtaGar = CortoLargo.Cta_VigGarAd
                       TempCtas.CtaCGa = CortoLargo.Cta_ContrapartidaGar.

                FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                                       AND Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
                IF NOT AVAIL(Liqui_Int) THEN DO:
                    MESSAGE "Falta Liqui_Int para el Pro_Creditos.Cod_Credito:" Pro_Creditos.Cod_Credito SKIP
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_LiqAso
                                     AND Cuentas.Tipo EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAIL(Cuentas) THEN DO:
                    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_LiqAso
                                         AND Cuentas.Tipo EQ 2
                                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                    IF AVAIL(Cuentas) THEN DO:
                        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaInt_AntAso
                                             AND Cuentas.Tipo EQ 2
                                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                        IF AVAIL(Cuentas) THEN DO:
                            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_MoraAso
                                                 AND Cuentas.Tipo EQ 2
                                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                            IF AVAIL(Cuentas) THEN DO:
                                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_DifCobAso
                                                     AND Cuentas.Tipo EQ 2
                                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                                IF AVAIL(Cuentas) THEN
                                    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_DifCobAso
                                                         AND Cuentas.Tipo EQ 2
                                                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                            END.
                        END.
                    END.
                END.
                ELSE DO:
                    MESSAGE "En Liqui_Int las cuentas: CtaCr_LiqAso, CtaDb_LiqAso, CtaCr_DifCobAso, CtaInt_AntAso, CtaDb_MoraAso, CtaDb_DifCobAso" SKIP
                            "deben existir activas en Plan de Cuentas..." SKIP
                            "para el Pro_Creditos.Cod_Credito:" Pro_Creditos.Cod_Credito
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                ASSIGN TempCtas.CtaLiq = Liqui_Int.CtaDb_LiqAso
                       TempCtas.CtaIng = Liqui_Int.CtaCr_LiqAso
                       TempCtas.IntAnt = Liqui_Int.CtaInt_AntAso
                       TempCtas.IntMor = Liqui_Int.CtaDb_MoraAso
                       TempCtas.DifCoD = Liqui_Int.CtaDb_DifCobAso
                       TempCtas.DifCoH = Liqui_Int.CtaCr_DifCobAso
                       TempCtas.Oper = Liqui_Int.Cod_Operacion.
            END.
        END.
    END.
END PROCEDURE.


PROCEDURE Movimientos:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
           Mov_Contable.Cuenta = TempCtas.cuenta
           Mov_Contable.Nit = Ahorros.Nit
           Mov_Contable.Fec_Contable = pFechaProceso
           Mov_Contable.Comentario = "Débito Automático"
           Mov_Contable.Usuario = pUsuario
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = creditos.agencia
           Mov_Contable.Comprobante = pComprobante
           Mov_Contable.Num_Documento = pSecuencia
           Mov_Contable.Doc_Refer = STRING(pSecuencia)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Db = valDebitar.

    CREATE Mov_Ahorros.
    ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
           Mov_Ahorros.Age_Destino = creditos.agencia
           Mov_Ahorros.Age_Fuente = ahorros.agencia
           Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
           Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
           Mov_Ahorros.Fecha = pFechaProceso
           Mov_Ahorros.Hora = TIME
           Mov_Ahorros.Nit = Ahorros.Nit
           Mov_Ahorros.Num_Documento = STRING(pSecuencia)
           Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
           Mov_Ahorros.Usuario = pUsuario
           Mov_Ahorros.Val_Efectivo = valDebitar
           Mov_Ahorros.Cod_Operacion = pOperacion
           Mov_Ahorros.Cpte = pComprobante
           Mov_Ahorros.Descrip = "Débito Automático".
END PROCEDURE.

PROCEDURE escribirLog:
DEFINE INPUT PARAMETER pMensaje AS CHARACTER.

OUTPUT TO VALUE("logs\" + pUsuario + ".csv") APPEND.
    EXPORT DELIMITER ";"
        NOW
        pMensaje.

OUTPUT CLOSE.
END PROCEDURE.
