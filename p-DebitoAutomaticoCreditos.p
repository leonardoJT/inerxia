DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pCodCredito AS INTEGER.

DEFINE INPUT PARAMETER pNumcredito AS INTEGER.

{Incluido/Variable.I "SHARED"}

DEFINE VAR pCuota AS DECIMAL.
DEFINE VAR valDebitar AS DECIMAL.
DEFINE VAR valAbono AS DECIMAL.
DEFINE VAR pCuenta AS CHARACTER.
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

DEFINE TEMP-TABLE TempCtas
    FIELD Agen LIKE Ahorros.Agencia
    FIELD TipP AS CHAR FORM "X(1)"
    FIELD Pto LIKE Ahorros.Cod_Ahorro
    FIELD CtaPro AS CHARACTER
    
    /* oakley */

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
    /* 1. Determinamos el valor de la cuota a debitar */
    pCuota = /*creditos.cuota */ creditos.val_atraso + creditos.INT_corriente + creditos.INT_difCobro + creditos.INT_MorCobrar.
    
    IF pCuota > creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.INT_moraDifCob + creditos.costas + creditos.polizas + creditos.honorarios -
                creditos.INT_anticipado THEN
        pCuota = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.INT_moraDifCob + creditos.costas + creditos.polizas + creditos.honorarios -
                 creditos.INT_anticipado.

    IF pCuota < 0 THEN
        pCuota = 0.

    /* 2. Debitamos las cuentas de ahorro */
    IF pCuota > 0 THEN DO:
        valDebitar = pCuota.

        /*FIND FIRST ahorros WHERE ahorros.agencia = creditos.agencia
                             AND ahorros.nit = creditos.nit
                             AND ahorros.tip_ahorro = 2
                             AND ahorros.cod_ahorro = 3
                             AND ahorros.sdo_disponible > 0 NO-ERROR.*/
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

            FIND FIRST TempCtas WHERE TempCtas.Agen EQ Ahorros.Agencia
                                  AND TempCtas.TipP EQ "A"
                                  AND TempCtas.Pto EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
            IF NOT AVAIL(TempCtas) THEN DO:
                MESSAGE "Falta configuración con Producto_Ahorro:" Ahorros.Cod_Ahorro SKIP
                        "para la agencia:" Ahorros.Agencia
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.

            IF valDebitar > 0 THEN DO:
                FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ creditos.agencia
                                          AND Comprobantes.Comprobante EQ 21
                                          AND Comprobantes.Estado EQ 1 NO-ERROR.
                IF AVAILABLE comprobantes THEN DO:
                    comprobantes.secuencia = comprobantes.secuencia + 1.

                    pComprobante = comprobantes.comprobante.
                    pSecuencia = comprobantes.secuencia.
                END.

                ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - valDebitar
                       Ahorros.Fec_UltTrans = W_Fecha
                       Ahorros.Num_RetMes = Ahorros.Num_RetMes + 1
                       Ahorros.Num_RetDia = Ahorros.Num_RetDia + 1
                       Ahorros.Val_RetDia = Ahorros.Val_RetDia + valDebitar
                       Ahorros.Val_RetMes = Ahorros.Val_RetMes + valDebitar
                       pCuenta = TempCtas.CtaPro.

                RUN Movimientos.
                RUN GMF.

                valAbono = valAbono + valDebitar.
                pCuota = pCuota - valDebitar.
            END.
        END.
        ELSE DO:
            FIND FIRST creditos WHERE creditos.nit = pNit
                                  AND creditos.cod_credito = pCodCredito
                                  AND creditos.num_credito = pNumCredito NO-LOCK NO-ERROR.

            RETURN.
        END.
    END.

    IF valAbono > 0 THEN DO:
        RUN p-pagocredito.r(INPUT YES,
                            INPUT Creditos.Cod_Credito,
                            INPUT Creditos.Nit,
                            INPUT Creditos.Num_Credito,
                            INPUT valAbono,
                            INPUT pComprobante,
                            INPUT pSecuencia,
                            INPUT 0,
                            INPUT 1,
                            INPUT w_fecha,
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
                            OUTPUT pResult) NO-ERROR.

        IF ERROR-STATUS:ERROR OR P_VlrNoDist GT 0 OR P_VlrNoDist LT 0 THEN DO:
            MESSAGE "El proceso de distribución del pago retornó valor no distribuido:" P_VlrNoDist SKIP
                    "o retornó ERROR..." SKIP
                    "para el Nit:" Creditos.Nit ", Cod_producto:" Creditos.Cod_Credito ", Nro-Crédito:" Creditos.Num_Credito SKIP
                    "Revise por favor... Distribución cancelada."
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.

        FIND FIRST TempCtas WHERE TempCtas.Agen EQ Creditos.Agencia
                              AND TempCtas.TipP EQ "C"
                              AND TempCtas.Pto EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
        IF NOT AVAIL(TempCtas) THEN DO:
            MESSAGE "Falta Configuración con Producto_Credito:" Creditos.Cod_Credito SKIP
                    "Para la agencia:" Creditos.Agencia
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.

    RELEASE comprobantes.
END.


PROCEDURE GMF:
    DEFINE VAR gmfAplicado AS DECIMAL.

    RUN RutGMF.R (INPUT TRUE,
                  INPUT W_Agencia,
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
    FOR EACH Pro_Ahorros WHERE pro_ahorros.tip_ahorro = 2
                           AND pro_ahorros.cod_ahorro = 3 NO-LOCK:
        FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto EQ 1
                              AND CortoLargo.Cod_Producto EQ Pro_Ahorros.Cod_Ahorro
                              AND CortoLargo.Plazo_Inicial GE 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                              BY CortoLargo.Cod_Producto
                                                                              BY CortoLargo.Plazo_Inicial:
            IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                     AND Cuentas.Tipo EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
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
                ASSIGN TempCtas.Age = CortoLargo.Agencia
                       TempCtas.TipP = "A"
                       TempCtas.Pto = CortoLargo.Cod_Producto
                       TempCtas.CtaPro = CortoLargo.Cta_AsoAd
                       TempCtas.CtaSyA = CortoLargo.Cta_SyA.

                FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                                       AND Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
                IF NOT AVAIL(Liqui_Int) THEN DO:
                    MESSAGE "Falta Liqui_Int para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro SKIP
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.
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
                ASSIGN TempCtas.Age = CortoLargo.Agencia
                       TempCtas.TipP = "C"
                       TempCtas.Pto = CortoLargo.Cod_Producto
                       TempCtas.CtaPro = CortoLargo.Cta_AsoAd
                       TempCtas.CtaSyA = CortoLargo.Cta_SyA
                       TempCtas.CtaHon = CortoLargo.Cta_HonorariosDB
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

                ASSIGN TempCtas.Oper = Liqui_Int.Cod_Operacion.
            END.
        END.
    END.
END PROCEDURE.


PROCEDURE Movimientos:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
           Mov_Contable.Cuenta = pCuenta
           Mov_Contable.Nit = Ahorros.Nit
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = "Débito Automático"
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Cen_Costos = W_Cencosgral
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Comprobante = pComprobante
           Mov_Contable.Num_Documento = pSecuencia
           Mov_Contable.Doc_Refer = STRING(pSecuencia)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.Db = valDebitar.

    CREATE Mov_Ahorros.
    ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
           Mov_Ahorros.Age_Destino = Ahorros.Agencia
           Mov_Ahorros.Age_Fuente = W_Agencia
           Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
           Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
           Mov_Ahorros.Fecha = W_Fecha
           Mov_Ahorros.Hora = TIME
           Mov_Ahorros.Nit = Ahorros.Nit
           Mov_Ahorros.Num_Documento = STRING(pSecuencia)
           Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
           Mov_Ahorros.Usuario = W_Usuario
           Mov_Ahorros.Val_Efectivo = valDebitar
           Mov_Ahorros.Cod_Operacion = pOperacion
           Mov_Ahorros.Cpte = pComprobante
           Mov_Ahorros.Descrip = "Débito Automático".
END PROCEDURE.


