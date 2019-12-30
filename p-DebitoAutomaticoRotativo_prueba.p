DEFINE INPUT PARAMETER pRowIdCredito AS ROWID.
DEFINE INPUT PARAMETER debitaAhPerm AS LOGICAL.
DEFINE INPUT PARAMETER pUsuario AS CHARACTER.
/* ----------------------- */

{Incluido\variable.i "NEW GLOBAL SHARED"}

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
    FIELD cuenta AS CHARACTER.

DEFINE VAR saldoMinimo AS DECIMAL.

RUN CargarCuentas.

FIND FIRST creditos WHERE ROWID(creditos) = pRowIdCredito NO-ERROR.
IF AVAILABLE creditos THEN DO:
    FIND FIRST Comprobantes WHERE Comprobantes.Agencia = creditos.agencia
                              AND Comprobantes.Comprobante = 21
                              AND Comprobantes.Estado = 1 NO-ERROR.
    IF AVAILABLE comprobantes THEN DO:
        pComprobante = comprobantes.comprobante.
        pSecuencia = comprobantes.secuencia + 1.
    END.

    /* 1. Determinamos el valor de la cuota a debitar */
    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.fec_pago <= TODAY
                           AND facturacion.estado = 1 NO-LOCK:
        pCuota = pcuota + (facturacion.cuota - facturacion.pago_mora - facturacion.pago_intCorriente - facturacion.pago_capital).
    END.

    IF pCuota > creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.INT_moraDifCob + creditos.costas + creditos.polizas + creditos.honorarios - creditos.INT_anticipado THEN
        pCuota = creditos.sdo_capital + creditos.INT_corriente + creditos.INT_morCobrar + creditos.INT_difCobro + creditos.INT_moraDifCob + creditos.costas + creditos.polizas + creditos.honorarios - creditos.INT_anticipado.

    IF pCuota < 0 THEN
        pCuota = 0.

    /* 2. Debitamos las cuentas de ahorro */
    IF pCuota > 0 THEN DO:
        valDebitar = pCuota.

        FIND FIRST ahorros WHERE ahorros.agencia = creditos.agencia
                             AND ahorros.nit = creditos.nit
                             AND ahorros.tip_ahorro = 1
                             AND ahorros.cod_ahorro = 9
                             AND ahorros.sdo_disponible > 0 NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            /* Valido la parametrización contable */
            FIND FIRST TempCtas WHERE TempCtas.Agencia = Ahorros.Agencia
                                  AND TempCtas.cod_ahorro = Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(TempCtas) THEN DO:
                RUN escribirLog (INPUT "Débito automático Rotativos: Aborta proceso por no encontrar la configuración contable para la línea de ahoros ahorros.cod_ahorro") NO-ERROR.

                RETURN ERROR.
            END.
            /* ----------------------- */

            FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE pro_ahorros THEN DO:
                IF pro_ahorros.id_salMinimo = YES THEN
                    saldoMinimo = pro_ahorros.val_sdoMinimo.
                ELSE
                    saldoMinimo = 0.
            END.

            IF ahorros.sdo_disponible - saldoMinimo < valDebitar * 1.004 THEN
                valDebitar = TRUNCATE(((ahorros.sdo_disponible - saldoMinimo) * 100) / 100.4,0).

            IF valDebitar > 0 THEN DO:
                ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - valDebitar
                       Ahorros.Fec_UltTrans = TODAY
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
            /* Valido la parametrización contable */
            FIND FIRST TempCtas WHERE TempCtas.Agencia = Ahorros.Agencia
                                  AND TempCtas.cod_ahorro = Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
            IF NOT AVAILABLE(TempCtas) THEN DO:
                RUN escribirLog (INPUT "Débito automático Rotativos: cliente_id: " + creditos.nit + " num_credito: " + STRING(creditos.num_credito) + " - Aborta proceso por no encontrar la configuración contable para la línea de ahoros ahorros.cod_ahorro") NO-ERROR.

                RETURN ERROR.
            END.
            /* ----------------------- */

            FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE pro_ahorros THEN DO:
                IF pro_ahorros.id_salMinimo = YES THEN
                    saldoMinimo = pro_ahorros.val_sdoMinimo.
                ELSE
                    saldoMinimo = 0.
            END.

            IF ahorros.sdo_disponible - saldoMinimo < valDebitar * 1.004 THEN
                valDebitar = TRUNCATE(((ahorros.sdo_disponible - saldoMinimo) * 100) / 100.4,0).

            IF valDebitar > 0 THEN DO:
                ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - valDebitar
                       Ahorros.Fec_UltTrans = TODAY
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
                            INPUT TODAY,
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
        RUN escribirLog (INPUT "Débito automático Rotativos: Aborta proceso por error en la rutina GMF - Cliente_id: " + ahorros.nit + " - Cuenta: " + ahorros.cue_ahorros + " - Valor a debitar: " + STRING(valDebitar,"$>>>,>>>,>>9.99")) NO-ERROR.
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
                IF AVAILABLE(Cuentas) THEN
                    FIND FIRST Cuentas WHERE Cuentas.Cuenta = CortoLargo.Cta_SyA
                                         AND Cuentas.Tipo = 2
                                         AND Cuentas.Estado = 1 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE(Cuentas) THEN DO:
                    MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir activas en Cuentas..." SKIP
                            "para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro SKIP
                            "de la Agencia:" CortoLargo.Agencia
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                CREATE TempCtas.
                TempCtas.Agencia = CortoLargo.Agencia.
                TempCtas.cod_ahorro = CortoLargo.Cod_Producto.
                TempCtas.cuenta = CortoLargo.Cta_AsoAd.

                FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto = 1
                                       AND Liqui_Int.Cod_Producto = CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
                IF NOT AVAILABLE(Liqui_Int) THEN DO:
                    MESSAGE "Falta Liqui_Int para el Pro_Ahorros.Cod_Ahorro:" Pro_Ahorros.Cod_Ahorro SKIP
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.


PROCEDURE Movimientos:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
           Mov_Contable.Cuenta = TempCtas.cuenta
           Mov_Contable.Nit = Ahorros.Nit
           Mov_Contable.Fec_Contable = TODAY
           Mov_Contable.Comentario = "Débito Automático Rotativos"
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
           Mov_Ahorros.Fecha = TODAY
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
