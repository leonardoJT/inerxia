DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER cuentaCxC AS CHARACTER.
DEFINE INPUT PARAMETER cen_costosCxC AS INTEGER.
DEFINE INPUT PARAMETER agenciaCxC AS INTEGER.
DEFINE INPUT PARAMETER valorCxC AS DECIMAL.

{Incluido/Variable.I "SHARED"}

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
DEFINE VAR p_IAC AS DECIMAL.

DEFINE TEMP-TABLE TempCtas
    FIELD Agen AS INTEGER
    FIELD TipP AS CHARACTER FORM "X(1)"
    FIELD Pto AS INTEGER
    FIELD CtaPro AS CHARACTER 
    FIELD CtaIng AS CHARACTER 
    FIELD CtaLiq AS CHARACTER 
    FIELD IntAnt AS CHARACTER 
    FIELD IntMor AS CHARACTER 
    FIELD DifCoD AS CHARACTER 
    FIELD DifCoH AS CHARACTER 
    FIELD CtaPol AS CHARACTER 
    FIELD CtaHon AS CHARACTER 
    FIELD CtaCos AS CHARACTER 
    FIELD CtaGar AS CHARACTER 
    FIELD CtaCGa AS CHARACTER 
    FIELD Oper AS INTEGER
    FIELD CtaSyA AS CHARACTER.

DEFINE VAR saldoMinimo AS DECIMAL.

RUN CargarCuentas.

/* Debitamos las cuentas de ahorro */
FIND FIRST ahorros WHERE ahorros.nit = pNit
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

        valDebitar = valorCxC.

        IF ahorros.sdo_disponible - saldoMinimo < valorCxC * 1.004 THEN
            valDebitar = TRUNCATE(((ahorros.sdo_disponible - saldoMinimo) * 100) / 100.4,0).
    END.

    FIND FIRST TempCtas WHERE TempCtas.Agen = Ahorros.Agencia

        /* oakley */
                          AND TempCtas.TipP = "A"
                          AND TempCtas.Pto = Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(TempCtas) THEN DO:
        MESSAGE "Falta configuración con Producto_Ahorro:" Ahorros.Cod_Ahorro SKIP
                "para la agencia:" Ahorros.Agencia
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    IF valDebitar > 0 THEN DO:
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia = ahorros.agencia
                                      AND Comprobantes.Comprobante = 21
                                      AND Comprobantes.Estado = 1 NO-ERROR.
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

        IF valDebitar > 0 THEN DO:
            CREATE Mov_Contable.
            ASSIGN Mov_Contable.Agencia = agenciaCxC
                   Mov_Contable.Cuenta = cuentaCxC
                   Mov_Contable.Nit = Ahorros.Nit
                   Mov_Contable.Fec_Contable = W_Fecha
                   Mov_Contable.Comentario = "Déb.Automático CxC Auxilios"
                   Mov_Contable.Usuario = W_Usuario
                   Mov_Contable.Cen_Costos = cen_costosCxC
                   Mov_Contable.Destino = W_Agencia
                   Mov_Contable.Comprobante = pComprobante
                   Mov_Contable.Num_Documento = pSecuencia
                   Mov_Contable.Doc_Refer = STRING(pSecuencia)
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Hora = TIME
                   Mov_Contable.Estacion = W_Estacion
                   Mov_Contable.cr = valDebitar.

            IF agenciaCxC <> ahorros.agencia THEN DO:
                CREATE Mov_Contable.
                ASSIGN Mov_Contable.Agencia = agenciaCxC
                       Mov_Contable.Cuenta = "19040501"
                       Mov_Contable.Nit = STRING(ahorros.agencia,"999")
                       Mov_Contable.Fec_Contable = W_Fecha
                       Mov_Contable.Comentario = "Déb.Automático CxC Auxilios"
                       Mov_Contable.Usuario = W_Usuario
                       Mov_Contable.Cen_Costos = cen_costosCxC
                       Mov_Contable.Destino = ahorros.agencia
                       Mov_Contable.Comprobante = pComprobante
                       Mov_Contable.Num_Documento = pSecuencia
                       Mov_Contable.Doc_Refer = STRING(pSecuencia)
                       Mov_Contable.Fec_Grabacion = TODAY
                       Mov_Contable.Hora = TIME
                       Mov_Contable.Estacion = W_Estacion
                       Mov_Contable.db = valDebitar.

                CREATE Mov_Contable.
                ASSIGN Mov_Contable.Agencia = ahorros.agencia
                       Mov_Contable.Cuenta = "19040501"
                       Mov_Contable.Nit = STRING(agenciaCxC,"999")
                       Mov_Contable.Fec_Contable = W_Fecha
                       Mov_Contable.Comentario = "Déb.Automático CxC Auxilios"
                       Mov_Contable.Usuario = W_Usuario
                       Mov_Contable.Cen_Costos = cen_costosCxC
                       Mov_Contable.Destino = agenciaCxC
                       Mov_Contable.Comprobante = pComprobante
                       Mov_Contable.Num_Documento = pSecuencia
                       Mov_Contable.Doc_Refer = STRING(pSecuencia)
                       Mov_Contable.Fec_Grabacion = TODAY
                       Mov_Contable.Hora = TIME
                       Mov_Contable.Estacion = W_Estacion
                       Mov_Contable.cr = valDebitar.
            END.
        END.

        RELEASE comprobantes.
    END.
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
        /*MESSAGE "La rutina RutGMF retornó ERROR. No se permite la operación." SKIP
                "Los datos son los siguientes:" SKIP
                "Cédula:" ahorros.nit SKIP
                "Cuenta:" ahorros.cue_ahorros SKIP
                "Saldo disponible:" ahorros.sdo_disponible SKIP
                "Valor a debitar:" valDebitar SKIP
                "# de crédito:" creditos.num_Credito SKIP(2)
                "Por favor, reportar este error con los datos descritos a Leonardo G. Ocampo," SKIP
                "para hacer el debido seguimiento y la debida corrección. Muchas gracias."
            VIEW-AS ALERT-BOX ERROR.*/

        RETURN ERROR.
    END.
END PROCEDURE.


PROCEDURE CargarCuentas:
    FOR EACH Pro_Ahorros WHERE pro_ahorros.tip_ahorro = 1
                           AND pro_ahorros.cod_ahorro = 4 NO-LOCK:
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
                ASSIGN TempCtas.Age = CortoLargo.Agencia
                       TempCtas.TipP = "A"
                       TempCtas.Pto = CortoLargo.Cod_Producto
                       TempCtas.CtaPro = CortoLargo.Cta_AsoAd.
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
           Mov_Ahorros.Descrip = "Déb.Automático CxC Auxilios".

END PROCEDURE.


