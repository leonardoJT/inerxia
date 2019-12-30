DEFINE INPUT PARAMETER pRowIdCredito AS ROWID.
DEFINE INPUT PARAMETER pValorEfectivo AS DECIMAL.
DEFINE INPUT PARAMETER pValorCheque AS DECIMAL.
DEFINE INPUT PARAMETER pComprobante AS INTEGER.
DEFINE INPUT PARAMETER pDescripcion AS CHARACTER.
DEFINE INPUT PARAMETER pNumDocumento AS INTEGER.
DEFINE INPUT PARAMETER pAgenciaDelUsuario AS INTEGER.
DEFINE INPUT PARAMETER pUsuario AS CHARACTER.

FIND FIRST creditos WHERE ROWID(creditos) = pRowIdCredito NO-ERROR.

creditos.Sdo_capital = Creditos.Sdo_capital + pValorEfectivo + pValorCheque.

CREATE Mov_Creditos.
Mov_Creditos.agencia = creditos.agencia.
Mov_Creditos.Cod_Credito = creditos.cod_credito.
Mov_Creditos.Cod_Operacion = 020102001.
Mov_Creditos.Cpte = pComprobante.
Mov_Creditos.Descrip = pDescripcion.
Mov_Creditos.Fecha = TODAY.
Mov_Creditos.Hora = TIME.
Mov_Creditos.Nit = creditos.nit.
Mov_Creditos.Num_Credito = credito.num_credito.
Mov_Creditos.Num_Documento = STRING(pNumDocumento).
Mov_Creditos.Ofi_Destino = pAgenciaDelUsuario.
Mov_Creditos.Ofi_Fuente = creditos.agencia.
Mov_Creditos.Pagare = creditos.pagare.
Mov_Creditos.Sdo_Capital = credirtos.sdo_capital.
Mov_Creditos.Usuario = pUsuario.
Mov_Creditos.Val_Cheque = pValorCheque.
Mov_Creditos.Val_Efectivo = pValorEfectivo.

/* oakley */

IF vComision > 0 THEN DO:
                CREATE Mov_Creditos.
                RUN movCreditos.

                ASSIGN Mov_Creditos.Cod_Operacion = 020102001
                       Mov_Creditos.Num_Documento = STRING(pNumDocumento)
                       Mov_Creditos.Val_Efectivo = vComision
                       Mov_Creditos.Descrip = vRev + aplicarVisionamos.TERMINAL_ubicacion.
            END.


            FIND FIRST pro_creditos WHERE pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
            IF AVAILABLE(pro_creditos) THEN
                FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.

            IF AVAILABLE(indicadores) THEN DO:
                tasaCredito = (((EXP((indicadores.tasa / 100) + 1,1 / 12)) - 1) * 100) * 12.

                IF STRING(tasaCredito,">>9.99") <> STRING(creditos.tasa,">>9.99") THEN DO:
                    CREATE Mov_Creditos.
                    RUN movCreditos.

                    ASSIGN Mov_Creditos.Cod_Operacion = 999999999
                           Mov_Creditos.Descrip = "Cambio de Tasa " + STRING(creditos.tasa,">>9.99") + "-->" + STRING(tasaCredito,">>9.99").

                    creditos.tasa = tasaCredito.
                END.
            END.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = Creditos.agencia.
            Mov_Contable.Destino = Creditos.agencia.
            Mov_Contable.Cuenta = "14420505".
            Mov_Contable.Nit = creditos.nit.
            Mov_contable.db = vdb.
            mov_contable.cr = vcr.

            CREATE mov_contable.
            RUN movContable.

            Mov_Contable.Agencia = Creditos.agencia.
            Mov_Contable.Destino = Creditos.agencia.
            Mov_Contable.Cuenta = "24459550".
            Mov_Contable.Nit = nitCompensacion.
            Mov_contable.db = vcr.
            mov_contable.cr = vdb.

            IF aplicarVisionamos.entidadDuenaTerminal = "00000018" AND aplicarVisionamos.entidadDuenaCuentaOrigen = "00000018" THEN DO:
                ASSIGN mov_contable.cuenta = "11050501"
                       mov_contable.nit = creditos.nit.

                FIND FIRST usuarios WHERE usuarios.usuario = aplicarVisionamos.usuario NO-LOCK NO-ERROR.
                IF AVAILABLE usuarios THEN
                    mov_contable.agencia = usuarios.agencia.
            END.


            /* Para Sucursales y Agencias */
            IF creditos.agencia <> 1 AND mov_contable.cuenta <> "11050501" THEN DO:
                RUN cuentaSucursales&agencias.

                mov_contable.cuenta = cuentaSyA.
                mov_contable.nit = "001".

                FIND FIRST comprobantes WHERE comprobantes.comprobante = 22
                                          AND comprobantes.agencia = 1 NO-ERROR.
                IF AVAILABLE(comprobantes) THEN
                    ASSIGN pNumDocumento = comprobantes.secuencia + 1
                           comprobantes.secuencia = comprobantes.secuencia + 1.

                RELEASE comprobantes.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = cuentaSyA
                       Mov_Contable.Nit = STRING(creditos.agencia,"999")
                       Mov_contable.db = vdb
                       mov_contable.cr = vcr.

                CREATE mov_contable.
                RUN movContable.

                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = "24459550"
                       Mov_Contable.Nit = nitCompensacion
                       Mov_contable.db = vcr
                       mov_contable.cr = vdb.
            END.

            RELEASE creditos.
