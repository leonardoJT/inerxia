DEFINE VAR time1 AS INTEGER.
DEFINE VAR time2 AS INTEGER.

time1 = TIME.

DEFINE TEMP-TABLE registros
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD sdo_disponible AS DECIMAL.

FOR EACH agencias NO-LOCK:
    EMPTY TEMP-TABLE registros.

    FIND FIRST comprobantes WHERE comprobantes.agencia = agencias.agencia
                              AND comprobantes.comprobante = 21 NO-ERROR.

    comprobantes.secuencia = comprobantes.secuencia + 1.

    /* 1. Debitamos las cuentas de Interès de Ahorro Permanente */
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND ahorros.cod_ahorro = 9
                       AND ahorros.sdo_disponible >= 0:
        IF sdo_disponible > 0 THEN DO:
            CREATE registros.
            ASSIGN registros.agencia = ahorros.agencia
                   registros.nit = ahorros.nit.
                   registros.sdo_disponible = ahorros.sdo_disponible.

            ahorros.sdo_disponible = 0.

            CREATE Mov_Ahorros.
            ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
                   Mov_Ahorros.Age_Destino = Ahorros.Agencia
                   Mov_Ahorros.Age_Fuente = ahorros.agencia
                   Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                   Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
                   Mov_Ahorros.Fecha = TODAY
                   Mov_Ahorros.Hora = TIME
                   Mov_Ahorros.Nit = Ahorros.Nit
                   Mov_Ahorros.Num_Documento = STRING(comprobantes.secuencia)
                   Mov_Ahorros.Sdo_Disponible = 0
                   Mov_Ahorros.Usuario = "2305"
                   Mov_Ahorros.Val_Efectivo = registros.sdo_disponible
                   Mov_Ahorros.Cod_Operacion = 010102001
                   Mov_Ahorros.Cpte = 21
                   Mov_Ahorros.Descrip = "TraslIntAhPermToAhAlaVista".

            CREATE Mov_Contable.
            ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
                   Mov_Contable.Cuenta = "21050502"
                   Mov_Contable.Nit = Ahorros.Nit
                   Mov_Contable.Fec_Contable = TODAY
                   Mov_Contable.Comentario = "TraslIntAhPermToAhAlaVista"
                   Mov_Contable.Usuario = "2305"
                   Mov_Contable.Cen_Costos = 999
                   Mov_Contable.Destino = ahorros.agencia
                   Mov_Contable.Comprobante = 21
                   Mov_Contable.Num_Documento = comprobantes.secuencia
                   Mov_Contable.Doc_Refer = ahorros.cue_ahorros
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Hora = TIME
                   Mov_Contable.Db = registros.sdo_disponible.
        END.

        ahorros.estado = 2.
        ahorros.fec_ulttransaccion = TODAY.
        ahorros.fec_cancelacion = TODAY.
    END.

    /* 2. Acreditamos las cuentas de Ahorro a la Vista */
    FOR EACH registros NO-LOCK:
        FIND FIRST ahorros WHERE ahorros.agencia = registros.agencia
                             AND ahorros.cod_ahorro = 4
                             AND ahorros.nit = registros.nit
                             AND ahorros.estado = 1 NO-ERROR.
        IF NOT AVAILABLE ahorros THEN DO:
            FIND FIRST ahorros WHERE ahorros.agencia = registros.agencia
                                 AND ahorros.cod_ahorro = 4
                                 AND ahorros.nit = registros.nit NO-ERROR.
            IF NOT AVAILABLE ahorros THEN DO:
                CREATE ahorros.

                FIND FIRST pro_ahorros WHERE pro_ahorros.Cod_ahorro = 4 NO-ERROR.
                Pro_Ahorros.Num_Consecutivo = Pro_Ahorros.Num_Consecutivo + 1.
                Ahorros.Cue_Ahorros = STRING(Pro_Ahorros.Num_Consecutivo).
                Ahorros.Agencia = registros.agencia.
                Ahorros.Tip_Ahorro = 1.
                Ahorros.Cod_Ahorro = 4.
                Ahorros.Nit = registros.nit.
                Ahorros.trf = 0.
                Ahorros.TRF_notas = "".
                Ahorros.Usu_Creacion = "2305".

                FIND FIRST clientes WHERE clientes.nit = registros.nit NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    Ahorros.IdNombre = Clientes.Nombre.
                    Ahorros.IdApellido1 = Clientes.Apellido1.
                END.
            END.

            ahorros.estado = 1.
            Ahorros.Detalle_Estado = 1.
            ahorros.fec_Cancelacion = ?.
            Ahorros.Per_Liquidacion = 1.
            Ahorros.Tasa = 3.6.
            Ahorros.Fec_ProLiquidacion = TODAY.
            Ahorros.For_Liquidacion = 2.
            Ahorros.Ind_Tipo_Subsidio = 1.
        END.

        ahorros.sdo_disponible = ahorros.sdo_disponible + registros.sdo_disponible.
        ahorros.fec_ulttransaccion = TODAY.
        
        CREATE Mov_Ahorros.
        ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
               Mov_Ahorros.Age_Destino = Ahorros.Agencia
               Mov_Ahorros.Age_Fuente = ahorros.agencia
               Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
               Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
               Mov_Ahorros.Fecha = TODAY
               Mov_Ahorros.Hora = TIME
               Mov_Ahorros.Nit = Ahorros.Nit
               Mov_Ahorros.Num_Documento = STRING(comprobantes.secuencia)
               Mov_Ahorros.Sdo_Disponible = ahorros.sdo_disponible
               Mov_Ahorros.Usuario = "2305"
               Mov_Ahorros.Val_Efectivo = registros.sdo_disponible
               Mov_Ahorros.Cod_Operacion = 010101001
               Mov_Ahorros.Cpte = 21
               Mov_Ahorros.Descrip = "TraslIntAhPermToAhAlaVista".

        CREATE Mov_Contable.
        ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
               Mov_Contable.Cuenta = "21050501"
               Mov_Contable.Nit = Ahorros.Nit
               Mov_Contable.Fec_Contable = TODAY
               Mov_Contable.Comentario = "TraslIntAhPermToAhAlaVista"
               Mov_Contable.Usuario = "2305"
               Mov_Contable.Cen_Costos = 999
               Mov_Contable.Destino = ahorros.agencia
               Mov_Contable.Comprobante = 21
               Mov_Contable.Num_Documento = comprobantes.secuencia
               Mov_Contable.Doc_Refer = ahorros.cue_ahorros
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Hora = TIME
               Mov_Contable.cr = registros.sdo_disponible.

    END.
END.

time2 = TIME.

MESSAGE STRING(time2 - time1,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
