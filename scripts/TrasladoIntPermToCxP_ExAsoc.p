DEFINE VAR time1 AS INTEGER.
DEFINE VAR time2 AS INTEGER.

DEFINE VAR debito AS DECIMAL.

time1 = TIME.

FOR EACH agencias NO-LOCK:
    FIND FIRST comprobantes WHERE comprobantes.agencia = agencias.agencia
                              AND comprobantes.comprobante = 21 NO-ERROR.

    comprobantes.secuencia = comprobantes.secuencia + 1.

    /* 1. Debitamos las cuentas de Interès de Ahorro Permanente */
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND ahorros.cod_ahorro = 9
                       AND ahorros.sdo_disponible > 0:
        IF sdo_disponible > 0 THEN DO:
            debito = ahorros.sdo_disponible.
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
                   Mov_Ahorros.Val_Efectivo = debito
                   Mov_Ahorros.Cod_Operacion = 010102001
                   Mov_Ahorros.Cpte = 21
                   Mov_Ahorros.Descrip = "TraslIntAhPermToAhAlaVista".

            CREATE Mov_Contable.
            ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
                   Mov_Contable.Cuenta = "21050502"
                   Mov_Contable.Nit = Ahorros.Nit
                   Mov_Contable.Fec_Contable = TODAY
                   Mov_Contable.Comentario = "LiquidaIntTrimestral"
                   Mov_Contable.Usuario = "2305"
                   Mov_Contable.Cen_Costos = 999
                   Mov_Contable.Destino = ahorros.agencia
                   Mov_Contable.Comprobante = 21
                   Mov_Contable.Num_Documento = comprobantes.secuencia
                   Mov_Contable.Doc_Refer = ahorros.cue_ahorros
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Hora = TIME
                   Mov_Contable.Db = debito.

            CREATE Mov_Contable.
            ASSIGN Mov_Contable.Agencia = ahorros.agencia
                   Mov_Contable.Cuenta = "24650501"
                   Mov_Contable.Nit = ahorros.Nit
                   Mov_Contable.Fec_Contable = TODAY
                   Mov_Contable.Comentario = "LiquidaIntTrimestral"
                   Mov_Contable.Usuario = "2305"
                   Mov_Contable.Cen_Costos = 999
                   Mov_Contable.Destino = ahorros.agencia
                   Mov_Contable.Comprobante = 21
                   Mov_Contable.Num_Documento = comprobantes.secuencia
                   Mov_Contable.Doc_Refer = "29042014"
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Hora = TIME
                   Mov_Contable.cr = debito.
        END.

        ahorros.estado = 2.
        ahorros.fec_ulttransaccion = TODAY.
        ahorros.fec_cancelacion = TODAY.
    END.
END.

time2 = TIME.

MESSAGE STRING(time2 - time1,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
