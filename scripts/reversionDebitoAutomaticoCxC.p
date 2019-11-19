DEFINE VAR cont AS INTEGER.
DEFINE VAR saldo AS DECIMAL.
DEFINE VAR vAgencia AS INTEGER INITIAL 4.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER.

DEFINE TEMP-TABLE ttmov LIKE mov_contable.

FOR EACH anexos WHERE anexos.agencia = vAgencia
                  AND anexos.cuenta = "16909502"
                  AND anexos.ano = 2017 NO-LOCK:
    saldo = anexos.sdo_inicial.

    DO cont = 1 TO 8:
        saldo = saldo + anexos.db[cont] - anexos.cr[cont].
    END.

    IF saldo < 0 THEN DO:
        CREATE tt.
        tt.nit = anexos.nit.
    END.
END.

FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                        AND mov_contable.fec_contable = 08/14/2017
                        AND mov_contable.comprobante = 21
                        AND mov_contable.usuario = "desarrollo" NO-LOCK:
    FIND FIRST tt WHERE tt.nit = mov_contable.nit NO-LOCK NO-ERROR.
    IF AVAILABLE tt THEN DO:
        CREATE ttmov.
        BUFFER-COPY mov_contable TO ttmov.

        ttmov.fec_contable = TODAY.
        ttmov.comentario = "Rev-" + ttmov.comentario.
    
        IF ttmov.db > 0 THEN DO:
            ttmov.cr = ttmov.db.
            ttmov.db = 0.
        END.
        ELSE DO:
            ttmov.db = ttmov.cr.
            ttmov.cr = 0.
        END.

        IF mov_contable.cuenta = "21050501" THEN DO:
            FIND FIRST ahorros WHERE ahorros.nit = mov_contable.nit
                                 AND ahorros.tip_ahorro = 1
                                 AND ahorros.cod_ahorro = 4
                                 AND ahorros.estado = 1 NO-ERROR.
            IF AVAILABLE ahorros THEN DO:
                ahorros.sdo_disponible = ahorros.sdo_disponible + mov_contable.db.

                CREATE Mov_Ahorros.
                ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
                       Mov_Ahorros.Age_Destino = Ahorros.Agencia
                       Mov_Ahorros.Age_Fuente = 1
                       Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                       Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
                       Mov_Ahorros.Fecha = TODAY
                       Mov_Ahorros.Hora = TIME
                       Mov_Ahorros.Nit = Ahorros.Nit
                       Mov_Ahorros.Num_Documento = string(mov_contable.num_documento)
                       Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
                       Mov_Ahorros.Usuario = mov_contable.usuario
                       Mov_Ahorros.Val_Efectivo = mov_contable.db
                       Mov_Ahorros.Cod_Operacion = 010101001
                       Mov_Ahorros.Cpte = mov_contable.comprobante
                       Mov_Ahorros.Descrip = "Rev-" + mov_contable.comentario.
            END.
        END.
    END.
END.

FOR EACH ttmov NO-LOCK:
    CREATE mov_contable.
    BUFFER-COPY ttmov TO mov_contable.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
