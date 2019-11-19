DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD valor AS DECIMAL.

DEFINE TEMP-TABLE ttmov LIKE mov_contable.

DEFINE VAR suma AS INTEGER.

FOR EACH mov_contable WHERE mov_contable.agencia = 4
                        AND mov_contable.fec_contable = 02/28/2017
                        AND mov_contable.comprobante = 21
                        AND mov_contable.num_documento = 73 NO-LOCK BREAK:
    IF mov_contable.cuenta = "21301001" THEN DO:
        CREATE tt.
        tt.nit = mov_contable.nit.
        tt.valor = mov_contable.db.
        suma = suma + mov_contable.db.
    END.

    CREATE ttmov.
    BUFFER-COPY mov_contable TO ttmov.
END.

MESSAGE suma
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH ttmov:
    ttmov.fec_contable = 03/01/2017.
    CREATE mov_contable.
    BUFFER-COPY ttmov TO mov_contable.
END.

FOR EACH tt NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = tt.nit
                         AND ahorros.cod_ahorro = 3
                         AND ahorros.estado = 1 NO-ERROR.
    IF NOT AVAILABLE ahorros THEN
        MESSAGE tt.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ahorros.sdo_disponible = ahorros.sdo_disponible + tt.valor.

    CREATE mov_ahorros.
    ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
           Mov_Ahorros.Age_Destino = Ahorros.Agencia
           Mov_Ahorros.Age_Fuente = ahorros.agencia
           Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
           Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
           Mov_Ahorros.Fecha = TODAY
           Mov_Ahorros.Hora = TIME
           Mov_Ahorros.Nit = Ahorros.Nit
           Mov_Ahorros.Num_Documento = "390"
           Mov_Ahorros.Sdo_Disponible = ahorros.sdo_disponible
           Mov_Ahorros.Usuario = "2305"
           Mov_Ahorros.Val_Efectivo = tt.valor
           Mov_Ahorros.Cod_Operacion = 010101001
           Mov_Ahorros.Cpte = 21
           Mov_Ahorros.Descrip = "ReversiónDebitoAutomatico".
END.
