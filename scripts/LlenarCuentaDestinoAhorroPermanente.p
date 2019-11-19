DISABLE TRIGGERS FOR LOAD OF ahorros.
DEFINE BUFFER bfrAhorros FOR ahorros.
DEFINE VAR ageDestino AS INTEGER.
DEFINE VAR cueDestino AS CHARACTER.

DEFINE TEMP-TABLE registros
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD cue_ahorros AS CHARACTER.


FOR EACH ahorros WHERE ahorros.cod_ahorro = 3 AND ahorros.estado = 1 AND ahorros.cue_destino = "":
    FIND FIRST bfrAhorros WHERE bfrAhorros.agencia = ahorros.agencia
                            AND bfrAhorros.nit = ahorros.nit
                            AND bfrAhorros.cod_ahorro = 4
                            AND bfrAhorros.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfrAhorros THEN DO:
        CREATE registros.
        registros.agencia = ahorros.agencia.
        registros.nit = ahorros.nit.
        registros.cue_ahorros = ahorros.cue_ahorros.

        ahorros.cue_destino = "".
        ahorros.agencia_destino = ?.
        ahorros.pro_destino = ?.
        ahorros.Des_Intereses = 3.
    END.
    ELSE DO:
        Ahorros.Agencia_Destino = ahorros.agencia.
        Ahorros.Pro_Destino = 4.
        Ahorros.Cue_Destino = bfrAhorros.cue_ahorros.
        ahorros.des_interes = 1.
    END.
END.

FOR EACH registros NO-LOCK:
    FIND FIRST ahorros WHERE Ahorros.agencia = registros.agencia
                         AND Ahorros.nit = registros.nit
                         AND Ahorros.cod_ahorro = 4 NO-ERROR.
    IF AVAILABLE ahorros THEN
        ahorros.estado = 1.
    ELSE DO:
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

    cueDestino = ahorros.cue_ahorros.

    FIND FIRST ahorros WHERE Ahorros.agencia = registros.agencia
                         AND Ahorros.nit = registros.nit
                         AND Ahorros.cod_ahorro = 3
                         AND ahorros.cue_ahorros = registros.cue_ahorros
                         AND ahorros.estado = 1 NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        Ahorros.Agencia_Destino = registros.agencia.
        Ahorros.Pro_Destino = 4.
        Ahorros.Cue_Destino = cueDestino.
        ahorros.des_interes = 1.
    END.
END.
    
MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

