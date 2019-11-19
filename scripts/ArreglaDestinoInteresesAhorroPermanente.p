DEFINE BUFFER bfrAhorros FOR ahorros.
    
FOR EACH ahorros WHERE cod_ahorro = 3 AND estado = 1:
    FIND FIRST bfrAhorros WHERE bfrAhorros.nit = ahorros.nit
                            AND bfrAhorros.cod_ahorro = 4
                            AND bfrAhorros.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE bfrAhorros THEN DO:
        ahorros.cue_destino = bfrAhorros.cue_ahorros.
        ahorros.agencia_destino = bfrAhorros.agencia.
        ahorros.pro_destino = 4.
        ahorros.Des_Intereses = 1.
    END.
    ELSE DO:
        ahorros.cue_destino = "".
        ahorros.agencia_destino = ?.
        ahorros.pro_destino = ?.
        ahorros.Des_Intereses = 3.
    END.
END.
