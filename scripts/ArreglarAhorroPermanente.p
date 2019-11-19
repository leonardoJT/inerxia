
DEFINE BUFFER bfrAhorros FOR ahorros.

FOR EACH ahorros WHERE cod_ahorro = 3
                   AND estado = 1
                   AND cue_destino = ""
                   /*AND cue_destino = "64278"*/:
    FIND FIRST bfrAhorros WHERE bfrAhorros.nit = ahorros.nit
                            AND bfrAhorros.cod_ahorro = 9
                            AND bfrAhorros.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfrAhorros THEN
        MESSAGE ahorros.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        ahorros.agencia_Destino = bfrAhorros.agencia.
        ahorros.pro_destino = bfrAhorros.cod_ahorro.
        ahorros.cue_destino = bfrAhorros.cue_ahorros.
    END.
END.
