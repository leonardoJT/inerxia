DISABLE TRIGGERS FOR LOAD OF ahorros.
DISABLE TRIGGERS FOR LOAD OF mov_ahorros.

FOR EACH mov_ahorros WHERE mov_ahorros.agencia <> 0
                       AND mov_ahorros.cod_ahorro = 8
                       AND mov_ahorros.cue_ahorros <> ""
                       AND mov_ahorros.fecha = TODAY
                       AND mov_ahorros.num_documento = "1423":
    FIND FIRST ahorros WHERE ahorros.nit = mov_ahorros.nit
                         AND ahorros.cue_ahorros = mov_ahorros.cue_ahorros NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        ahorros.sdo_disponible = ahorros.sdo_disponible - mov_ahorros.val_efectivo.
        DELETE mov_ahorros.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
