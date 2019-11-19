DEFINE TEMP-TABLE tt LIKE mov_ahorros.

FOR EACH mov_ahorros WHERE fecha = 12/04/2018 AND num_documento = "1243" NO-LOCK:
    CREATE tt.
    BUFFER-COPY mov_ahorros TO tt.
END.

FOR EACH tt NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = tt.nit
                         AND ahorros.cod_ahorro = tt.cod_ahorro
                         AND ahorros.cue_ahorros = tt.cue_ahorros NO-ERROR.
    
    ahorros.sdo_disponible = ahorros.sdo_disponible - tt.val_efectivo.

    CREATE mov_ahorros.
    BUFFER-COPY tt TO mov_ahorros.
    mov_ahorros.cod_operacion = 010102001.
    mov_ahorros.descrip = "Rev-" + mov_ahorros.descrip.
    mov_ahorros.sdo_disponible = mov_ahorros.sdo_disponible - tt.val_efectivo.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
