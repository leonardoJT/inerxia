FOR EACH ahorros WHERE ahorros.tip_ahorro = 2
                   AND ahorros.cod_ahorro = 3:
    FOR EACH mov_ahorros WHERE mov_ahorros.nit = ahorros.nit
                           AND mov_ahorros.cod_ahorro = ahorros.cod_ahorro
                           AND mov_ahorros.cue_ahorro = ahorros.cue_ahorro
                           AND mov_ahorros.fecha = 05/31/2011 NO-LOCK:
        FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
        IF AVAILABLE operacion THEN DO:
            IF operacion.tipo_operacion = 1 THEN
                ahorros.Sdo_Anuales[5] = ahorros.Sdo_Anuales[5] + (mov_ahorros.val_efectivo + mov_ahorros.val_cheque).

            IF operacion.tipo_operacion = 2 THEN
                Ahorros.Sdo_Anuales[5] = Ahorros.Sdo_Anuales[5] - (mov_ahorros.val_efectivo + mov_ahorros.val_cheque).
        END.
    END.
END.
