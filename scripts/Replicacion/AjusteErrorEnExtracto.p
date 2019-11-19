FOR EACH BD_WEB.mov_ahorros WHERE BD_WEB.mov_ahorros.cod_operacion = 010101003
                              AND BD_WEB.mov_ahorros.descrip <> "Abono Liq.Interés" NO-LOCK:
    FIND FIRST bdcentral.mov_ahorros WHERE bdcentral.mov_ahorros.nit = BD_WEB.mov_ahorros.nit
                                       AND bdcentral.mov_ahorros.cue_ahorros = BD_WEB.mov_ahorros.cue_ahorros
                                       AND bdcentral.mov_ahorros.cod_operacion = BD_WEB.mov_ahorros.cod_operacion
                                       AND bdcentral.mov_ahorros.fecha = BD_WEB.mov_ahorros.fecha
                                       AND bdcentral.mov_ahorros.descrip = BD_WEB.mov_ahorros.descrip
                                       AND bdcentral.mov_ahorros.val_efectivo <> BD_WEB.mov_ahorros.val_efectivo NO-LOCK NO-ERROR.
    IF AVAILABLE bdcentral.mov_ahorros THEN DO:
        DISPLAY bd_web.mov_ahorros.descrip bd_web.mov_ahorros.val_efectivo bdcentral.mov_ahorros.val_efectivo.
    END.
END.
