DISABLE TRIGGERS FOR LOAD OF bd_web.mov_ahorros.
DISABLE TRIGGERS FOR LOAD OF bdcentral.mov_ahorros.

/*FOR EACH bd_web.mov_ahorros WHERE cod_operacion = 010101003
                              AND descrip <> "Abono Liq.Interés"
                              AND cpte <> 20 NO-LOCK:
    DISPLAY bd_web.mov_ahorros WITH WIDTH 300 1 COL.
END.*/

FOR EACH bd_web.mov_ahorros WHERE bd_web.mov_ahorros.cpte <> 20 BY fecha DESC:
    FIND FIRST bdcentral.mov_ahorros WHERE bdcentral.mov_ahorros.nit = bd_Web.mov_ahorros.nit
                                       AND bdcentral.mov_ahorros.cue_ahorros = bd_Web.mov_ahorros.cue_ahorros
                                       AND bdcentral.mov_ahorros.fecha = bd_Web.mov_ahorros.fecha
                                       AND bdcentral.mov_ahorros.cod_operacion = bd_Web.mov_ahorros.cod_operacion
                                       AND bdcentral.mov_ahorros.descrip = bd_Web.mov_ahorros.descrip
                                       AND bdcentral.mov_ahorros.cpte = bd_Web.mov_ahorros.cpte
                                       AND bdcentral.mov_ahorros.val_efectivo <> bd_Web.mov_ahorros.val_efectivo
                                       AND bdcentral.mov_ahorros.sdo_disponible = bd_Web.mov_ahorros.sdo_disponible
                                       AND bdcentral.mov_ahorros.hora = bd_Web.mov_ahorros.hora NO-ERROR.
    IF AVAILABLE bdcentral.mov_ahorros THEN DO:
        /*MESSAGE bd_web.mov_ahorros.fecha bd_web.mov_ahorros.hora bd_web.mov_ahorros.descrip bd_web.mov_ahorros.val_efectivo bd_web.mov_ahorros.sdo_disponible bd_web.mov_ahorros.cod_operacion SKIP
                bdcentral.mov_ahorros.fecha bdcentral.mov_ahorros.hora bdcentral.mov_ahorros.descrip bdcentral.mov_ahorros.val_efectivo bdcentral.mov_ahorros.sdo_disponible bdcentral.mov_ahorros.cod_operacion
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        
        bd_web.mov_ahorros.val_efectivo = bdcentral.mov_ahorros.val_efectivo.
    END.
END.

MESSAGE "Fin ajuste"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
