DISABLE TRIGGERS FOR LOAD OF ahorros.
DISABLE TRIGGERS FOR LOAD OF mov_ahorros.

FOR EACH mov_contable WHERE mov_contable.agencia = 4
                        AND mov_contable.fec_contable = TODAY
                        AND mov_contable.comprobante = 20
                        AND mov_contable.num_documento = 6870:
    IF mov_contable.cuenta = "21050501" THEN DO:
        FIND FIRST ahorros WHERE ahorros.nit = mov_contable.nit
                             AND ahorros.cod_ahorro = 4
                             AND ahorros.estado = 1 NO-ERROR.
        IF NOT AVAILABLE ahorros THEN
            DISPLAY mov_contable WITH WIDTH 300 1 COL.
        ELSE
            ahorros.sdo_disponible = ahorros.sdo_disponible - mov_contable.cr.
                                 
        FIND FIRST mov_ahorros WHERE mov_ahorros.nit = mov_contable.nit
                                 AND mov_ahorros.cod_ahorro = 4
                                 AND mov_ahorros.fecha = TODAY
                                 AND mov_ahorros.num_documento = string(Mov_contable.num_documento)
                                 AND mov_ahorros.val_efectivo = mov_contable.cr NO-ERROR.
        IF NOT AVAILABLE mov_ahorros THEN
            DISPLAY mov_contable WITH WIDTH 300 1 COL.
        ELSE
            DELETE mov_ahorros.
    END.

    DELETE mov_contable.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
