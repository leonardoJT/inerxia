/*FOR EACH mov_ahorros WHERE nit = "14200258" AND fecha = 12/24/2014 AND cod_ahorro = 4 NO-LOCK:
    DISPLAY cod_operacion descrip val_efectivo hora STRING(hora,"HH:MM:SS") .
END.*/

FOR EACH mov_contable WHERE mov_contable.fec_contable = 12/24/2014
                        AND mov_contable.comprobante = 20
                        AND mov_contable.agencia = 4
                        AND mov_contable.num_documento = 2976
                        AND mov_contable.cuenta = "24050501" NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = mov_contable.nit
                         AND ahorros.cod_ahorro = 4
                         AND ahorros.estado = 1
                         AND ahorros.INT_pagar >= mov_contable.cr NO-ERROR.
    IF NOT AVAILABLE ahorros THEN
        FIND FIRST ahorros WHERE ahorros.nit = mov_contable.nit
                             AND ahorros.cod_ahorro = 4
                             AND ahorros.INT_pagar >= mov_contable.cr NO-ERROR.
    
    IF NOT AVAILABLE ahorros THEN
        DISPLAY mov_contable.nit mov_contable.cr.

    FOR EACH mov_ahorros WHERE mov_ahorros.fecha = 12/24/2014
                           AND mov_ahorros.nit = mov_contable.nit
                           AND mov_ahorros.cod_ahorro = 4
                           AND (mov_ahorros.cod_operacion = 010101003 OR mov_ahorros.cod_operacion = 010102001)
                           AND mov_ahorros.hora < 39600:
        DELETE mov_ahorros.
    END.

    ahorros.INT_pagar = ahorros.INT_pagar - mov_contable.cr.
END.

MESSAGE "Ok"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
