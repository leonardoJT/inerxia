DEFINE VAR acum AS DECIMAL.

FOR EACH mov_contable WHERE mov_contable.agencia = 1
                        AND comprobante <> 20
                        AND comprobante <> 22
                        AND mov_contable.cuenta = "21050501"
                        /*AND mov_contable.cr <= 2392
                        AND mov_contable.cr > 0*/
                        AND mov_contable.fec_contable >= 09/21/2015
                        AND mov_contable.fec_contable <= 09/30/2015 NO-LOCK BY mov_contable.fec_contable DESC:
    FIND FIRST mov_ahorros WHERE mov_ahorros.agencia = mov_contable.agencia
                             AND mov_ahorros.cod_ahorro = 4
                             AND mov_ahorros.fecha = mov_contable.fec_contable
                             AND mov_ahorros.val_efectivo + mov_ahorros.val_cheque = mov_contable.cr + mov_contable.db
                             AND integer(mov_ahorros.num_documento) = mov_contable.num_documento NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mov_ahorros THEN DO:
        DISPLAY mov_contable WITH WIDTH 300 1 COL.
    END.
END.


MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
