DEFINE VAR vFecha AS DATE.

DEFINE TEMP-TABLE integridad
    FIELD nit AS CHARACTER
    FIELD producto AS DECIMAL
    FIELD contable AS DECIMAL.

DO vFecha = 01/01/2019 TO 01/31/2019:
    EMPTY TEMP-TABLE integridad.

    FOR EACH mov_ahorros WHERE mov_ahorros.fecha = vFecha
                           AND mov_ahorros.cod_ahorro = 6
                           AND mov_ahorros.agencia = 1 NO-LOCK:
        FIND FIRST integridad WHERE integridad.nit = mov_ahorros.nit NO-ERROR.
        IF NOT AVAILABLE integridad THEN DO:
            CREATE integridad.
            integridad.nit = mov_ahorros.nit.
        END.

        IF SUBSTRING(STRING(mov_ahorros.cod_operacion),5,2) = "10" THEN
            integridad.producto = integridad.producto + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.

        IF SUBSTRING(STRING(mov_ahorros.cod_operacion),5,2) = "20" THEN
            integridad.producto = integridad.producto - mov_ahorros.val_efectivo - mov_ahorros.val_cheque.
    END.

    FOR EACH mov_contable WHERE mov_contable.agencia = 1
                            AND mov_contable.fec_contable = vFecha
                            AND mov_contable.cuenta = "21101001" NO-LOCK:
        FIND FIRST integridad WHERE integridad.nit = mov_contable.nit NO-ERROR.
        IF NOT AVAILABLE integridad THEN DO:
            CREATE integridad.
            integridad.nit = mov_contable.nit.
        END.

        integridad.contable = integridad.contable - mov_contable.db + mov_contable.cr.
    END.

    FOR EACH integridad NO-LOCK:
        IF integridad.producto <> integridad.contable THEN
            MESSAGE vfecha integridad.nit integridad.producto integridad.contable integridad.producto - integridad.contable
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
