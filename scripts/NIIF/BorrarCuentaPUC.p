DEFINE VAR vCuenta AS CHARACTER INITIAL "61401021".
FIND FIRST mov_contable WHERE mov_contable.cuenta = vCuenta NO-LOCK NO-ERROR.
IF AVAILABLE mov_contable THEN DO:
    MESSAGE "Se encuentran movimientos contables"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-ERROR.
    IF AVAILABLE cuentas THEN
        cuentas.estado = 2.
END.
ELSE DO:
    FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        DELETE cuentas.

        MESSAGE "Borrada" vCuenta
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
