DEFINE VAR saldoAhorros AS DECIMAL.
DEFINE VAR ag AS INTEGER.
DEFINE VAR saldoCuenta AS DECIMAL.
DEFINE VAR codAhorro AS INTEGER.
DEFINE VAR pCuenta AS CHARACTER.

saldoAhorros = 0.
saldoCuenta = 0.

/*codAhorro = 2.
pCuenta = "31050501".

FOR EACH agencias:
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND (ahorros.cod_ahorro = codAhorro OR ahorros.cod_ahorro = codAhorro)
                       AND ahorros.sdo_disponible <> 0 NO-LOCK:
        saldoAhorros = saldoAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2012
                          AND sal_cuenta.cuenta = pCuenta NO-LOCK:
        saldoCuenta = saldoCuenta + sal_cuenta.sal_inicial + (sal_cuenta.cr[1] + sal_cuenta.cr[2] + sal_cuenta.cr[3]) - (sal_cuenta.db[1] + sal_cuenta.db[2] + sal_cuenta.db[3]).
    END.

    MESSAGE agencias.agencia pCuenta saldoAhorros - saldoCuenta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    saldoAhorros = 0.
    saldoCuenta = 0.
END.

codAhorro = 3.
pCuenta = "21301001".

FOR EACH agencias:
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND (ahorros.cod_ahorro = codAhorro OR ahorros.cod_ahorro = 7)
                       AND ahorros.sdo_disponible <> 0 NO-LOCK:
        saldoAhorros = saldoAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2012
                          AND sal_cuenta.cuenta = pCuenta NO-LOCK:
        saldoCuenta = saldoCuenta + sal_cuenta.sal_inicial + (sal_cuenta.cr[1] + sal_cuenta.cr[2] + sal_cuenta.cr[3]) - (sal_cuenta.db[1] + sal_cuenta.db[2] + sal_cuenta.db[3]).
    END.

    MESSAGE agencias.agencia pCuenta saldoAhorros - saldoCuenta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    saldoAhorros = 0.
    saldoCuenta = 0.
END.

codAhorro = 4.
pCuenta = "21050501".

FOR EACH agencias:
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND (ahorros.cod_ahorro = codAhorro OR ahorros.cod_ahorro = codAhorro)
                       AND ahorros.sdo_disponible <> 0 NO-LOCK:
        saldoAhorros = saldoAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2012
                          AND sal_cuenta.cuenta = pCuenta NO-LOCK:
        saldoCuenta = saldoCuenta + sal_cuenta.sal_inicial + (sal_cuenta.cr[1] + sal_cuenta.cr[2] + sal_cuenta.cr[3]) - (sal_cuenta.db[1] + sal_cuenta.db[2] + sal_cuenta.db[3]).
    END.

    MESSAGE agencias.agencia pCuenta saldoAhorros - saldoCuenta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    saldoAhorros = 0.
    saldoCuenta = 0.
END.

codAhorro = 5.
pCuenta = "21100501".

FOR EACH agencias:
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND (ahorros.cod_ahorro = codAhorro OR ahorros.cod_ahorro = codAhorro)
                       AND ahorros.sdo_disponible <> 0 NO-LOCK:
        saldoAhorros = saldoAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2012
                          AND sal_cuenta.cuenta = pCuenta NO-LOCK:
        saldoCuenta = saldoCuenta + sal_cuenta.sal_inicial + (sal_cuenta.cr[1] + sal_cuenta.cr[2] + sal_cuenta.cr[3]) - (sal_cuenta.db[1] + sal_cuenta.db[2] + sal_cuenta.db[3]).
    END.

    MESSAGE agencias.agencia pCuenta saldoAhorros - saldoCuenta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    saldoAhorros = 0.
    saldoCuenta = 0.
END.

codAhorro = 6.
pCuenta = "21101001".

FOR EACH agencias:
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND (ahorros.cod_ahorro = codAhorro OR ahorros.cod_ahorro = codAhorro)
                       AND ahorros.sdo_disponible <> 0 NO-LOCK:
        saldoAhorros = saldoAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2012
                          AND sal_cuenta.cuenta = pCuenta NO-LOCK:
        saldoCuenta = saldoCuenta + sal_cuenta.sal_inicial + (sal_cuenta.cr[1] + sal_cuenta.cr[2] + sal_cuenta.cr[3]) - (sal_cuenta.db[1] + sal_cuenta.db[2] + sal_cuenta.db[3]).
    END.

    MESSAGE agencias.agencia pCuenta saldoAhorros - saldoCuenta saldoCuenta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    saldoAhorros = 0.
    saldoCuenta = 0.
END.*/

codAhorro = 8.
pCuenta = "24953001".

FOR EACH agencias:
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND (ahorros.cod_ahorro = codAhorro OR ahorros.cod_ahorro = codAhorro)
                       AND ahorros.sdo_disponible <> 0 NO-LOCK:
        saldoAhorros = saldoAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2012
                          AND sal_cuenta.cuenta = pCuenta NO-LOCK:
        saldoCuenta = saldoCuenta + sal_cuenta.sal_inicial + (sal_cuenta.cr[1] + sal_cuenta.cr[2] + sal_cuenta.cr[3]) - (sal_cuenta.db[1] + sal_cuenta.db[2] + sal_cuenta.db[3]).
    END.

    MESSAGE agencias.agencia pCuenta saldoAhorros - saldoCuenta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    saldoAhorros = 0.
    saldoCuenta = 0.
END.

codAhorro = 9.
pCuenta = "21050502".

FOR EACH agencias:
    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND (ahorros.cod_ahorro = codAhorro OR ahorros.cod_ahorro = codAhorro)
                       AND ahorros.sdo_disponible <> 0 NO-LOCK:
        saldoAhorros = saldoAhorros + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2012
                          AND sal_cuenta.cuenta = pCuenta NO-LOCK:
        saldoCuenta = saldoCuenta + sal_cuenta.sal_inicial + (sal_cuenta.cr[1] + sal_cuenta.cr[2] + sal_cuenta.cr[3]) - (sal_cuenta.db[1] + sal_cuenta.db[2] + sal_cuenta.db[3]).
    END.

    MESSAGE agencias.agencia pCuenta saldoAhorros - saldoCuenta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    saldoAhorros = 0.
    saldoCuenta = 0.
END.
