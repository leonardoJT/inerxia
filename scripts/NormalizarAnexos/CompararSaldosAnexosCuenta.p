DEFINE VAR cont AS INTEGER.
DEFINE VAR sdoanexos AS DECIMAL.
DEFINE VAR sdoCuenta AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER INITIAL "16989501".
DEFINE VAR subs as INTEGER.

subs = LENGTH(vCuenta).

FOR EACH agencias /*WHERE agencias.agencia = 2*/ NO-LOCK:
    sdoAnexos = 0.
    sdoCuenta = 0.

    FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,subs) = vCuenta
                      AND anexos.agencia = agencias.agencia
                      AND anexos.ano = 2015 NO-LOCK:
        sdoanexos = sdoanexos + anexos.sdo_inicial.

        FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

        DO cont = 1 TO 12:
            IF cuentas.naturaleza = "DB" THEN
                sdoanexos = sdoanexos + anexos.db[cont] - anexos.cr[cont].
            ELSE
                sdoanexos = sdoanexos - anexos.db[cont] + anexos.cr[cont].
        END.
    END.

    FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,subs) = vCuenta
                          AND sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = 2015 NO-LOCK:
        sdoCuenta = sdoCuenta + sal_cuenta.sal_inicial.

        FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.

        DO cont = 1 TO 12:
            IF cuentas.naturaleza = "DB" THEN
                sdoCuenta = sdoCuenta + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
            ELSE
                sdoCuenta = sdoCuenta - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
        END.
    END.

    MESSAGE agencias.agencia sdoCuenta sdoanexos sdoCuenta - sdoanexos
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
