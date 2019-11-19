DEFINE VAR cont AS INTEGER.
DEFINE VAR sdoanexos AS DECIMAL.
DEFINE VAR sdoCuenta AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER INITIAL "25100251".
DEFINE VAR subs as INTEGER.
DEFINE VAR vYear AS INTEGER INITIAL 2014.

subs = LENGTH(vCuenta).

FOR EACH agencias NO-LOCK:
    sdoAnexos = 0.
    sdoCuenta = 0.

    FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,subs) = vCuenta
                      AND anexos.agencia = agencias.agencia
                      AND anexos.ano = vYear NO-LOCK:
        sdoanexos = sdoanexos + anexos.sdo_inicial.
        
        DO cont = 1 TO 12:
            sdoanexos = sdoanexos + anexos.db[cont] - anexos.cr[cont].
        END.
    END.

    FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,subs) = vCuenta
                          AND sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = vYear NO-LOCK:
        sdoCuenta = sdoCuenta + sal_cuenta.sal_inicial.

        DO cont = 1 TO 12:
            sdoCuenta = sdoCuenta + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        END.
    END.

    MESSAGE agencias.agencia sdoCuenta sdoanexos sdoCuenta - sdoanexos
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
