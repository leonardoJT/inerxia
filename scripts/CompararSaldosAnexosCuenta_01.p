DEFINE VAR saldoBalance AS DECIMAL.
DEFINE VAR saldoAnexos AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER INITIAL "1625".
DEFINE VAR cont AS INTEGER.

FOR EACH sal_cuenta13 WHERE SUBSTRING(sal_cuenta13.cuenta,1,LENGTH(vCuenta)) = vCuenta
                        AND sal_cuenta13.ano = 2011 NO-LOCK:
    saldoBalance = saldoBalance + sal_cuenta13.sal_inicial.

    DO cont = 1 TO 12:
        saldoBalance = saldoBalance + sal_cuenta13.db[cont] - sal_cuenta13.cr[cont].
    END.
END.

FOR EACH anexos13 WHERE SUBSTRING(anexos13.cuenta,1,LENGTH(vCuenta)) = vCuenta
                    AND anexos13.ano = 2011 NO-LOCK:
    saldoAnexos = saldoAnexos + anexos13.sdo_inicial.

    DO cont = 1 TO 12:
        saldoAnexos = saldoAnexos + anexos13.db[cont] - anexos13.cr[cont].
    END.
END.


MESSAGE saldoBalance saldoAnexos saldoBalance - saldoAnexos
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
