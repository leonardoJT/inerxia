DEFINE VAR saldoCuenta AS DECIMAL.
DEFINE VAR saldoAnexos AS DECIMAL.
DEFINE VAR cont AS INTEGER.

OUTPUT TO C:\INFO_Fodun\Leonardo\Anexos.csv.

EXPORT DELIMITER ";" "Agencia" "Cuenta" "Cen_costos" "Balance" "Anexos" "Diferencia".

FOR EACH agencias NO-LOCK:
    FOR EACH cuentas NO-LOCK:
        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                              AND sal_cuenta.cuenta = cuentas.cuenta
                              AND sal_cuenta.ano = 2011 NO-LOCK:
            saldoCuenta = 0.
            saldoAnexos = 0.

            DO cont = 1 TO 12:
                IF cuentas.naturaleza = "DB" THEN
                    saldoCuenta = saldoCuenta + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    saldoCuenta = saldoCuenta - sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.

            FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                              AND anexos.cuenta = cuentas.cuenta
                              AND anexos.ano = 2011
                              AND anexos.cen_costos = sal_cuenta.cen_costos NO-LOCK:
                DO cont = 1 TO 12:
                    IF cuentas.naturaleza = "DB" THEN
                        saldoAnexos = saldoAnexos + anexos.db[cont] - anexos.cr[cont].
                    ELSE
                        saldoAnexos = saldoAnexos - anexos.db[cont] + anexos.cr[cont].
                END.
            END.

            IF saldoCuenta = saldoAnexos THEN
                EXPORT DELIMITER ";" sal_cuenta.agencia
                                     sal_cuenta.cuenta
                                     sal_cuenta.cen_costos
                                     saldoCuenta
                                     saldoAnexos
                                     saldoCuenta - saldoAnexos.
        END.
    END.
END.
