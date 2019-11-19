DEFINE VAR cont AS INTEGER.
DEFINE VAR sdoIni AS DECIMAL.
DEFINE VAR movs AS DECIMAL.

OUTPUT TO d:\Leonardo\cuentasSinHomologar.csv.
FOR EACH cuentas WHERE cuentas.estado = 1 AND cuentas.tipo = 2 AND cuentas.cuentaNIIF = "":
    FOR EACH agencias NO-LOCK:
        sdoIni = 0.
        movs = 0.

        FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = cuentas.cuenta
                               AND sal_cuenta.agencia = agencias.agencia
                               AND sal_cuenta.ano = 2015 NO-LOCK:
            sdoIni = sdoIni + sal_cuenta.sal_inicial.

            DO cont = 1 TO 12:
                movs = movs + sal_cuenta.db[cont] + sal_cuenta.cr[cont].
            END.
        END.

        IF sdoIni <> 0 OR movs <> 0 THEN
            EXPORT DELIMITER ";"
                STRING(agencias.agencia) + " - " + agencias.nombre
                cuentas.cuenta
                cuentas.nombre
                sdoIni
                movs.
    END.
END.
OUTPUT CLOSE.
