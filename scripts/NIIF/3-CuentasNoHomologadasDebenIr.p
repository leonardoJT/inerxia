DEFINE VAR saldo AS DECIMAL.
DEFINE VAR cont AS INTEGER.

FOR EACH agencias NO-LOCK BY agencias.agencia:
    OUTPUT TO VALUE ("d:\Leonardo\NIIF\ParaHomologar_" + string(agencias.agencia) + ".csv").
    FOR EACH cuentas WHERE cuentas.tipo = 2 AND cuentas.cuentaNIIF = "" NO-LOCK:
        saldo = 0.

        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                              AND sal_cuenta.cuenta = cuentas.cuenta
                              AND sal_cuenta.ano = 2015 NO-LOCK:
            saldo = saldo + sal_cuenta.sal_ini.

            DO cont = 1 TO 11:
                IF cuentas.naturaleza = "DB" THEN
                    saldo = saldo + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    saldo = saldo + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
            END.

            IF saldo = 0 THEN
                saldo = sal_cuenta.db[12] - sal_cuenta.cr[12].
        END.

        IF saldo <> 0 THEN
            EXPORT DELIMITER ";" cuentas.cuenta saldo.
    END.
    OUTPUT CLOSE.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
