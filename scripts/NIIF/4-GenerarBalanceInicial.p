DISABLE TRIGGERS FOR LOAD OF sal_cuenta_NIIF.
DISABLE TRIGGERS FOR LOAD OF anexos_NIIF.

DEFINE VAR saldo AS DECIMAL.
DEFINE VAR cont AS INTEGER.

/*FOR EACH agencias NO-LOCK BY agencias.agencia:
    FOR EACH sal_cuenta_NIIF WHERE sal_cuenta_NIIF.agencia = agencias.agencia
                               AND sal_cuenta_NIIF.ano = 2015:
        DELETE sal_cuenta_niif.
    END.

    FOR EACH cuentas WHERE cuentas.tipo = 2 AND cuentas.cuentaNIIF <> "" NO-LOCK:
        FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                              AND sal_cuenta.cuenta = cuentas.cuenta
                              AND sal_cuenta.ano = 2015 NO-LOCK:
            CREATE sal_cuenta_NIIF.
            sal_cuenta_NIIF.agencia = sal_cuenta.agencia.
            sal_cuenta_NIIF.cuenta = cuentas.cuentaNIIF.
            sal_cuenta_NIIF.cen_costos = sal_cuenta.cen_costos.
            sal_cuenta_NIIF.ano = sal_cuenta.ano.

            saldo = sal_cuenta.sal_ini.

            DO cont = 1 TO 11:
                IF cuentas.naturaleza = "DB" THEN
                    saldo = saldo + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
                ELSE
                    saldo = saldo + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
            END.

            sal_cuenta_NIIF.sal_inicial = saldo.
            sal_cuenta_NIIF.db[12] = sal_cuenta.db[12].
            sal_cuenta_NIIF.cr[12] = sal_cuenta.cr[12].
        END.
    END.
END.*/

FOR EACH agencias NO-LOCK BY agencias.agencia:
    FOR EACH anexos_NIIF WHERE anexos_NIIF.agencia = agencias.agencia
                           AND anexos_NIIF.ano = 2015:
        DELETE anexos_niif.
    END.

    FOR EACH cuentas WHERE cuentas.tipo = 2 AND cuentas.cuentaNIIF <> "" NO-LOCK:
        FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                          AND anexos.cuenta = cuentas.cuenta
                          AND anexos.ano = 2015 NO-LOCK:
            CREATE anexos_NIIF.
            anexos_NIIF.agencia = anexos.agencia.
            anexos_NIIF.cuenta = cuentas.cuentaNIIF.
            anexos_NIIF.cen_costos = anexos.cen_costos.
            anexos_NIIF.ano = anexos.ano.
            anexos_NIIF.nit = anexos.nit.

            saldo = anexos.sdo_ini.

            DO cont = 1 TO 11:
                IF cuentas.naturaleza = "DB" THEN
                    saldo = saldo + anexos.db[cont] - anexos.cr[cont].
                ELSE
                    saldo = saldo + anexos.cr[cont] - anexos.db[cont].
            END.

            anexos_NIIF.sdo_inicial = saldo.
            anexos_NIIF.db[12] = anexos.db[12].
            anexos_NIIF.cr[12] = anexos.cr[12].
        END.
    END.
END.


MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
