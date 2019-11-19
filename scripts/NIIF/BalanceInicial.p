DEFINE TEMP-TABLE tt LIKE sal_cuenta_NIIF.
DEFINE VAR sdo AS DECIMAL.
DEFINE VAR cont AS INTEGER.

FOR EACH sal_cuenta WHERE sal_cuenta.agencia = 1
                      AND sal_cuenta.ano = 2015 NO-LOCK:
    CREATE tt.
    tt.agencia = sal_cuenta.agencia.
    tt.cuenta = sal_cuenta.cuenta.
    tt.ano = sal_cuenta.ano.
    tt.cen_costos = sal_cuenta.cen_costos.
    tt.db[12] = sal_cuenta.db[12].
    tt.cr[12] = sal_cuenta.cr[12].

    FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas THEN DO:
        sdo = sal_cuenta.sal_ini.

        DO cont = 1 TO 11:
            IF cuentas.naturaleza = "DB" THEN
                sdo = sdo + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
            ELSE
                sdo = sdo + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
        END.

        tt.sal_ini = sdo.
    END.
END.

OUTPUT TO d:\Leonardo\NIIF\balanceInicial.csv.
FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";"
        tt.agencia
        tt.cuenta
        tt.cen_costos
        tt.sal_ini
        tt.db[12]
        tt.cr[12].
END.
OUTPUT CLOSE.
