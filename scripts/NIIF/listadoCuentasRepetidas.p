DEFINE BUFFER bfrcuentas FOR cuentas.

DEFINE TEMP-TABLE tt
    FIELD cuentaSES AS CHARACTER
    FIELD nombreSES AS CHARACTER
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreNIIF AS CHARACTER.

FOR EACH cuentas WHERE cuentas.cuentaniif <> "" NO-LOCK:
    FIND FIRST bfrcuentas WHERE bfrCuentas.cuenta <> cuentas.cuenta
                            AND bfrCuentas.cuentaniif = cuentas.cuentaniif NO-LOCK NO-ERROR.
    IF AVAILABLE bfrCuentas THEN DO:
        CREATE tt.
        tt.cuentaSES = cuentas.cuenta.
        tt.nombreSES = cuentas.nombre.
        tt.cuentaNIIF = cuentas.cuentaniif.

        FIND FIRST cuentas_NIIF WHERE cuentas_NIIF.cuenta = cuentas.cuentaNiif NO-LOCK NO-ERROR.

        tt.nombreNIIF = cuentas_NIIF.nombre.
    END.
END.

OUTPUT TO d:\leonardo\cuentasNIIFrepetidas.csv.
FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.
