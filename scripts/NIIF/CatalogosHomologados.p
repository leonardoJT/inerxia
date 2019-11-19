DEFINE TEMP-TABLE tt
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreCuentaNIIF AS CHARACTER
    FIELD cuentaSES AS CHARACTER
    FIELD nombreCuentaSES AS CHARACTER.

DEFINE VAR cuentaSES AS CHARACTER.
DEFINE VAR nombreCuentaSES AS CHARACTER.
    
FOR EACH cuentas WHERE cuentas.tipo = 2 AND cuentas.cuentaNIIF <> "" NO-LOCK BY cuentas.cuenta:
    CREATE tt.
    tt.cuentaNIIF = cuentas.cuentaNIIF.
    tt.cuentaSES = cuentas.cuenta.
    tt.nombreCuentaSES = cuentas.nombre.
END.

FOR EACH cuentas_NIIF NO-LOCK:
    FIND FIRST tt WHERE tt.cuentaNIIF = cuentas_NIIF.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE tt THEN DO:
        FOR EACH tt WHERE tt.cuentaNIIF = cuentas_NIIF.cuenta:
            tt.nombreCuentaNIIF = cuentas_NIIF.nombre.
        END.
    END.
    ELSE DO:
        CREATE tt.
        tt.cuentaNIIF = cuentas_NIIF.cuenta.
        tt.nombreCuentaNIIF = cuentas_NIIF.nombre.
    END.
END.

OUTPUT TO d:\Leonardo\CatalogoNIIF_vs_SES.csv.
EXPORT DELIMITER ";"
    "CuentaNIIF"
    "NombreCuentaNIIF"
    "CuentaSES"
    "NombreCuentaSES".

FOR EACH tt NO-LOCK BY tt.cuentaNIIF:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
