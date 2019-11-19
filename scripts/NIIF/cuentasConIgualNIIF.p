DEFINE VAR cont AS INTEGER.

DEFINE TEMP-TABLE niif
    FIELD cuenta AS CHARACTER.

DEFINE TEMP-TABLE inc
    FIELD ses AS CHARACTER
    FIELD niif AS CHARACTER.

FOR EACH cuentas WHERE cuentas.tipo = 2
                   AND cuentas.cuentaNiif <> "" NO-LOCK BREAK BY cuentas.cuentaNiif:
    IF FIRST-OF(cuentas.cuentaNiif) THEN
        cont = 1.
    ELSE
        cont = cont + 1.

    IF LAST-OF(cuentas.cuentaNiif) AND cont > 1 THEN DO:
        CREATE niif.
        niif.cuenta = cuentas.cuentaNiif.
    END.
END.

OUTPUT TO d:\Leonardo\cuentasNIIF.csv.
FOR EACH niif NO-LOCK:
    FOR EACH cuentas WHERE cuentas.cuentaNiif = niif.cuenta NO-LOCK:
        EXPORT DELIMITER ";" cuentas.cuenta cuentas.cuentaNIIF.
    END.
END.
OUTPUT CLOSE.
