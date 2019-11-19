OUTPUT TO d:\Leonardo\PUC_SS_NIIF.csv.
EXPORT DELIMITER ";"
    "CUENTA_PUC"
    "NOMBRE_CUENTA_PUC"
    "CUENTA_NIIF"
    "NOMBRE_CUENTA_NIIF".

DEFINE TEMP-TABLE homologa
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD cuenta_NIIF AS CHARACTER
    FIELD nombre_NIIF AS CHARACTER.

FOR EACH cuentas WHERE cuentas.tipo = 2 NO-LOCK BY cuentas.cuenta:
    CREATE homologa.
    homologa.cuenta = cuentas.cuenta.
    homologa.nombre = cuentas.nombre.
    homologa.cuenta_NIIF = cuentas.cuentaNIIF.

    FIND FIRST cuentas_NIIF WHERE cuentas_NIIF.cuenta = cuentas.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE cuentas_NIIF THEN
        homologa.nombre_NIIF = cuentas_NIIF.nombre.
END.

FOR EACH homologa NO-LOCK:
    EXPORT DELIMITER ";" homologa.
END.
OUTPUT CLOSE.
