DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE VAR cuentaPUC AS CHARACTER.
DEFINE VAR vCuentaNIIF AS CHARACTER.
DEFINE TEMP-TABLE tt LIKE cuentas.
DEFINE VAR nombreCuentaNIIF AS CHARACTER.

DEFINE TEMP-TABLE niif
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD cuentaNiif AS CHARACTER
    FIELD nombreNiif AS CHARACTER.

INPUT FROM d:\Leonardo\niif.csv.
REPEAT:
    CREATE niif.
    IMPORT DELIMITER ";" niif.
END.
INPUT CLOSE.

FOR EACH niif WHERE niif.cuenta <> "":
    FIND FIRST cuentas WHERE cuentas.cuenta = niif.cuenta NO-ERROR.
    
    cuentas.cuentaNIIF = niif.cuentaNiif.

    FIND FIRST cuentas_NIIF WHERE cuentas_NIIF.cuenta = niif.cuentaNiif NO-ERROR.
    IF NOT AVAILABLE cuentas_NIIF THEN DO:
        CREATE tt.
        BUFFER-COPY cuentas TO tt.
        tt.cuenta = niif.cuentaNiif.
        tt.nombre = niif.nombreNiif.

        CREATE cuentas_NIIF.
        BUFFER-COPY tt TO cuentas_niif.
    END.
    ELSE DO:
        MESSAGE "La cuenta ya existe"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    FOR EACH mov_contable WHERE mov_contable.cuenta = niif.cuenta
                            AND mov_contable.fec_contable >= 01/01/2014:
        mov_contable.cuentaNIIF = niif.cuentaNiif.
    END.

    FOR EACH sal_cuenta WHERE sal_cuenta.cuenta = niif.cuenta
                          AND sal_cuenta.ano >= 2014:
        sal_cuenta.cuentaNIIF = niif.cuentaNiif.
    END.

    FOR EACH anexos WHERE anexos.cuenta = niif.cuenta
                      AND anexos.ano >= 2014:
        anexos.cuentaNIIF = niif.cuentaNiif.
    END.

    FOR EACH sal_cuenta13 WHERE sal_cuenta13.cuenta = niif.cuenta
                            AND sal_cuenta13.ano >= 2014:
        sal_cuenta13.cuentaNIIF = niif.cuentaNiif.
    END.

    FOR EACH anexos13 WHERE anexos13.cuenta = niif.cuenta
                        AND anexos13.ano >= 2014:
        anexos13.cuentaNIIF = niif.cuentaNiif.
    END.
END.

MESSAGE "OK"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
