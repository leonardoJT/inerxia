FOR EACH sal_cuenta_niif:
    FIND FIRST cuentas_niif WHERE cuentas_niif.cuenta = sal_cuenta_niif.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas_niif THEN DO:
        /*DISPLAY sal_cuenta_niif WITH 1 COL.*/
        DELETE sal_cuenta_niif.
    END.
END.
