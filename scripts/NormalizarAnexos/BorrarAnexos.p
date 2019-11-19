/*FOR EACH anexos WHERE cuenta = "16965301":
    UPDATE anexos WITH 1 COL.
END.*/

DEFINE VAR vCuenta AS CHARACTER INITIAL "16965602".
    
FOR EACH anexos WHERE anexos.cuenta = vCuenta:
    DELETE anexos.
END.

FOR EACH anexos13 WHERE anexos13.cuenta = vCuenta:
    DELETE anexos13.
END.
