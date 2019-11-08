DEFIN TEMP-TABLE homol 
    FIELD cta LIKE cuentas.cuenta
    FIELD homo LIKE cuentas.cuenta.
INPUT FROM c:\INFO_juriscoop\sinhomologar.csv.
REPEAT :
    CREATE homol.
    IMPORT DELIMITER ";" homol.
END.

FOR EACH homol:
    FIND FIRST cuentas WHERE cuentas.cuenta = homol.cta NO-ERROR.
    IF AVAILABLE(cuentas) THEN DO:
    DISP cuentas.cuenta homol.cta cuentas.cta_homologada homo .
        ASSIGN cuentas.cta_homologada = homo.
    END.
END.
