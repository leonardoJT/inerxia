DEFINE VAR newnumber AS CHARACTER.

DEFINE TEMP-TABLE ttcr
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER.

INPUT FROM d:\Leonardo\creditos.csv.
REPEAT :
    CREATE ttcr.
    IMPORT DELIMITER ";" ttcr.
END.
INPUT CLOSE.

OUTPUT TO d:\Leonardo\Renumerar.csv.
FOR EACH ttcr NO-LOCK:
    FIND FIRST creditos WHERE creditos.nit = ttcr.nit
                          AND creditos.num_credito = ttcr.num_credito
                          AND creditos.cod_credito = 123 NO-LOCK NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        IF SUBSTRING(STRING(creditos.num_credito),1,4) = "6200" THEN
            newNumber = "990" + SUBSTRING(string(creditos.num_credito),5).
        ELSE
            newNumber = "990" + STRING(creditos.num_credito).
        
        EXPORT DELIMITER ";" ttcr.nit ttcr.num_credito newNumber.
    END.
END.
OUTPUT CLOSE.
