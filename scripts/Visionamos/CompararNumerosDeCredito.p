DEFINE TEMP-TABLE ttcr
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER
    FIELD fodun AS INTEGER.

INPUT FROM d:\leonardo\creditos.csv.
REPEAT :
    CREATE ttcr.
    IMPORT DELIMITER ";" ttcr.
END.
INPUT CLOSE.

FOR EACH ttcr:
    FIND FIRST creditos WHERE creditos.nit = ttcr.nit
                          AND creditos.num_credito = ttcr.num_credito
                          AND creditos.estado = 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE creditos THEN DO:
        FIND FIRST creditos WHERE creditos.nit = ttcr.nit
                              AND creditos.cod_credito = 123
                              AND creditos.estado = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN
            ttcr.fodun = creditos.num_credito.
    END.
    ELSE
        DELETE ttcr.
END.

OUTPUT TO d:\Leonardo\CreditosFodunVisionamos.csv.
FOR EACH ttcr NO-LOCK:
    EXPORT DELIMITER ";" ttcr.
END.
OUTPUT CLOSE.
