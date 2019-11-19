DISABLE TRIGGERS FOR LOAD OF creditos.

DEFINE BUFFER bfrcreditos FOR creditos.

FOR EACH bfrcreditos WHERE SUBSTRING(STRING(bfrcreditos.num_credito),1,3) = "990"
                       AND bfrcreditos.estado = 2 NO-LOCK:
    FIND FIRST creditos WHERE creditos.nit = bfrcreditos.nit
                          AND "990" + STRING(creditos.num_credito) = STRING(bfrcreditos.num_credito)
                          AND creditos.estado = 2 NO-ERROR.
    IF AVAILABLE creditos THEN
        DELETE creditos.
END.
