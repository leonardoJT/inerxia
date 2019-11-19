DEFINE TEMP-TABLE nits
    FIELD nit1 AS CHARACTER
    FIELD estado1 AS INTEGER
    FIELD nit2 AS CHARACTER
    FIELD estado2 AS INTEGER.

INPUT FROM d:\Leonardo\nitsRepetidos.csv.
REPEAT :
    CREATE nits.
    IMPORT DELIMITER ";" nits.
END.
INPUT CLOSE.

FOR EACH nits NO-LOCK:
    /* 1. Buscamos 
    FIND FIRST ahorros WHERE ahorros.nit = nits.nit
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN
    FIND FIRST mov_contable WHERE mov_contable.fec_contable >= 01/01/2012
                              AND mov_contable.nit = nits.nit NO-LOCK NO-ERROR.
    IF AVAILABLE mov_contable THEN
        DISPLAY mov_contable WITH WIDTH 300 1 COL.
END.
