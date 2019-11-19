DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD cuenta AS CHARACTER
    FIELD cuota AS DECIMAL.

INPUT FROM d:\leonardo\cuotas.csv.
REPEAT :
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = tt.nit
                         AND ahorros.cue_ahorros = tt.cuenta
                         AND ahorros.estado = 1 NO-ERROR.
    IF AVAILABLE ahorros THEN
        ahorros.cuota = tt.cuota.
END.
