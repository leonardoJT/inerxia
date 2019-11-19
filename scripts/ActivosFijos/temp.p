DEFINE VAR vDepreciacion AS DECIMAL.
DEFINE VAR vMeses AS INTEGER.

DEFINE TEMP-TABLE tt
    FIELD id AS CHARACTER.

INPUT FROM d:\Leonardo\activosFijos_unir.txt.
REPEAT :
    CREATE tt.
    IMPORT tt.id.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST activosFijos WHERE activosFijos.id = tt.id AND estado = 1 NO-ERROR.
    IF AVAILABLE activosFijos THEN DO:
        UPDATE activosFijos WITH WIDTH 300 1 COL.
    END.
END.


MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
