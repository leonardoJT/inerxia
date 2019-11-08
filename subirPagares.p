DISABLE TRIGGERS FOR LOAD OF clientes.

DEFINE TEMP-TABLE pagares
    FIELD nit AS CHARACTER
    FIELD fecha AS DATE.

INPUT FROM d:\Leonardo\pagares.csv.
REPEAT :
    CREATE pagares.
    IMPORT DELIMITER ";" pagares.
END.

FOR EACH pagares NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = pagares.nit NO-ERROR.
    IF NOT AVAILABLE clientes THEN
        MESSAGE pagares.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        clientes.fecPagare = pagares.fecha.
    END.
END.
