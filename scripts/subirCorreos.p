DISABLE TRIGGERS FOR LOAD OF clientes.

DEFINE TEMP-TABLE archivo
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD email AS CHARACTER.

INPUT FROM value("c:\INFO_fodun\leonardo\correosMedellin.csv").
REPEAT :
    CREATE archivo.
    IMPORT DELIMITER ";" archivo.
END.
INPUT CLOSE.

FOR EACH archivo NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = archivo.nit NO-ERROR.
    IF NOT AVAILABLE clientes THEN DO:
        MESSAGE archivo.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        NEXT.
    END.

    clientes.email = archivo.email.
END.
