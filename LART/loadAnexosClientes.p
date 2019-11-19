
DEFINE VARIABLE vi AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE tc
    FIELDS nit          LIKE anexos_clientes.nit
    FIELDS apellido1    LIKE clientes.apellido1
    FIELDS apellido2    LIKE clientes.apellido2
    FIELDS nombre1      LIKE anexos_clientes.nombre1
    FIELDS nombre2      LIKE anexos_clientes.nombre2.


EMPTY TEMP-TABLE tc.

INPUT FROM "c:\info_fodun\anexosClientes.csv".
REPEAT:
    CREATE tc.
    IMPORT DELIMITER ";" tc.
END.
INPUT CLOSE.


FOR EACH tc WHERE nit NE "" NO-LOCK:
    FIND FIRST anexos_clientes WHERE anexos_clientes.nit EQ tc.nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE anexos_clientes THEN DO:
        ASSIGN vi = vi + 1.
        CREATE anexos_clientes.
        BUFFER-COPY tc TO anexos_clientes.
    END.
END.

MESSAGE "cargados " vi
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


