DISABLE TRIGGERS FOR LOAD OF clientes.

DEFINE TEMP-TABLE generos
    FIELD nit AS CHARACTER
    FIELD genero AS CHARACTER.

INPUT FROM d:\Leonardo\generos.csv.
REPEAT :
    CREATE generos.
    IMPORT DELIMITER ";" generos.
END.

FOR EACH generos NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = generos.nit NO-ERROR.
    IF NOT AVAILABLE clientes THEN
        MESSAGE generos.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        IF generos.genero = "M" THEN
            clientes.sexo = 1.
        ELSE
            clientes.sexo = 2.
    END.
END.
