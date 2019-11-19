FOR EACH habiles:
    DELETE habiles.
END.

DEFINE TEMP-TABLE thabiles
    FIELD agencia AS INTEGER
    FIELD cedula AS CHARACTER
    /*FIELD nombre AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER*/
    FIELD facultad AS CHARACTER.


INPUT FROM VALUE("C:\INFO_FODUN\Leonardo\HÁBILES definivitov.csv").
REPEAT:
    CREATE tHabiles.
    IMPORT DELIMITER ";" tHabiles.
    /*DISPLAY tHabiles WITH WIDTH 100.*/

    FIND FIRST clientes WHERE clientes.nit = tHabiles.cedula NO-LOCK NO-ERROR.
    IF NOT AVAILABLE clientes THEN
        MESSAGE tHabiles.cedula
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

FOR EACH tHabiles:
    FIND FIRST clientes WHERE clientes.nit = tHabiles.cedula NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        CREATE habiles.
        ASSIGN Habiles.agencia = thabiles.agencia
               Habiles.apellido1 = clientes.apellido1
               Habiles.apellido2 = clientes.apellido2
               Habiles.facultad = tHabiles.facultad
               Habiles.nit = tHabiles.cedula
               Habiles.nombre = clientes.nombre.
    END.
END.

