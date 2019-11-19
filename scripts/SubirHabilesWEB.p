DEFINE TEMP-TABLE ttHabiles
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD facultad AS CHARACTER.

FOR EACH bd_web.habiles:
    DELETE bd_web.habiles.
END.

INPUT FROM c:\INFO_fodun\Leonardo\habiles.csv.
REPEAT:
    CREATE ttHabiles.
    IMPORT DELIMITER ";" ttHabiles NO-ERROR.
END.

FOR EACH ttHabiles WHERE ttHabiles.agencia > 0 NO-LOCK:
    CREATE bd_web.habiles.
    BUFFER-COPY ttHabiles TO bd_web.habiles.

    FIND FIRST clientes WHERE clientes.nit = habiles.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        habiles.nombre = clientes.nombre.
        habiles.apellido1 = clientes.apellido1.
        habiles.apellido2 = clientes.apellido2.

        FIND FIRST facultades WHERE facultades.codigo = STRING(clientes.departamento,"99") + STRING(clientes.facultad,"999") NO-LOCK NO-ERROR.
        IF AVAILABLE facultades THEN
            habiles.facultad = facultades.nombre.
    END.
END.
