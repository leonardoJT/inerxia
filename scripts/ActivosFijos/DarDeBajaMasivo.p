DEFINE TEMP-TABLE tt
    FIELD id AS CHARACTER.

INPUT FROM d:\Leonardo\activosFijos_2.txt.
REPEAT :
    CREATE tt.
    IMPORT tt.id.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST activosFijos WHERE activosFijos.id = tt.id AND estado = 1 NO-ERROR.
    IF AVAILABLE activosFijos THEN DO:
        activosFijos.estado = 2.
        activosFijos.anotac = "21/11/2018 - Dado de baja por decisión administrativa " + activosFijos.anotac.
    END.
END.

/*FOR EACH tt NO-LOCK:
    FIND FIRST activosFijos WHERE activosFijos.id = tt.id AND estado = 2 NO-ERROR.
    IF AVAILABLE activosFijos THEN DO:
        activosFijos.estado = 1.

        IF INDEX(activosFijos.anotac,"/10/2018 - Dado de baja por decisión administrativa ") > 0 THEN
            activosFijos.anotac = SUBSTRING(activosFijos.anotac,55).
    END.
END.*/
