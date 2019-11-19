DEFINE VAR cedula1 AS CHARACTER.
DEFINE VAR cedula2 AS CHARACTER.

DEFINE TEMP-TABLE buenas
    FIELD cedula AS CHARACTER.

INPUT FROM C:\INFO_Fodun\Leonardo\Nits.txt.

REPEAT:
    IMPORT cedula1 cedula2.
    
    FIND FIRST creditos WHERE creditos.nit = cedula1
                          AND creditos.estado = 2 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE creditos THEN DO:
        FIND FIRST ahorros WHERE ahorros.nit = cedula1
                             AND ahorros.estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ahorros THEN DO:
            CREATE buenas.
            buenas.cedula = cedula1.
        END.
    END.
END.

OUTPUT TO C:\INFO_Fodun\Leonardo\Nits1.txt.
FOR EACH buenas:
    DISPLAY cedula FORMAT "X(20)".
END.
