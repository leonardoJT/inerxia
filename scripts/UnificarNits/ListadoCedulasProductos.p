DEFINE TEMP-TABLE cedulas
    FIELD cedula AS CHARACTER.

FOR EACH ahorros WHERE ahorros.estado = 1 NO-LOCK BREAK BY ahorros.nit:
    IF FIRST-OF(ahorros.nit) THEN DO:
        FIND FIRST cedulas WHERE cedula = ahorros.nit NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cedulas THEN DO:
            CREATE cedulas.
            cedulas.cedula = ahorros.nit.
        END.
    END.
END.

FOR EACH creditos WHERE creditos.estado = 2 NO-LOCK BREAK BY creditos.nit:
    IF FIRST-OF(creditos.nit) THEN DO:
        FIND FIRST cedulas WHERE cedula = creditos.nit NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cedulas THEN DO:
            CREATE cedulas.
            cedulas.cedula = creditos.nit.
        END.
    END.
END.

OUTPUT TO c:\INFO_Fodun\Leonardo\cedulasUnificadas.txt.
FOR EACH cedulas NO-LOCK:
    DISPLAY cedulas.cedula FORMAT "X(15)".
END.
