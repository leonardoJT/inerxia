DEFINE TEMP-TABLE cedulas
    FIELD cedula AS CHARACTER.

FOR EACH clientes WHERE clientes.estado = 1 NO-LOCK:
    CREATE cedulas.
    cedulas.cedula = clientes.nit.
END.

FOR EACH cedulas:
    FIND FIRST ahorros WHERE ahorros.nit = cedulas.cedula AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        DELETE cedulas.
        NEXT.
    END.

    FIND FIRST creditos WHERE creditos.nit = cedulas.cedula AND creditos.estado = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        DELETE cedulas.
        NEXT.
    END.
END.

OUTPUT TO c:\INFO_Fodun\Leonardo\cedulasUnificadasSinProductos.txt.
FOR EACH cedulas NO-LOCK:
    DISPLAY cedulas.cedula FORMAT "X(15)".
END.
OUTPUT CLOSE.

OUTPUT TO c:\INFO_Fodun\Leonardo\ClientesInactivadosCedulas.txt.
FOR EACH cedulas WHERE INDEX(cedulas.cedula,"-") > 0 NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit <> cedulas.cedula
                          AND INDEX(cedulas.cedula,clientes.nit) = 1
                          AND LENGTH(clientes.nit) = LENGTH(cedulas.cedula) - 2 NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        clientes.estado = 2.
        clientes.fec_retiro = TODAY.
        clientes.cod_retiro = 22.

        DISPLAY clientes.nit FORMAT "X(15)"
                cedulas.cedula FORMAT "X(15)".
    END.
END.

MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
