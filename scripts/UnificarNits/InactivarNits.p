DEFINE VAR cedula AS CHARACTER.

DEFINE TEMP-TABLE cedulas
    FIELD cedula AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER.

INPUT FROM c:\INFO_Fodun\Leonardo\cedulasUnificadas.txt.
REPEAT :
    IMPORT cedula.

    FOR EACH clientes WHERE clientes.nit <> cedula
                        AND INDEX(clientes.nit,cedula) = 1
                        AND clientes.estado = 1:
        CREATE cedulas.
        cedulas.cedula = clientes.nit.
        cedulas.nombre = clientes.nombre.
        cedulas.apellido1 = clientes.apellido1.
        cedulas.apellido2 = clientes.apellido2.

        clientes.estado = 2.
        clientes.fec_retiro = TODAY.
        clientes.cod_retiro = 22.
    END.
END.
INPUT CLOSE.

OUTPUT TO c:\INFO_Fodun\Leonardo\ClientesInactivados.txt.
FOR EACH cedulas NO-LOCK:
    DISPLAY cedulas.cedula FORMAT "X(15)"
            STRING(cedulas.nombre + " " + cedulas.apellido1 + " " + cedulas.apellido2,"X(100)")
        WITH WIDTH 120.
END.
