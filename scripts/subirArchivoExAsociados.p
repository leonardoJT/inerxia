DISABLE TRIGGERS FOR LOAD OF clientes.

DEFINE TEMP-TABLE archivo
    FIELD nit AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD ciudad AS CHARACTER.

INPUT FROM value("c:\INFO_fodun\leonardo\listado ex asociados.csv").
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

    clientes.DIR_residencia = archivo.direccion.

    FIND FIRST ubicacion WHERE ubicacion.nombre = archivo.ciudad AND (ubicacion.tipo = "C" OR ubicacion.tipo = "D") NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ubicacion THEN DO:
        MESSAGE archivo.nit archivo.ciudad
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        NEXT.
    END.

    clientes.lugar_residencia = ubicacion.ubicacion.
END.
