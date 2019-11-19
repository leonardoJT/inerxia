DEFINE TEMP-TABLE asociados
    FIELD nit AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD DIR_residencia AS CHARACTER
    FIELD tel_residencia AS CHARACTER
    FIELD tel_comercial AS CHARACTER
    FIELD email AS CHARACTER
    FIELD celular AS CHARACTER
    FIELD sexo AS CHARACTER
    FIELD ubicacion AS CHARACTER.

INPUT FROM c:\INFO_fodun\Leonardo\AsociadosMedellin.csv.
REPEAT :
    CREATE asociados.
    IMPORT DELIMITER ";" asociados.
END.
INPUT CLOSE.

FOR EACH asociados NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = asociados.nit NO-ERROR.
    IF NOT AVAILABLE clientes THEN
        MESSAGE asociados.nit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        clientes.apellido1 = CAPS(asociados.apellido1).
        clientes.apellido2 = CAPS(asociados.apellido2).
        clientes.nombre = CAPS(asociados.nombre1 + " " + asociados.nombre2).
        clientes.DIR_residencia = CAPS(asociados.DIR_residencia).
        clientes.tel_Residencia = CAPS(asociados.tel_residencia).
        clientes.tel_comercial = CAPS(Asociados.tel_comercial).
        clientes.email = asociados.email.
        clientes.celular = CAPS(asociados.celular).

        CASE asociados.sexo:
            WHEN "H" THEN clientes.sexo = 1.
            WHEN "M" THEN clientes.sexo = 2.
        END CASE.

        clientes.lugar_residencia = STRING(INTEGER(asociados.ubicacion),"99999999").

    END.
END.
