DEFINE TEMP-TABLE formato
    FIELD nit AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD intereses AS DECIMAL.

DEFINE VAR cont AS INTEGER.

FOR EACH anexos WHERE anexos.ano = 2014
                  AND SUBSTRING(anexos.cuenta,1,4) = "1655" NO-LOCK BREAK BY anexos.nit:
    IF FIRST-OF(anexos.nit) THEN DO:
        CREATE formato.
        formato.nit = anexos.nit.

        FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            formato.apellido1 = clientes.apellido1.
            formato.apellido2 = clientes.apellido2.
            formato.nombre = clientes.nombre.
            formato.direccion = clientes.DIR_residencia.

            FIND FIRST ubicacion WHERE SUBSTRING(ubicacion.ubicacion,1,5) = SUBSTRING(clientes.lugar_residencia,1,5) NO-LOCK NO-ERROR.
            IF AVAILABLE ubicacion THEN
                formato.municipio = ubicacion.nombre.
        END.
    END.

    DO cont = 1 TO 12:
        formato.intereses = formato.intereses + anexos.cr[cont] - anexos.db[cont].
    END.
END.

OUTPUT TO C:\INFO_CUB\FormatoIntereses.csv.
EXPORT DELIMITER ";"
    "NIT"
    "APELLIDO1"
    "APELLIDO2"
    "NOMBRE"
    "DIRECCION"
    "MUNICIPIO"
    "INTERESES".

FOR EACH formato NO-LOCK:
    EXPORT DELIMITER ";" formato.
END.
OUTPUT CLOSE.

MESSAGE "Finalizó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
