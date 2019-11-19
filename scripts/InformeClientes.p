DEFINE VAR tipoDocumento AS CHARACTER.

OUTPUT TO c:\INFO_fodun\Leonardo\Clientes.csv.
EXPORT DELIMITER ";"
    "TIPO_ID"
    "CÉDULA/NIT"
    "PRIMER APELLIDO"
    "SEGUNDO APELLIDO"
    "NOMBRE"
    "DIRECCIÓN RESIDENCIA"
    "TELÉFONO RESIDENCIA"
    "DEPARTAMENTO RESIDENCIA"
    "MUNICIPIIO RESIDENCIA"
    "DIRECCIÓN COMERCIAL"
    "TELÉFONO COMERCIAL"
    "DEPARTAMENTO COMERCIAL"
    "MUNICIPIIO COMERCIAL"
    "E-MAIL".

FOR EACH clientes WHERE estado = 1 NO-LOCK:
    EXPORT DELIMITER ";"
        clientes.tipo_identificacion
        clientes.nit
        clientes.apellido1
        clientes.apellido2
        clientes.nombre
        clientes.DIR_residencia
        clientes.tel_residencia
        SUBSTRING(clientes.lugar_residencia,1,2)
        SUBSTRING(clientes.lugar_residencia,3,3)
        clientes.DIR_comercial
        clientes.tel_comercial
        SUBSTRING(clientes.lugar_comercial,1,2)
        SUBSTRING(clientes.lugar_comercial,3,3)
        clientes.email.
END.
