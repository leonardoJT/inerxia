

DEFINE TEMP-TABLE tc
    FIELDS nit          LIKE clientes.nit
    FIELDS apellido1    LIKE clientes.apellido1
    FIELDS apellido2    LIKE clientes.apellido2
    FIELDS nombre1      LIKE clientes.nombre
    FIELDS nombre2      LIKE clientes.nombre
    INDEX idx nit.

INPUT FROM "c:\migrar\ASOCIADOSVarios.csv".

REPEAT:
    CREATE tc.
    IMPORT DELIMITER ";" tc.
END.
INPUT CLOSE.

FOR EACH tc WHERE nit NE "" NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit EQ tc.nit NO-ERROR.
    UPDATE clientes.apellido1 = tc.apellido1
           clientes.apellido2 = tc.apellido2
           clientes.nombre     = tc.nombre1 + " "  + tc.nombre2
           clientes.tipo_vinculo = 1.
    FIND FIRST anexos_clientes WHERE anexos_clientes.nit EQ clientes.nit NO-ERROR.
    IF NOT AVAILABLE anexos_clientes THEN
        CREATE anexos_clientes.
    ASSIGN anexos_clientes.nit = tc.nit
            anexos_clientes.nombre1   = tc.nombre1  
            anexos_clientes.nombre2   = tc.nombre2. 

END.
