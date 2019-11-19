DEFINE TEMP-TABLE ttAnexos
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD sdo_inicial AS DECIMAL.

INPUT FROM d:\Leonardo\anexos.csv.
REPEAT :
    CREATE ttAnexos.
    IMPORT DELIMITER ";" ttAnexos.

    FIND FIRST clientes WHERE clientes.nit = ttAnexos.nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE clientes THEN DO:
        CREATE clientes.
        clientes.agencia = ttAnexos.agencia.
        clientes.nit = ttAnexos.nit.
        clientes.nombre = ttAnexos.nombre.
    END.

    CREATE anexos13.
    BUFFER-COPY ttAnexos TO anexos13.
    anexos13.ano = 2011.
    
    CREATE anexos.
    BUFFER-COPY ttAnexos TO anexos.
    anexos.ano = 2012.

    CREATE anexos13.
    BUFFER-COPY ttAnexos TO anexos13.
    anexos13.ano = 2012.

    CREATE anexos.
    BUFFER-COPY ttAnexos TO anexos.
    anexos.ano = 2013.

    DISPLAY ttAnexos WITH 1 COL.
END.
