DISABLE TRIGGERS FOR LOAD OF clientes.

DEFINE temp-TABLE ttfec
    FIELD nit AS CHARACTER
    FIELD fec_Ingreso AS DATE.

INPUT FROM d:\Leonardo\fecIngreso.csv.
REPEAT :
    CREATE ttfec.
    IMPORT DELIMITER ";" ttfec NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        DISPLAY ttfec.
END.
INPUT CLOSE.

FOR EACH ttfec WHERE ttfec.fec_ingreso <> ? NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = ttfec.nit NO-ERROR.
    IF NOT AVAILABLE clientes THEN
        DISPLAY ttfec.

    clientes.fec_ingreso = ttfec.fec_ingreso.
END.
