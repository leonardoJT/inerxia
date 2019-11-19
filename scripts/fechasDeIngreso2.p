DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD fecIngreso AS DATE.

INPUT FROM d:\leonardo\fecIngreso.csv.
REPEAT :
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.

FOR EACH tt WHERE tt.nit <> "" AND tt.fecIngreso <> ? NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = tt.nit NO-ERROR.
    IF AVAILABLE clientes THEN
        clientes.fec_ingreso = tt.fecIngreso.
END.
