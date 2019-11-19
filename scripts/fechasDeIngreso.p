DEFINE VAR cont AS INTEGER.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD fec_ingreso AS DATE
    FIELD fec_ingEmp AS DATE.

INPUT FROM d:\Leonardo\fechas.csv.
REPEAT :
    cont = cont + 1.

    CREATE tt.
    IMPORT DELIMITER ";" tt NO-ERROR.

    IF cont = 1 THEN
        DELETE tt.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = tt.nit NO-ERROR.

    clientes.fec_ingreso = tt.fec_ingreso.
    clientes.fec_ingEmpresa = tt.fec_ingEmp.
END.
