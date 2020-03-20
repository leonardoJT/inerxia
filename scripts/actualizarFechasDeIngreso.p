DISABLE TRIGGERS FOR LOAD OF clientes.
DISABLE TRIGGERS FOR LOAD OF ahorros.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD fecIngreso AS DATE.

INPUT FROM d:\leonardo\fecsIngresos.csv.
REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.
INPUT CLOSE.

FOR EACH tt NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = tt.nit NO-ERROR.
    IF clientes.fec_ingreso <> tt.fecIngreso THEN
        clientes.fec_ingreso = tt.fecIngreso.

    FIND FIRST ahorros WHERE ahorros.nit = clientes.nit
                         AND ahorros.tip_ahorro = 4
                         AND ahorros.estado = 1
                         AND ahorros.fec_apertura = 02/28/2011 NO-ERROR.
    IF AVAILABLE ahorros THEN
        ahorros.fec_apertura = tt.fecIngreso.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
