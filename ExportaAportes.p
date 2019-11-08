
DEFINE VARIABLE Nom AS CHARACTER FORMAT "x(70)" NO-UNDO.
DEFINE VARIABLE Num AS INTEGER     NO-UNDO.
DEFINE VARIABLE Concepto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE CuotaExtra AS INTEGER     NO-UNDO.
DEFINE VARIABLE SaldoExtra AS INTEGER     NO-UNDO.

OUTPUT TO c:\INFO_cooprudea\Aportes.csv.

ASSIGN num = 1.
FOR EACH clientes WHERE Tipo_Vinculo = 1  NO-LOCK:
    ASSIGN nom = "".
    ASSIGN nom = trim(clientes.apellido1 + " " + clientes.apellido2 + " " + clientes.Nombre).
    ASSIGN num = num + 1. 
    ASSIGN CuotaExtra = 0.
    ASSIGN SaldoExtra = 0.
    FOR EACH ahorros WHERE ahorros.nit = clientes.nit AND cod_ahorro = 5 NO-LOCK:
        PUT num ";" clientes.nit ";Ordinarios;" ahorros.cuota ";" ahorros.Sdo_Disponible SKIP(0).
    END.
    ASSIGN num = num + 1.            
    FOR EACH ahorros WHERE ahorros.nit = clientes.nit AND cod_ahorro = 217 NO-LOCK:
        ASSIGN  CuotaExtra = CuotaExtra + ahorros.cuota
                SaldoExtra = SaldoExtra + ahorros.Sdo_Disponible.        
    END.
    IF CuotaExtra NE 0 OR SaldoExtra NE 0 THEN DO:
        PUT num ";" clientes.nit ";Por Servicios;" CuotaExtra ";" SaldoExtra SKIP(0).
    END.


END.
