DEFINE VARIABLE Nom AS CHARACTER FORMAT "x(70)" NO-UNDO.
DEFINE VARIABLE SalFinCre LIKE ahorros.Sdo_Disponible.
DEFINE VARIABLE SalFinCon LIKE ahorros.Sdo_Disponible.
DEFINE VARIABLE SalFinCreJub LIKE ahorros.Sdo_Disponible.

OUTPUT TO c:\INFO_cooprudea\Ahorros.csv.
/*
PUT "Id;Nit;Nombre;SaldoFinalCrecediario;SaldoFinalContractual" SKIP(0).
*/

FOR EACH clientes WHERE estado = 1 AND Tipo_Vinculo = 1 NO-LOCK.
    ASSIGN  SalFinCon = 0
            SalFinCre = 0
            SalFinCreJub = 0
            nom = ""
            nom = trim(clientes.apellido1 + " " + clientes.apellido2 + " " + clientes.Nombre).

            
    FOR EACH ahorros WHERE ahorros.nit = clientes.nit AND cod_ahorro = 3 NO-LOCK:
        SalFinCre = SalFinCre + ahorros.Sdo_Disponible.
    END.
    FOR EACH ahorros WHERE ahorros.nit = clientes.nit AND (cod_ahorro = 215 OR cod_ahorro = 216) NO-LOCK:
        SalFinCon = SalFinCon + ahorros.Sdo_Disponible.
    END.
    /*
    FOR EACH ahorros WHERE ahorros.nit = clientes.nit AND cod_ahorro = 216:
        SalFinCreJub = SalFinCreJub + ahorros.Sdo_Disponible.
    END.
    */
    PUT clientes.nit ";" clientes.Nit ";" nom ";" SalFinCre ";" SalFinCon SKIP(0).

    /*
    IF SalFinCreJub = 0 THEN
        PUT clientes.nit ";" clientes.Nit ";" nom ";" SalFinCre ";" SalFinCon SKIP(0).
    ELSE
        PUT clientes.nit ";" clientes.Nit ";" nom ";" SalFinCreJub ";" SalFinCon SKIP(0).
    */

END.
OUTPUT CLOSE.


/* COLOCA LA FECHA DE LOS SALDOS */


DEFINE VARIABLE mes AS CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE Texto AS CHARACTER FORMAT "x(200)"  NO-UNDO.


CASE MONTH(TODAY):
    WHEN 1 THEN mes = "Enero".
    WHEN 2 THEN mes = "Febrero".
    WHEN 3 THEN mes = "Marzo".
    WHEN 4 THEN mes = "Abril".
    WHEN 5 THEN mes = "Mayo".
    WHEN 6 THEN mes = "Junio".
    WHEN 7 THEN mes = "Julio".
    WHEN 8 THEN mes = "Agosto".
    WHEN 9 THEN mes = "Septiembre".
    WHEN 10 THEN mes = "Octubre".
    WHEN 11 THEN mes = "Noviembre".
    WHEN 12 THEN mes = "Diciembre".
END CASE.
OUTPUT TO c:\INFO_cooprudea\Fechas.csv.

Texto = "1;" + trim(mes) + " " + string(DAY(TODAY)) + " de " + string(YEAR(TODAY)) + "  " + STRING(TIME, "HH:MM:SS AM") + ";" + trim(mes) + " " + string(DAY(TODAY)) + " de " + string(YEAR(TODAY)) + "  " + STRING(TIME, "HH:MM:SS AM") +  ";" + trim(mes) + " " + string(DAY(TODAY)) + " de " + string(YEAR(TODAY)) + "  " + STRING(TIME, "HH:MM:SS AM").

PUT Texto SKIP(0).

