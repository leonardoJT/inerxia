DEFINE TEMP-TABLE cedulas
    FIELD cedula AS CHARACTER
    FIELD valor AS DECIMAL.

INPUT FROM "C:\INFO_FODUN\nomina medellin\APORTES05PENS.csv".

REPEAT :
    CREATE cedulas.
    IMPORT DELIMITER ";" cedulas.

    FIND FIRST clientes WHERE clientes.nit = cedulas.cedula NO-ERROR.
    IF AVAILABLE clientes THEN
        clientes.cod_empresa = 9.
        /*DISPLAY clientes.cod_empresa.*/
END.
