DISABLE TRIGGERS FOR LOAD OF anexos13.

DEFINE VAR suma AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR vProducto AS INTEGER.
DEFINE VAR vMes AS INTEGER.

DEFINE TEMP-TABLE archivo
    FIELD agencia AS INTEGER
    FIELD producto AS INTEGER
    FIELD cedula AS CHARACTER
    FIELD interes AS DECIMAL
    FIELD causado AS INTEGER.

vMes = 9.
INPUT FROM D:\Leonardo\Liquidacion\Ahorros_Octubre_Trim.csv.
REPEAT :
    CREATE archivo.
    IMPORT DELIMITER ";" archivo NO-ERROR.

    IF ERROR-STATUS:ERROR OR (archivo.interes = 0 AND archivo.causado = 0) THEN
        DELETE archivo.
END.

/*
03 - 61752001
04 - 61750503
05 - 61751002
06 - 61751001
07 - 61751501
08 - 61750505
09 - 61750505
*/

FOR EACH archivo /*WHERE archivo.agencia = 1 AND archivo.producto = 4*/ NO-LOCK:
    CASE archivo.producto:
        WHEN 3 THEN vCuenta = "61752001".
        WHEN 4 THEN vCuenta = "61750503".
        WHEN 5 THEN vCuenta = "61751002".
        WHEN 6 THEN vCuenta = "61751001".
        WHEN 7 THEN vCuenta = "61751501".
        WHEN 8 THEN vCuenta = "61750505".
        WHEN 9 THEN vCuenta = "61750505".
    END CASE.
        
    FIND FIRST anexos13 WHERE anexos13.nit = archivo.cedula
                          AND anexos13.agencia = archivo.agencia
                          AND anexos13.cuenta = vCuenta
                          AND anexos13.ano = 2012
                          AND anexos13.cen_costos = 999 NO-ERROR.
    IF NOT AVAILABLE anexos13 THEN DO:
        CREATE anexos13.
        ASSIGN anexos13.nit = archivo.cedula
               anexos13.agencia = archivo.agencia
               anexos13.cuenta = vCuenta
               anexos13.ano = 2012
               anexos13.cen_costos = 999.
    END.

    IF archivo.producto = 3 OR archivo.producto = 5 OR archivo.producto = 6 THEN
        anexos13.db[vMes] = anexos13.db[vMes] + archivo.causado.
    ELSE
        anexos13.db[vMes] = anexos13.db[vMes] + archivo.interes.

    suma = suma + archivo.interes + archivo.causado.
END.

MESSAGE "OK" string(suma,"$->>>,>>>,>>>,>>9.99")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
