DEFINE VAR saldo AS DECIMAL.
DEFINE VAR cont AS INTEGER.

DEFINE TEMP-TABLE ttanexos
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD cuenta AS CHARACTER
    FIELD saldoFinal AS DECIMAL.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,4) = "1625" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1635" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1650" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1660" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1690")
                  AND anexos.ano = 2011 NO-LOCK:
    saldo = anexos.sdo_inicial.

    DO cont = 1 TO 12:
        saldo = saldo + anexos.db[cont] - anexos.cr[cont].
    END.

    FIND FIRST ttanexos WHERE ttanexos.agencia = anexos.agencia
                          AND ttanexos.nit = anexos.nit
                          AND ttanexos.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttanexos THEN DO:
        CREATE ttanexos.
        ASSIGN ttanexos.agencia = anexos.agencia
               ttanexos.nit = anexos.nit
               ttanexos.cuenta = anexos.cuenta.
    END.

    ttanexos.saldoFinal = ttanexos.saldoFinal + saldo.
END.

OUTPUT TO c:\INFO_Fodun\Leonardo\1008_Negativos.csv.
FOR EACH ttAnexos WHERE ttanexos.saldoFinal < 0 NO-LOCK:
    EXPORT DELIMITER ";" ttanexos.
END.
