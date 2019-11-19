OUTPUT TO c:\INFO_fodun\anexos1.csv.
FOR EACH anexos WHERE cuenta = "16909502" AND ano = 2013 AND agencia = 1 NO-LOCK:
    EXPORT DELIMITER ";" nit cen_costos sdo_inicial.
END.
OUTPUT CLOSE.

DEFINE VAR cont AS INTEGER.
DEFINE VAR sdoFinal AS DECIMAL.

OUTPUT TO c:\INFO_fodun\anexos2.csv.
FOR EACH anexos WHERE cuenta = "16909502" AND ano = 2012 AND agencia = 1 NO-LOCK:
    sdoFinal = anexos.sdo_inicial.

    DO cont = 1 TO 12:
        sdoFinal = sdoFinal + anexos.db[cont] - anexos.cr[cont].
    END.

    EXPORT DELIMITER ";" nit cen_costos sdo_Final.
END.
OUTPUT CLOSE.
