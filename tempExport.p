OUTPUT TO c:\INFO_fodun\prueba.csv.
FOR EACH creditos WHERE cod_credito = 113 AND estado = 2 NO-LOCK:
    EXPORT DELIMITER ";" nit num_credito sdo_capital.
END.
OUTPUT CLOSE.
