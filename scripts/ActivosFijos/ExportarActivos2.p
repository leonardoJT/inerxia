OUTPUT TO d:\Leonardo\activos.csv.
FOR EACH activosFijos WHERE tipo = 5 AND agencia = 1 AND cen_costos = 2 AND contabilizado = TRUE NO-LOCK:
    EXPORT DELIMITER ";" idActivo valorCompra. 
END.
OUTPUT CLOSE.
