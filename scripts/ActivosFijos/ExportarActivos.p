OUTPUT TO d:\Leonardo\activos.csv.
FOR EACH activosFijos WHERE agencia = 1 AND tipo = 5 NO-LOCK:
    EXPORT DELIMITER ";" id nombre valorCompra valorActual + valorDepreciado cen_costos estado.
END.
