DEFINE VARIABLE totage1 AS INTEGER.
DEFINE VARIABLE totage2 AS INTEGER.
DEFINE VARIABLE totage3 AS INTEGER.
DEFINE VARIABLE totjur AS INTEGER.
DEFINE VARIABLE tothom AS INTEGER.
DEFINE VARIABLE totmuj AS INTEGER.
DEFINE VARIABLE totpre AS INTEGER.




/* asociados */
OUTPUT TO c:\estadistico.txt.
/*FOR EACH ahorros WHERE cod_ahorro = 1 AND (sdo_disponible + sdo_canje) GT 0:
    IF agencia = 1 THEN totage1 = totage1 + 1.
    IF agencia = 2 THEN totage2 = totage2 + 1.
    IF agencia = 3 THEN totage3 = totage3 + 1.
END.

DISPLAY "asociados boyaca " totage1 SKIP
        "asociados pedregal" totage2 SKIP
        "asociados estrella " totage3 SKIP
        "total .. " (totage1 + totage2 + totage3).
OUTPUT TO CLOSE.*/

/* hombres - mujeres - juridicos */
FOR EACH ahorros WHERE cod_ahorro = 1 AND (sdo_disponible + sdo_canje) GT 0:
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-ERROR.
    IF tipo_cliente GT 2 THEN totjur = totjur + 1.
    ELSE DO:
       IF sexo = 1 THEN tothom  = tothom + 1.
       ELSE totmuj = totmuj + 1.
    END.
    
END.
totjur = totjur + 52.
tothom = tothom - 52.
DISPLAY "asociados juridicos " totjur SKIP
        "asociados hombres   " tothom SKIP
        "asociados mujeres   " totmuj SKIP
        "total .. " (totjur + tothom + totmuj) SKIP(3).  


FOR EACH creditos WHERE YEAR(fec_desembolso) = 2006:
    totpre = totpre + 1.
END.
DISPLAY "Numero de Prestamo " totpre SKIP(2).

DISPLAY "Tasa Creditos " 23.6265 SKIP(1).
DISPLAY "Tasa Ahorros  " 4.1536  SKIP(1).
DISPLAY "Tasa CDAT     " 7.36    SKIP(1).

