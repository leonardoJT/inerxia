DEFINE VARIABLE nombres LIKE clientes.nombre.
DEFINE VARIABLE xtot AS DECIMAL.
OUTPUT TO c:\aportes.txt.
PUT "agencia;" 
    "Cedula;"
    "Aportes" SKIP.

FOR EACH ahorros WHERE tip_ahorro = 4 AND 
                       sdo_disponible + sdo_canje GT 0 NO-LOCK :
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.
    IF AVAILABLE(clientes) THEN DO:
        ASSIGN nombres = TRIM(apellido1) + " " + TRIM(apellido2) + " " + TRIM(nombre).
        PUT ahorros.agencia  ";" 
            ahorros.nit      ";"
            nombres          ";"
            sdo_disponible + sdo_canje FORMAT ">>>>>>>>>>>"   SKIP.
    END.
    ASSIGN xtot = xtot + sdo_disponible + sdo_canje.
END.
OUTPUT TO CLOSE.
UPDATE xtot FORMAT ">>>,>>>,>>>,>>>,>>>".
