DISABLE TRIGGERS FOR LOAD OF creditos.
DEFINE VAR xcre LIKE creditos.num_credito.
DEFINE VAR xtot LIKE creditos.sdo_capital.
DEFINE TEMP-TABLE t
   FIELD nit      LIKE clientes.nit
   FIELD NumCre   LIKE Creditos.Num_Credito
   FIELD intdif   LIKE Creditos.Sdo_Capital
   FIELD Salmor   LIKE Creditos.Sdo_Capital.


INPUT FROM C:\migracion\faltacredito\falta3.csv.
REPEAT:
    CREATE t.
    IMPORT DELIMITER ";" t.
END.
INPUT CLOSE.



FOR EACH t:
    FIND FIRST creditos WHERE creditos.nit = t.nit AND
                              creditos.num_credito = t.numcre and
                              creditos.agencia = 3 NO-ERROR.
    IF AVAILABLE(creditos) THEN DO:
        DISPLAY creditos.INT_difcobro t.intdif t.salmor.
        ASSIGN creditos.INT_difcobro =  creditos.INT_difcobro + t.intdif + t.salmor.
    END.
    ELSE
        DISPLAY "no encontrado " t.nit numcre.
END.




