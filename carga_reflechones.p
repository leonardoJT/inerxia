DISABLE TRIGGERS FOR LOAD OF creditos.
DEFINE VAR xcre LIKE creditos.num_credito.
DEFINE VAR xtot LIKE creditos.sdo_capital.
DEFINE TEMP-TABLE t
   FIELD nit      LIKE clientes.nit
   FIELD tasa     LIKE creditos.tasa
   FIELD NumCre   LIKE Creditos.Num_Credito
   FIELD Fecha    AS CHARACTER FORMAT "X(8)"
   FIELD Salcap   LIKE Creditos.Sdo_Capital.

FIND LAST creditos.
IF AVAILABLE Creditos THEN xcre = creditos.num_credito.

INPUT FROM c:\migracion\refporcicultura.csv.
REPEAT:
    CREATE t.
    IMPORT DELIMITER ";" t.
END.
INPUT CLOSE.



FOR EACH t:
    CREATE creditos.
    ASSIGN Creditos.Agencia        = 1
           Creditos.Cod_Credito    = 183
           Creditos.Cuota          = 0
           Creditos.Desembolso     = 1
           Creditos.Estado         = 7
           Creditos.Fec_Aprobacion = DATE(STRING(SUBSTRING(t.fecha,1,2)) + "/" + 
                                          STRING(SUBSTRING(t.fecha,3,2)) + "/" + 
                                          STRING(SUBSTRING(t.fecha,5,4)))
           Creditos.Fec_Desembolso = Creditos.Fec_Aprobacion
           Creditos.For_Interes    = 1
           Creditos.For_Pago       = 1
           Creditos.Monto          = t.salcap
           Creditos.Nit            = t.Nit
           Creditos.Num_Credito    = t.numcre
           Creditos.Num_Solicitud  = t.numcre
           Creditos.Pagare         = STRING(t.numcre)
           Creditos.Per_Pago       = 4
           Creditos.Plazo          = 6
           Creditos.Sdo_Capital    = t.salcap
           Creditos.Sistema        = 1
           Creditos.Tasa           = t.tasa
           Creditos.Tip_Credito    = 1
           Creditos.Usuario        = "100"
           Creditos.Val_Desembolso = t.salcap
           xcre = xcre + 1.
END.




