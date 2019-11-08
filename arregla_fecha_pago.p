DEFINE TEMP-TABLE feccre
    FIELD nro AS INTEGER
    FIELD tnit LIKE creditos.nit
    FIELD tnc  LIKE creditos.num_credito
    FIELD tplazo LIKE creditos.plazo
    FIELD fecini LIKE creditos.fec_desembolso
    FIELD fecpago LIKE creditos.fec_pago.


INPUT FROM c:\cartera\car3.csv.
REPEAT:
   CREATE feccre.
   IMPORT DELIMITER ";" feccre.
  
   FIND FIRST creditos WHERE creditos.nit = feccre.tnit AND
                             creditos.num_credito = feccre.tnc NO-ERROR.
   IF AVAILABLE(creditos) THEN DO:
       IF fecpago ne ? THEN do:
          DISP tnit fecpago creditos.fec_pago (fecpago - creditos.fec_pago).
        ASSIGN creditos.fec_pago = feccre.fecpago.
       END.
   END.
   ELSE DISP "no existe " nro.      
END.
