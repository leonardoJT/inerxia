DISABLE TRIGGERS FOR LOAD OF CREDITOS.
DEFINE TEMP-TABLE tcred
    FIELD tnit  LIKE creditos.nit
    FIELD tfecpro LIKE  creditos.fec_pago
    FIELD tpagare LIKE  creditos.pagare
    FIELD tforma  LIKE  creditos.FOR_pago.

INPUT FROM c:\CREDIDNUEVO.csv.
REPEAT :
    CREATE tcred.

    IMPORT DELIMITER ";" tcred.
END.

FOR EACH tcred:
    FIND FIRST creditos WHERE creditos.nit = tcred.tnit AND
                              creditos.pagare = tcred.tpagare NO-ERROR.
    IF AVAILABLE(creditos)  THEN DO: 
      /*IF tcred.tforma NE creditos.for_pago THEN DO:

            DISP tnit tfecpro tpagare tforma VIEW-AS TEXT creditos.for_pago VIEW-AS TEXT.
            ASSIGN CREDITOS.FOR_PAGO = TCRED.TFORMA.                                                                   
       END.*/                                              
      IF tcred.tfecpro ne creditos.fec_pago THEN DO:
            DISP tnit tfecpro tpagare fec_pago.
            ASSIGN CREDITOS.FEC_PAGO = TFECPRO.
      END.
    END.

    
END.
