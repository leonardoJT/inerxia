

DEFINE VARIABLE vi AS INTEGER     NO-UNDO.
 
 DEFINE TEMP-TABLE tm
     FIELDS nit             LIKE creditos.nit
     FIELDS num_credito     LIKE creditos.num_credito
     FIELDS dias_atraso     LIKE creditos.dias_atraso
     FIELDS val_atraso      LIKE creditos.val_atraso
     INDEX Idx NIT num_credito.
 

 INPUT FROM "c:\migrar\atrasos.csv".

 REPEAT :
     CREATE tm.
     IMPORT DELIMITER ";" tm.
 END.

 FOR EACH tm WHERE tm.nit NE "" AND val_atraso GT 0 NO-LOCK:
     ASSIGN vi = vi + 1.
     FIND FIRST creditos WHERE creditos.nit EQ tm.nit AND creditos.num_credito EQ tm.num_credito NO-ERROR.
     UPDATE creditos.dias_atraso  = tm.dias_atraso 
             creditos.val_atraso  = tm.val_atraso  .
     DISP tm.dias_atraso tm.val_atraso.
 END.

 MESSAGE "Cargados " vi
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
