DEF VAR W_ManFin AS HANDLE no-undo.
DEF VAR xpro AS DECIMAL.
DEF VAR xatr AS DECIMAL.
DEF VAR xfpro AS DATE.
DEF VAR xUbica AS DATE.
DEF VAR xDiasatr AS DECIMAL.

    RUN RUTFINAN.R PERSISTENT SET W_ManFin.    
    
RUN HSP IN W_manfin (03/31/2006 - 45,
                     6500000,
                     334007,
                     6500000,
                     21,
                     36,
                     1019257,
                         OUTPUT xpro,
                         OUTPUT xatr,
                         OUTPUT xfpro,
                         OUTPUT xUbica,
                         OUTPUT xDiasatr).
    
    MESSAGE xpro 
            xatr   
            xfpro  
            xUbica 
            xDiasatr
            
            
            
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
