    DEFINE VARIABLE BASETEM LIKE CREDITOS.SDO_CAPITAL.
    DEFINE VARIABLE XDIA AS INTEGER.
    DEFINE VARIABLE I AS INTEGER.
    DEFINE VARIABLE CAUSA LIKE CREDITOS.SDO_CAPITAL.
    DEFINE VARIABLE totCAUSA LIKE CREDITOS.SDO_CAPITAL.
    DEFINE VARIABLE diferencia AS DECIMAL.

    OUTPUT TO c:\ajustecapitalizado.csv.
    put   "agencia ;
             cod_credito ;
             BASETEM     ;
             totCAUSA    ;
             creditos.INT_corriente ; 
             diferencia" SKIP.
    FOR EACH CREDITOS WHERE  (creditos.cod_credito = 108 OR 
                        creditos.cod_credito = 113) AND SDO_CAPITAL GT 0 AND FEC_DESEMBOLSO NE ? : 
     ASSIGN XDIA = DATE(12,31,2010) - CREDITOS.FEC_DESEMBOLSO.
     IF XDIA GT 0 THEN DO:
       ASSIGN basetem = sdo_capital
              totcausa   = 0.
       do i = 1 TO xdia:
           CAUSA = (BASETEM) * (TASA / 36500).
           basetem = basetem + causa.
           totcausa = totcausa + causa.
       END.
       diferencia = totcausa - INT_corriente.
       IF diferencia gt 0 THEN DO:
           put   agencia ";"
                 nit     ";" 
                 xdia    ";"
                 cod_credito ";"
                 BASETEM      FORMAT ">>>>>>>>>>>>" ";"
                 totCAUSA     FORMAT ">>>>>>>>>>>>" ";"
                 creditos.INT_corriente FORMAT ">>>>>>>>>>>>"  
                 SKIP.
       END.
       ASSIGN INT_corriente = INT_corriente + diferencia.
     END.
    END.
