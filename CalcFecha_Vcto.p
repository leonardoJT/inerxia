    DEFI INPUT  PARAM FecIni    AS DATE.               /*Ej.Desembolso*/
    DEFI INPUT  PARAM W_NroDias AS INTEG FORM "99999".
    DEFI INPUT  PARAM FecAct    AS DATE.               /*La Anterior para hallar la nueva*/
    DEFI OUTPUT PARAM W_Fectra  AS DATE.
    
    DEFINE VAR P_Fecha  AS DATE.
    DEFINE VAR W_Mes    AS INTEGER INITIAL 0.
    DEFINE VAR W_Ind    AS INTEGER INITIAL 0.
    DEFINE VAR W_DiaMes AS INTEGER EXTENT 12 INITIAL 
               [31,28,31,30,31,30,31,31,30,31,30,31].

    ASSIGN P_Fecha  = FecIni
           W_Fectra = FecAct.

    IF W_NroDias >= 30 THEN DO: 
       W_Mes = W_NroDias / 30.  

       DO W_Ind = 1 TO W_mes BY 1:                        
          IF MONTH(W_FecTra) = 2 AND YEAR(W_FecTra) MODULO 4 = 0 THEN
             W_FecTra = W_FecTra + 1.

          IF (W_DiaMes[MONTH(P_Fecha)] = DAY(P_Fecha)) 
          OR (MONTH(P_Fecha) = 2 AND DAY(P_Fecha) >= 28) THEN DO: 
             IF MONTH(W_FecTra) + 1 > 12 THEN
                W_FecTra = W_FecTra + W_DiaMes[1].
             ELSE W_FecTra = W_FecTra + W_DiaMes[MONTH(W_FecTra) + 1].
          END.
          ELSE DO:
            IF DAY(P_Fecha) >= 29 AND MONTH(W_FecTra) <= 2 THEN
               IF MONTH(W_FecTra) = 1 THEN
                  W_FecTra = DATE(02,28,(YEAR(W_FecTra))).
               ELSE
                  W_FecTra = DATE(03,DAY(P_Fecha),(YEAR(W_FecTra))).
            ELSE
               W_FecTra = W_FecTra + W_DiaMes[MONTH(W_FecTra)].
          END.     
       END.
    END.
    ELSE DO:
       IF W_NroDias = 10 OR W_NroDias = 15 THEN DO:                                             
          IF MONTH(W_FecTra) = 4 OR MONTH(W_FecTra) = 6 OR MONTH(W_FecTra) = 9 OR MONTH(W_FecTra) = 11 THEN
             W_FecTra = W_FecTra + W_NroDias. 
          ELSE DO:
             IF MONTH(W_FecTra) NE 2 THEN DO:
                IF DAY(W_FecTra) LE 15 OR (W_NroDias = 10 AND DAY(W_FecTra) LE 20) THEN
                   W_FecTra = W_FecTra + W_NroDias.
                ELSE
                   W_FecTra = W_FecTra + W_NroDias + 1.                                                         
             END.
             ELSE DO:  /*Solo Febrero de FecAct*/
                IF DAY(W_FecTra) LE 15 OR (W_NroDias = 10 AND DAY(W_FecTra) LE 20) THEN DO:
                   W_FecTra = W_FecTra + W_NroDias.
                   IF MONTH(W_FecTra) EQ 3 THEN
                      W_FecTra = DATE(2,28,YEAR(W_FecTra)).
                END.
                ELSE
                   W_FecTra = W_FecTra + W_NroDias - 2.

                IF MONTH(W_FecTra) EQ 3 THEN DO:
                   IF YEAR(W_FecTra) MODULO 4 EQ 0 THEN
                      W_FecTra = W_FecTra + 1.

                   IF  DAY(W_FecTra) LE 15 AND DAY(P_Fecha) LE 15
                   AND DAY(P_Fecha) NE DAY(W_FecTra) THEN
                       ASSIGN W_FecTra = DATE(3,DAY(P_Fecha),YEAR(W_FecTra)).
                END.
             END.
          END.
       END.
       ELSE W_FecTra = W_FecTra + W_NroDias.        
    END.
