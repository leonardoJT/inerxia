 /***********************************************************************************
  PROCEDURE: Procedimiento que pasa un valor numérico a letras.
  FECHA    : IX-17-1.997
  
 ************************************************************************************/
 DEFINE /*INPUT  PARAMETER*/ VAR P_Valor AS DECIMAL INITIAL /*1280458000*/ 990000000000
     FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99".
 DEFINE /*INPUT  PARAMETER*/ VAR P_Sw    AS INTEGER.
 DEFINE /*OUTPUT PARAMETER*/ VAR P_Monto AS CHAR    FORMAT "X(120)" INITIAL "".


 DEFINE VAR W_Esc AS INTEGER INITIAL 0.
 DEFINE VAR W_Ind AS INTEGER INITIAL 0.
 DEFINE VAR W_Ent AS INTEGER INITIAL 0.
 DEFINE VAR W_Cad AS CHAR    INITIAL "".
 DEFINE VAR W_Vlr AS CHAR    INITIAL "".
   
 DEFINE VAR Unidades AS CHAR EXTENT 10 INITIAL
            ["Un ","Dos ","Tres ","Cuatro ","Cinco ","Seis ","Siete ","Ocho ","Nueve "].

 DEFINE VAR Dec_10 AS CHAR EXTENT 10 INITIAL
            ["Once ","Doce ","Trece ","Catorce ","Quince ","Dieciseis ",
             "Diecisiete ","Dieciocho ","Diecinueve ","Veinte "].

 DEFINE VAR Decenas AS CHAR EXTENT 10 INITIAL
            ["Diez ","Veinti ","Treinta ","Cuarenta ","Cincuenta ","Sesenta ","Setenta ",
             "Ochenta ","Noventa "].

 DEFINE VAR Centenas AS CHAR EXTENT 10 INITIAL
            ["Cien","Doscientos ","Trescientos ","Cuatrocientos ","Quinientos ",
             "Seiscientos ","Setecientos ","Ochocientos ","Novecientos "].
 RUN Monto.

 IF P_Valor NE ROUND(P_Valor,0) THEN
    ASSIGN P_Monto = REPLACE(P_Monto,"ML","Con ")
           P_Monto = P_Monto + " Cvs.ML.".
/************************************************************************************
 ************************************************************************************/
 PROCEDURE Monto:
   W_Cad = TRIM(STRING(P_Valor,"999999999999.99")).

   DO W_Esc = 1 TO 5 BY 1:
      CASE W_Esc:
        WHEN 1 THEN DO:      
            W_Vlr = SUBSTRING(W_Cad,1,3).
            IF INTEGER(W_vlr) >= 1 THEN
             DO:
               RUN Descomponer.       
               P_Monto = P_Monto + "Mil ".
             END.
        END.                         

        WHEN 2 THEN DO:
            W_Vlr = SUBSTRING(W_Cad,4,3).
            IF INTEGER(W_vlr) > 0 THEN
               RUN Descomponer.       
            IF P_Monto <> "" AND DECIMAL(W_Cad) < 1999999 THEN
                 P_Monto = P_Monto + "Millon ".
            ELSE
              IF P_Monto <> "" AND DECIMAL(W_Cad) > 1999999 THEN
                 P_Monto = P_Monto + "Millones ". 
            IF DECIMAL(SUBSTRING(W_Cad,7,6)) = 0 AND P_Monto <> "" THEN
               P_Monto = P_Monto + "de ".
        END.  

        WHEN 3 THEN DO:
            W_Vlr = SUBSTRING(W_Cad,7,3).
            IF INTEGER(W_vlr) > 0 THEN
             DO:
               IF INTEGER(W_vlr) > 1 THEN
                  RUN Descomponer.
               P_Monto = P_Monto + "Mil ".
             END.
        END.  

        WHEN 4 THEN DO:  
            W_Vlr = SUBSTRING(W_Cad,10,3).
            RUN Descomponer.
            IF P_Sw = 0 THEN
               IF INTEGER(W_Vlr) <> 1 THEN
                  P_Monto = P_Monto + "Pesos ML".
               ELSE
                  P_Monto = P_Monto + "Peso ML".            
        END. 

        WHEN 5 THEN DO:  
            W_Vlr = "0" + SUBSTRING(W_Cad,14,2).
            IF INTEGER(W_Vlr) <> 0 AND P_Sw = 1 THEN
               P_Monto = P_Monto + "Punto ".
            RUN Descomponer.
        END.
      END CASE.  
   END.  

   P_Monto = TRIM(P_Monto).

 END PROCEDURE.


 /*--------------------*/                
 PROCEDURE Descomponer:
   DEFINE VAR W_Aux AS INTEGER INITIAL 0.
   
   IF INTEGER(W_Vlr) <> 0 THEN DO:   
       DO W_Ind = 1 TO 3 BY 1:
          IF W_Ind = 2 AND W_Ent = 1 THEN
             IF INTEGER(SUBSTRING(W_Vlr,2,2)) <> 0 THEN
                P_Monto = P_Monto + "to ".
             ELSE
                P_Monto = P_Monto + " ".

          W_Ent = INTEGER(SUBSTRING(W_Vlr,W_Ind,1)).

          IF W_Ent <> 0 THEN
             CASE W_Ind:
               WHEN 1 THEN  
                 P_Monto = P_Monto + Centenas[W_Ent].

               WHEN 2 THEN DO:                   
                   W_Aux = INTEGER(SUBSTRING(W_Vlr,2,2)).
                   IF W_Aux > 10 AND W_Aux < 21 THEN DO:    
                      W_Aux = W_Aux - 10.
                      P_Monto = P_Monto + Dec_10[W_Aux].
                      LEAVE.
                   END.
                   ELSE       
                      IF INTEGER(SUBSTRING(W_Vlr,3,1)) = 0 THEN
                         ASSIGN P_Monto = P_Monto + Decenas[W_Ent].
                      ELSE DO:
                        IF P_Sw = 0 THEN DO:
                           IF W_Ent EQ 2 THEN
                              ASSIGN P_Monto = P_Monto + TRIM(Decenas[W_Ent]).
                           ELSE
                              ASSIGN P_Monto = P_Monto + Decenas[W_Ent] + "y ".
                        END.
                        ELSE
                           ASSIGN P_Monto = P_Monto + Decenas[W_Ent].
                      END.
               END.

               WHEN 3 THEN
                 IF INTEGER(W_Vlr) <= 1 THEN
                    P_Monto = P_Monto + "Un ".
                 ELSE
                    P_Monto = P_Monto + Unidades[W_Ent].
             END CASE.
       END. 
   END.
 END PROCEDURE.
