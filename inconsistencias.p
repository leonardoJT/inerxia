
DEFINE VAR wvlrIant        AS DECIMAL INITIAL 0.
DEFINE VAR wvlrIDifCobro   AS DECIMAL INITIAL 0.
DEFINE VAR wvlrIctes       AS DECIMAL INITIAL 0.
DEFINE VAR WCONTADOR AS DECIMAL INITIAL 0.
OUTPUT TO c:\incons_Creditos_IntAnticipados.txt.
PUT "       Age Cédula         Nro.Cr       Monto Fec_Desem  Plazo SdoCapital      cuota        Dif.Cob      IntCtes        IntAnt        Dif.Cob       IntCtes        IntAnt" SKIP(1).

FOR EACH creditos  WHERE (Creditos.Int_Anticipado > 0 AND (Creditos.INT_Corrientes + Creditos.Int_DifCobro) > 0 ):
  ASSIGN wvlrIant         =  Int_Anticipado 
         wvlrIctes        =  Int_Corrientes 
         wvlrIDifCobro    =  Int_DifCobro
         Wcontador = wcontador + 1.  
   PUT   wcontador          FORMAT "zz,zz9" " "
         agencia            FORMAT "zz9" " "        
         Nit                FORMAT "X(12)" " "       
         num_credito        FORMAT "zzzzzzz9" " "   
         Monto              FORMAT "zzz,zzz,zz9" " "
         Fec_Desembolso     FORMAT "99/99/9999"  " " 
         Plazo              FORMAT "zzz9"        " "       
         Sdo_Capital        FORMAT "zzz,zzz,zz9" " "
         Cuota              FORMAT "zz,zzz,zz9"  " " 
         Int_DifCobro       FORMAT "zz,zzz,zz9.99"  " " 
         Int_Corrientes     FORMAT "zz,zzz,zz9.99"  " " 
         Int_Anticipado     FORMAT "zz,zzz,zz9.99"  " ". 
  IF wvlrIDifCobro > 0 THEN DO:
     IF  wvlrIDifCobro >= wvlrIant THEN DO:
         ASSIGN wvlrIDifCobro = wvlrIDifCobro - wvlrIant
                wvlrIant = 0.
         PUT wvlrIDifCobro FORMAT "zzz,zzz,zz9.99" " "  wvlrIctes FORMAT "zz,zzz,zz9.99"  " " wvlrIant  FORMAT "zz,zzz,zz9.99" SKIP(0).
     END.
     ELSE DO: 
         ASSIGN  wvlrIant = wvlrIant - wvlrIDifCobro .
                 wvlrIDifCobro = 0.

         IF wvlrIant >= wvlrIctes THEN DO:
            ASSIGN wvlrIant = wvlrIant - wvlrIctes
                   wvlrIctes = 0.
         END.
         ELSE DO:
            ASSIGN wvlrIctes = wvlrIctes - wvlrIant
                   wvlrIant = 0.
         END.
         PUT wvlrIDifCobro FORMAT "zzz,zzz,zz9.99" " "  wvlrIctes FORMAT "zz,zzz,zz9.99"  " " wvlrIant  FORMAT "zz,zzz,zz9.99" SKIP(0).
     END.
  END.
  ELSE DO:
      IF  wvlrIctes >= wvlrIant THEN DO:
          ASSIGN wvlrIctes = wvlrIctes - wvlrIant
                 wvlrIant = 0.
      END.
      ELSE DO: 
           ASSIGN wvlrIant = wvlrIant - wvlrIctes
                  wvlrIctes = 0.
      END.
      PUT wvlrIDifCobro FORMAT "zzz,zzz,zz9.99" " "  wvlrIctes FORMAT "zz,zzz,zz9.99"  " " wvlrIant  FORMAT "zz,zzz,zz9.99" SKIP(0).
  END.
  ASSIGN  Creditos.Int_DifCobro     =  wvlrIDifCobro      
          Creditos.Int_Corrientes   =  wvlrIctes     
          Creditos.Int_Anticipado   =  wvlrIant. 
END.
/*FOR EACH Creditos  WHERE Creditos.Nit = '' AND num_credito = 11171 BY Creditos.Nit:  
      DISPLAY  "Creditos.Sdo_Capital      "    Creditos.Sdo_Capital         SKIP
               "Creditos.Nit              "    Creditos.Nit                 SKIP
               "Creditos.Costas           "    Creditos.Costas              SKIP
               "Creditos.Cuota            "    Creditos.Cuota               SKIP
               "Creditos.Fec_Desembolso   "    Creditos.Fec_Desembolso      SKIP
               "Creditos.Int_Anticipado   "    Creditos.Int_Anticipado      SKIP
               "Creditos.Int_Corrientes   "    Creditos.Int_Corrientes      SKIP
               "Creditos.Int_DifCobro     "    Creditos.Int_DifCobro        SKIP
               "Creditos.Int_MoraDifCob   "    Creditos.Int_MoraDifCob      SKIP
               "Creditos.Int_MorCobrar    "    Creditos.Int_MorCobrar       SKIP
               "Creditos.Monto            "    Creditos.Monto               SKIP
               "Creditos.Plazo            "    Creditos.Plazo               SKIP
               "Creditos.Polizas          "    Creditos.Polizas             SKIP
               "(Creditos.Int_Anticipado   GT 0 AND "                       SKIP
               "(Creditos.INT_Corrientes + Creditos.Int_DifCobro GT 0)"    .
      UPDATE creditos WITH 1 COLUMN.
END.
  */                 
/*
OUTPUT TO c:\Inc_IntAnticip.txt.
SELECT agencia            FORMAT "zz9",
       Nit                FORMAT "X(12)",
       num_credito        FORMAT "zzzzzzz9",
       Monto              FORMAT "zzz,zzz,zz9",
       Fec_Desembolso     FORMAT "99/99/9999",
       Plazo              FORMAT "zzz9",
       Sdo_Capital        FORMAT "zzz,zzz,zz9",
       Costas             FORMAT "zz,zzz,zz9",
       Cuota              FORMAT "zz,zzz,zz9",
       Int_Anticipado     FORMAT "zz,zzz,zz9",
       Int_Corrientes     FORMAT "zz,zzz,zz9",
       Int_DifCobro       FORMAT "zz,zzz,zz9",
       Int_MoraDifCob     FORMAT "zz,zzz,zz9",
       Int_MorCobrar      FORMAT "zz,zzz,zz9",
       Polizas            FORMAT "zz,zzz,zz9"
    FROM CREDITOS 
    WHERE (Creditos.Int_Anticipado > 0 AND (Creditos.INT_Corrientes + Creditos.Int_DifCobro > 0))
    WITH FRAME j WIDTH 320.      OUTPUT CLOSE.

*/

      
