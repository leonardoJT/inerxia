DEFI VAR FProxLiq LIKE Ahorros.Fec_ProLiquidacion. 
DEFI VAR NroA  AS INTEG FORM "9999".
DEFI VAR WDia AS INTEG FORM "99".

DISABLE TRIGGERS FOR LOAD OF ahorros.
OUTPUT TO C:\Infred\CooErrvctocdat.txt.
FOR EACH ahorros WHERE tip_aho EQ 3 AND sdo_dispon GT 0 
                   AND fec_proliq GE TODAY AND fec_venci LT TODAY:
      DISPLAY fec_apert fec_prorrog plazo fec_venci fec_proliq per_liq fec_ultliq fec_venci fec_proliq WITH WIDTH 150 NO-BOX NO-LABEL.


      ASSIGN FProxLiq = Ahorros.Fec_ProLiquidacion
             Ahorros.Fec_Vencimiento = Ahorros.Fec_ProLiquidacion + 1.
                                                                                                                                                                              
      IF Ahorros.Per_Liquidacion EQ 6 THEN
         ASSIGN Ahorros.Fec_Vencimiento = Ahorros.Fec_ProLiquidacion + 1.
      ELSE IF (Ahorros.Per_Liquidacion EQ 5 AND Ahorros.Plazo GT 365) THEN DO:                                            
         ASSIGN NroA = ROUND(Ahorros.Plazo / 360,0) 
                WDia = DAY(FProxLiq).
         IF WDia GT 30 THEN
            WDia = 30.

         IF MONTH(FProxLiq) EQ 2 AND WDia GT 28 THEN
            WDia = 28.

         Ahorros.Fec_Vencimiento = DATE(MONTH(FProxLiq),WDia,(YEAR(FProxLiq) + NroA)).                 
      END.
      ELSE                                                                                                             
         IF ((  Ahorros.Per_Liquidacion EQ 2 AND Ahorros.Plazo NE 30)                                                 
            OR (Ahorros.Per_Liquidacion EQ 3 AND Ahorros.Plazo NE 90)                                                  
            OR (Ahorros.Per_Liquidacion EQ 4 AND Ahorros.Plazo NE 180))                                                
            AND Ahorros.Plazo GT 31 THEN                                                                               
                RUN HallaFVcto.
      

    DISPLAY fec_apert fec_prorrog plazo fec_venci fec_proliq per_liq fec_ultliq fec_venci fec_proliq WITH FRAME f2 WIDTH 150 NO-BOX NO-LABEL.
  END.   

  PROCEDURE HallaFVcto:

  DEFI VAR NMes      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR FProxVcto AS DATE.
  DEFI VAR NroM      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR WDia      AS INTEG FORMAT "99".
 
  ASSIGN FProxVcto = Ahorros.fec_prorrog - 1 + Ahorros.Plazo 
         NMes      = MONTH(Ahorros.fec_prorrog)
         NroM      = ROUND(Ahorros.Plazo / 30,0)
         WDia      = DAY(Ahorros.fec_prorrog).

  IF WDia GT 30 THEN
     WDia = 30.

  IF Ahorros.Plazo GT 31 THEN DO:        
     ASSIGN NMes = Nmes + NroM.

     IF NMes GE 25 THEN DO:                                                            
        ASSIGN NMes = NMes - 24.
        IF NMes EQ 2 AND WDia GT 28 THEN
           WDia = 28.
        FProxVcto = DATE(NMes,WDia,YEAR(Ahorros.fec_prorrog) + 2).            
     END.
     ELSE IF NMes GE 13 THEN DO:                                                            
        ASSIGN NMes = NMes - 12.
        IF NMes EQ 2 AND WDia GT 28 THEN
           WDia = 28.
        FProxVcto = DATE(NMes,WDia,YEAR(Ahorros.fec_prorrog) + 1).  
     END.
     ELSE DO:                                                                          
        IF NMes EQ 2 AND WDia GT 28 THEN
           WDia = 28.
        FProxVcto = DATE(NMes,WDia,YEAR(Ahorros.fec_prorrog)). 
     END.

     ASSIGN Ahorros.Fec_Vencimiento = FProxVcto.     
  END.
END PROCE.
