 /*Genera_PlanPagos.P..Desde Créditos*/
  DEFI TEMP-TABLE CopPP LIKE PlanPagos.
  
  DEFI VAR W_Fecha AS DATE.
  DEFI VAR TotReg  AS INTEG FORM "9999999".
  DEFI VAR CuoAdel LIKE Creditos.Cuo_Pagadas.
  DEFI VAR W_Proyectado LIKE Creditos.Sdo_Proyectado.
  DEFI VAR W_Tasa       LIKE Creditos.Tasa.
  DEFI VAR W_PdoLiq     AS INTEG FORM "99".
  DEFI VAR W_DiaPdo     AS INTEG FORM "99".
  DEFI VAR KSdo      LIKE Creditos.Sdo_Capital INIT 0. 
  DEFI VAR KInt      LIKE Creditos.Sdo_Capital INIT 0. 
  DEFI VAR KCap      LIKE Creditos.Sdo_Capital INIT 0. 
  DEFI VAR KIAcu     LIKE Creditos.Sdo_Capital INIT 0. 
  DEFI VAR KCAcu     LIKE Creditos.Sdo_Capital INIT 0. 

  DEFI VAR KCuo      LIKE Creditos.Cuota.              
  DEFI VAR NN           AS INTEG FORM "99999".
  DEFI VAR CuoFalt      AS INTEG FORM "99999".  
  DEFI VAR PdoTrans     AS INTEG FORM "99999".
  DEFI VAR W_FecIni   AS DATE.                      
  DEFI VAR xFecIni   AS DATE.                      
  DEFI VAR W_FecTra   AS DATE.
  DEFI VAR W_FecIniCont   AS DATE.
  DEFI VAR W_SiPdoTr  AS LOG INIT FALSE.
  
  OUTPUT TO C:\InfRed\ErrGenreal.Txt.

  W_Fecha = TODAY.

SESSION:SET-WAIT-STATE("General").

MESSAGE "Inicio Proceso Generar PlanPagos..., Primero se Borran todos los PlanPagos." SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.



  DISABLE TRIGGERS FOR LOAD OF creditos.
  DEFINE VARIABLE iax AS INTEGER.

DO iax = 41 TO 50:

  FOR EACH Creditos WHERE creditos.agencia = iax AND tip_Credito = 7 BY Creditos.Nit:
      IF Creditos.Sdo_Capital LE 0 THEN NEXT.
      /*if Creditos.fec_desembolso gt DATE(03,9,2007) THEN NEXT.*/
      
      IF Creditos.Sdo_Capital LE 0 
      OR Creditos.Nit         LE "0"      
      OR Creditos.Costas      LT 0 
      OR Creditos.Cuota       LE 0
      OR Creditos.Fec_Desembolso EQ ? OR STRING(Creditos.Fec_Desembolso) LE " "
      OR Creditos.Int_Anticipado LT 0
      OR Creditos.Int_Corrientes LT 0
      OR Creditos.Int_DifCobro   LT 0 
      OR Creditos.Int_MoraDifCob LT 0
      OR Creditos.Int_MorCobrar  LT 0 
      OR Creditos.Monto          LE 0
      OR Creditos.Plazo          LE 0
      OR Creditos.Polizas        LT 0 
      OR (Creditos.Int_Anticipado GT 0 AND (Creditos.INT_Corrientes + Creditos.Int_DifCobro GT 0)) THEN DO: 
          DISPLAY  Creditos.Nit Creditos.Pagare Creditos.Num_credito Cod_credito sdo_capital " Error datos"
           WITH FRAME F1 NO-LABELS NO-BOX.            
         NEXT.   
      END.

      FOR EACH  planpagos WHERE planpagos.agencia = creditos.agencia         AND
                                planpagos.nit     = creditos.nit             AND
                                planpagos.cod_credito = creditos.cod_credito AND
                                planpagos.num_credito = creditos.num_credito: 
          DELETE planpagos. 
      END.


      xFecIni = Creditos.Fec_Desembolso.
      IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
        xFecIni = Creditos.Fec_PagAnti.

      ASSIGN Per_pago = 4.
            /* Creditos.Fec_Desembolso = Creditos.Fec_Aprobac
             Creditos.Fec_PagAnt     = Creditos.Fec_Aprobac.*/

      IF Creditos.Monto LT Creditos.Sdo_Capital THEN
         Creditos.Monto = Creditos.Sdo_Capital.

      IF Creditos.Sistema NE 1 AND (Creditos.Cuota LT Creditos.Monto) THEN
         Creditos.Sistema = 1.

      ASSIGN W_FecTra   = xfecini
             W_FecIni   = xfecini
             Creditos.Sdo_CapPag     = Creditos.Monto - Creditos.Sdo_Capital
             Creditos.Val_Desembolso = Creditos.Monto
             Creditos.Sdo_Proyectado = Creditos.Monto
             Creditos.Capital_Acum   = 0
             Creditos.Int_LiqAcum    = 0
             Creditos.Sdo_Intpag     = 0
             Creditos.Cuo_Pagadas    = 0
             Creditos.Cuo_Atraso     = 0
             Creditos.Dias_Atraso    = 0
             Creditos.Val_Atraso     = 0
             Creditos.Estado         = 2
             /*Creditos.Fec_Aprobacion = Creditos.Fec_Desembolso*/
             Creditos.For_Interes    = 1
             W_PdoLiq = 12    /*Inicia mensual*/
             W_DiaPdo = 30.

      IF Creditos.Per_Pago EQ 1 THEN
         ASSIGN W_DiaPdo = 7
                W_PdoLiq = 52.
      ELSE IF Creditos.Per_Pago EQ 2 THEN
         ASSIGN W_DiaPdo = 10
                W_PdoLiq = 36.
      ELSE IF Creditos.Per_Pago EQ 3 THEN
         ASSIGN W_DiaPdo = 15
                W_PdoLiq = 24.
   
      ASSIGN Creditos.Fec_Pago = xfecini + W_DiaPdo
             W_Tasa            = Creditos.Tasa / (W_PdoLiq * 100). 


     IF Creditos.Sistema         EQ 2 
      OR Creditos.Plazo           EQ 1
      OR (Creditos.Sdo_Capital GE Creditos.Monto) THEN DO:
         ASSIGN Creditos.Cuo_Pagadas = 0.

         IF Creditos.Sistema EQ 2 AND Creditos.Plazo GT 1 THEN DO NN = 1 TO Creditos.Plazo:
            RUN Halla_FecVcto.R (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                                 OUTPUT W_FecTra).  
            Creditos.Fec_Pago = W_FecTra.
         END.
         ELSE DO:
            RUN Halla_FecVcto.R (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                                 OUTPUT W_FecTra).  
            Creditos.Fec_Pago = W_FecTra.
         END.            
      END.

      IF Creditos.Sistema EQ 2 
      OR Creditos.Plazo   EQ 1 THEN DO:
         RUN CrearPlan.
         ASSIGN PlanPagos.Id_PdoMes         = 1                      
                PlanPagos.Nro_Cuota         = 1                      
                PlanPagos.Fec_Ini           = xfecini
                PlanPagos.Fec_Vcto          = Creditos.Fec_Pago      
                PlanPagos.Fec_ProxPag       = Creditos.Fec_Pago      
                PlanPagos.Cuo_Pagas         = 0
                PlanPagos.Pagos_CapitalAcum = Creditos.Sdo_CapPag
                PlanPagos.Pagos_CapitalPdo  = Creditos.Sdo_CapPag
                PlanPagos.Int_LiqPdo        = Creditos.Int_Corrientes + Creditos.Int_DifCobr.

         IF TODAY GE Creditos.Fec_Pago THEN    /*Ya está vencido totalmente*/
            ASSIGN Creditos.Sdo_Proyectado = 0
                   Creditos.Capital_Acum   = Creditos.Monto
                   Creditos.Int_LiqAcum    = Creditos.Int_Corrientes + Creditos.Int_DifCobro  
                   PlanPagos.Capital_Acum  = Creditos.Monto
                   PlanPagos.Int_LiqAcum   = Creditos.Int_Corrientes + Creditos.Int_DifCobr
                   Creditos.Cuo_Atraso     = 1
                   Creditos.Dias_Atraso    = TODAY - Creditos.Fec_Pago
                   Creditos.Val_Atraso     = Creditos.Sdo_Capital.         
         NEXT.
      END.

      ASSIGN KSdo     = Creditos.Sdo_Capital
             KCuo     = Creditos.Cuota      
             CuoFalt  = Creditos.Plazo
             W_FecTra   = xfecini
             W_FecIni   = xfecini.

      IF (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
          Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
          Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado) LE Creditos.Cuota THEN 
          ASSIGN Creditos.Cuo_Pagadas = Creditos.Plazo - 1
                 CuoFalt              = 1.
      ELSE IF Creditos.Sdo_Capital LT Creditos.Monto THEN DO:
          DO NN = 1 TO Creditos.Plazo:   /*Con Sdo-Capital halla Faltantes*/
             ASSIGN KInt = ROUND(KSdo * W_Tasa,0)
                    KCap = ROUND(KCuo - KInt,0)
                    KSdo = KSdo - KCap.

             IF KSdo LE 0 THEN DO:  
                CuoFalt = NN.       
                LEAVE.
             END.

             CuoFalt = NN.
          END.

          Creditos.Cuo_Pagadas = Creditos.Plazo - CuoFalt.   /*Plazo menos faltantes son pagadas*/
      END.

      DO NN = 1 TO (Creditos.Cuo_Pagadas):  /*Halla Fecha próximo pago con base en Pagadas*/ 
         RUN Halla_FecVcto.R (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                              OUTPUT W_FecTra).  
         Creditos.Fec_Pago = W_FecTra.
      END.

      ASSIGN W_FecTra   = xfecini  
             W_FecIni   = xfecini
             /*KSdo       = Creditos.Sdo_Capital*/
             KSdo       = Creditos.Monto
             KCuo       = Creditos.Cuota
             KIAcu      = 0
             KCAcu      = 0
             PdoTrans   = 0
             KInt       = 0
             KCap       = 0.

      DO NN = 1 TO Creditos.Plazo:    /*Halla la Cuota(Pdo.) que transcurre y Sdo_proy con base Monto*/ 
         ASSIGN KInt  = ROUND(KSdo * W_Tasa,0)
                KCap  = ROUND(KCuo - KInt,0) 
                KIAcu = KIAcu + KInt
                KCAcu = KCAcu + KCap
                KSdo  = KSdo  - KCap
                W_FecIniCont = W_FecTra.

         RUN Halla_FecVcto.R (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                              OUTPUT W_FecTra). 

         IF W_FecTra GE TODAY THEN DO:
            PdoTrans = NN.
            LEAVE.
         END.  

         /*ASSIGN KInt  = ROUND(KSdo * W_Tasa,0)
                KCap  = ROUND(KCuo - KInt,0) 
                KIAcu = KIAcu + KInt
                KCAcu = KCAcu + KCap
                KSdo  = KSdo  - KCap.*/
      END.

      IF PdoTrans LE 0 AND xfecini LT (TODAY - W_DiaPdo) THEN
         ASSIGN PdoTrans = Creditos.Plazo + 1.
      ELSE IF PdoTrans LE 0 THEN
         PdoTrans = 1.

      ASSIGN W_FecTra   = xfecini  
             W_FecIni   = xfecini.             

      IF PdoTrans GT 1 THEN DO NN = 1 TO PdoTrans - 1:     /*Halla la Fec-Ini del que transcurre - 1*/ 
         ASSIGN W_FecIniCont = W_FecTra.

         RUN Halla_FecVcto.R (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                              OUTPUT W_FecTra). 
      END.            

      ASSIGN W_SiPdoTr  = FALSE
             KSdo       = Creditos.Sdo_Capital
             KCuo       = Creditos.Cuota
             W_FecIni   = W_FecIniCont
             W_FecTra   = W_FecIniCont. 

      DO NN = (PdoTrans - 1) TO Creditos.Plazo + 1:  /*Genera los Reg.del PlanPagos*/ 
         RUN CrearPlan.
         ASSIGN PlanPagos.Nro_Cuota = NN                      
                PlanPagos.Fec_Ini   = W_FecTra
                PlanPagos.Fec_Vcto  = W_FecTra.

         IF NN GT 0 THEN DO:
            RUN Halla_FecVcto.R (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                                 OUTPUT W_FecTra).
            PlanPagos.Fec_Vcto = W_FecTra.
         END.

         IF NN LT PdoTrans THEN
            ASSIGN PlanPagos.Id_PdoMes         = 2   /*Ya Cumplido*/
                   Creditos.Capital_Acum       = KCAcu - KCap
                   Creditos.Int_LiqAcum        = KIAcu - KInt
                   PlanPagos.Capital_Acum      = KCAcu - KCap
                   Creditos.Sdo_Proyectado     = Creditos.Monto - Creditos.Capital_Acum
                   PlanPagos.Int_LiqAcum       = KIAcu - KInt
                   PlanPagos.Int_LiqPdo        = KInt
                   PlanPagos.Capital_Pdo       = KCap
                   PlanPagos.Pagos_CapitalPdo  = Creditos.Sdo_CapPag.
         ELSE IF NN EQ PdoTrans THEN DO:
            ASSIGN PlanPagos.Id_PdoMes         = 1   /*Transcurre*/
                   Creditos.Capital_Acum       = KCAcu - KCap
                   Creditos.Sdo_Proyectado     = Creditos.Monto - Creditos.Capital_Acum
                   Creditos.Int_LiqAcum        = KIAcu - KInt
                   PlanPagos.Capital_Acum      = KCAcu - KCap
                   PlanPagos.Int_LiqAcum       = KIAcu - KInt
                   PlanPagos.Int_LiqPdo        = ROUND(((Creditos.Sdo_Proyectado * W_Tasa) / W_DiaPdo)
                                                       * (TODAY - PlanPagos.Fec_Ini),0)
                   /*PlanPagos.Int_LiqPdo        = (KInt / W_DiaPdo) * (TODAY - PlanPagos.Fec_Ini + 1)*/
                   PlanPagos.Capital_Pdo       = 0.
            IF NN EQ Creditos.Cuo_Pagadas AND Creditos.Sdo_Proyectado LE Creditos.Sdo_Capital THEN
               ASSIGN Creditos.Cuo_Pagadas  = Creditos.Cuo_Pagadas - 1
                      Creditos.Fec_Pago     = PlanPagos.Fec_Ini
                      PlanPagos.Fec_ProxPag = Creditos.Fec_Pago.
            ELSE IF  NN GT Creditos.Cuo_Pagadas AND Creditos.Sdo_Proyectado LE Creditos.Sdo_Capital
                 AND (Creditos.Sdo_Capital - Creditos.Sdo_Proyectado) LT Creditos.Cuota THEN
               ASSIGN Creditos.Cuo_Pagadas  = Creditos.Cuo_Pagadas + 1
                      Creditos.Fec_Pago     = PlanPagos.Fec_Inic 
                      PlanPagos.Fec_ProxPag = Creditos.Fec_Pago.
         END.

         IF NN LE PdoTrans THEN
            ASSIGN PlanPagos.Fec_ProxPag       = Creditos.Fec_Pago
                   PlanPagos.Cuo_Pagas         = Creditos.Cuo_Pagadas
                   PlanPagos.Pagos_CapitalAcum = Creditos.Sdo_CapPag.                   
      END.

      FIND LAST PlanPagos WHERE PlanPagos.Agencia         EQ Creditos.Agencia    
                               AND PlanPagos.Nit          EQ Creditos.Nit                                    
                               AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                            
                               AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                            
                               AND PlanPagos.Id_PdoMes    EQ 1   /*Transc.*/  NO-ERROR.          
      IF NOT AVAIL(PlanPagos) THEN DO:
         RUN CrearPlan.                        
         ASSIGN PlanPagos.Id_PdoMes     = 1
                PlanPagos.Nro_Cuota     = Creditos.Plazo + 1       
                PlanPagos.Fec_Ini       = TODAY
                PlanPagos.Fec_Vcto      = Creditos.Fec_Pago
                PlanPagos.Fec_ProxPago  = Creditos.Fec_Pago
                Creditos.Sdo_Proyectado = 0.
      END.

      ASSIGN Creditos.Sdo_IntPag     = Creditos.Int_LiqAcum - (Creditos.Int_Corrientes + Creditos.Int_DifCobro)
             PlanPagos.Pagos_IntAcum = Creditos.Sdo_IntPag.

      IF PlanPagos.Nro_Cuota EQ Creditos.Cuo_Pagadas AND Creditos.Sdo_Capital GT Creditos.Sdo_Proyectado THEN
         ASSIGN Creditos.Cuo_Pagadas = Creditos.Cuo_Pagadas - 1
                PlanPagos.Cuo_Pagas  = Creditos.Cuo_Pagadas.

      IF PlanPagos.Nro_Cuota GT Creditos.Plazo THEN
         ASSIGN Creditos.Sdo_Proyectado = 0
                Creditos.Capital_Acum   = Creditos.Monto
                Creditos.Cuo_Atraso     = Creditos.Plazo - Creditos.Cuo_Pagadas
                PlanPagos.Capital_Acum  = Creditos.Monto.
      ELSE IF PlanPagos.Nro_Cuota - 1 LE Creditos.Cuo_Pagadas THEN
         Creditos.Cuo_Atraso = 0.
      ELSE IF PlanPagos.Nro_Cuota - 1 GT Creditos.Cuo_Pagadas THEN
         Creditos.Cuo_Atraso = (PlanPagos.Nro_Cuota - 1) - Creditos.Cuo_Pagadas.

      ASSIGN Creditos.Val_Atraso = Creditos.Sdo_Capital - Creditos.Sdo_Proyectado WHEN
                                   Creditos.Sdo_Capital GT Creditos.Sdo_Proyectado.

      ASSIGN Creditos.Dias_Atraso = TODAY - Creditos.Fec_Pago WHEN 
                                    TODAY GT Creditos.Fec_Pago.

  END.
END.
SESSION:SET-WAIT-STATE("").

MESSAGE "Generó PlanPagos: " TotReg
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

  /*------------------------*/
  PROCEDURE CrearPlan:
     CREATE PlanPagos.
     ASSIGN PlanPagos.Agencia      = Creditos.Agencia          
            PlanPAgos.Nit          = Creditos.Nit              
            PlanPagos.Num_Credito  = Creditos.Num_credito          
            PlanPagos.Pagare       = Creditos.Pagare        
            PlanPagos.Cod_Credito  = Creditos.Cod_Credito           
            PlanPagos.Tip_Credito  = Creditos.Tip_Credito
            PlanPagos.Cuota        = Creditos.Cuota            
            PlanPagos.Tasa         = Creditos.Tasa             
            PlanPagos.Plazo        = Creditos.Plazo            
            PlanPagos.Monto_Actual = Creditos.Monto            
            PlanPagos.Id_PdoMes    = 0                 /*Inicia en futuro*/
            TotReg = TotReg + 1.
  END PROCE.
