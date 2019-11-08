
/*------------------------------------------------------------------------------
  Purpose:  El crèdito se reinicia con las nuevas condiciones, y en PlanPagos.
 ------------------------------------------------------------------------------*/
  DEFI VAR W_ContCuota  LIKE PlanPagos.Nro_Cuota INIT 0.
  DEFI VAR W_NCuotas    AS   INTEG FORM "-99999" INIT 0.
  DEFI VAR I            AS   INTEG FORM "9999".
  DEFI VAR W_DiasTrans  AS   INTEG FORM "9999".
  DEFI VAR W_DiasPdo    AS   INTEG FORM "9999".
  DEFI VAR W_NvoPdo     AS   INTEG FORM "9999". 
  DEFI VAR W_IntLiqPdo  LIKE PlanPagos.Int_LiqPdo INIT 0.
  DEFI VAR W_FecTran    AS DATE.
  DEFI VAR W_FecVcto    AS DATE.
  DEFI VAR Cont AS INTEG FORM "9999999" EXTENT 2 INIT 0.
  
  /*FOR EACH TPlanP. DELETE TPlanP. END.*/

  DEFINE VARIABLE PlazoW     LIKE Solicitud.Plazo.
  DEFINE VARIABLE TotPtW     LIKE Solicitud.Monto.
  DEFINE VARIABLE CuotaW     LIKE Solicitud.Cuota.
  DEFI   VAR      Cop_CuotaW LIKE Solicitud.Cuota.
  DEFINE VARIABLE TasaW      LIKE Solicitud.Tasa.
  DEFINE VARIABLE TinteW     LIKE Solicitud.Monto.

  DEFI   VAR W_CuoPla    AS INTEG FORM "9".
  DEFI   VAR NPdo        LIKE Creditos.Per_Pago.

  DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
  DEFINE VAR Wimp_Coop   AS INTEGER INITIAL 0. 

  {INCLUIDO\VARIABLE.I "SHARED"}

  FOR EACH Creditos WHERE Creditos.Tip_Credito NE 4
      AND Creditos.Estado EQ 2 AND Sdo_Capital GT 0:
      IF Creditos.Sistema NE 1 THEN DO:
         MESSAGE "Credito con sistema diferente a cuota-fija : " Creditos.Nit Creditos.Num_credito SKIP
             "Sistema : "  Creditos.Sistema
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
         NEXT.
      END.

      ASSIGN TotPtW   = Creditos.Sdo_Capital
             CuotaW   = Creditos.Cuota
             W_CuoPla = 2                     /*Halla plazo faltante*/
             TasaW    = Creditos.Tasa
             NPdo     = Creditos.Per_Pago
             PlazoW   = 0
             W_NvoPdo  = 12
             W_DiasPdo = 30
             TInteW    = 0
          Cont[1] = Cont[1] + 1.

      IF      Creditos.Per_Pago EQ 1 THEN 
        ASSIGN W_NvoPdo = 52
               W_DiasPdo = 7.
      ELSE IF Creditos.Per_Pago EQ 2 THEN 
        ASSIGN W_NvoPdo  = 36
               W_DiasPdo = 10.
      ELSE IF Creditos.Per_Pago EQ 3 THEN 
        ASSIGN W_NvoPdo  = 24
               W_DiasPdo = 15.
                                   
      RUN NVEF IN W_ManFin (INPUT TasaW / (W_NvoPdo * 100),INPUT W_NvoPdo,OUTPUT TasaW).       
     
      TasaW = TasaW * 100.        
     
      RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw, INPUT-OUTPUT PlazoW, INPUT-OUTPUT CuotaW,            
                           INPUT-OUTPUT TInteW, INPUT-OUTPUT TasaW, INPUT 0,                              
                           INPUT 0,INPUT NPdo,  INPUT W_CuoPla,                                      
                           INPUT 1,                                                    
                           INPUT Creditos.Sistema). 

      IF PlazoW LE 0 THEN DO:                                                                        
         MESSAGE "El Plazo Restante debe ser mayor a cero. Rectifique!" SKIP
                 "Credito : " Creditos.Nit Creditos.Num_credito 
             VIEW-AS ALERT-BOX ERROR.                                                           
         NEXT.                                                                               
      END. 

      ASSIGN Creditos.Cuo_Pagadas  = Creditos.Plazo - PlazoW
             W_NCuotas             = Creditos.Cuo_Pagadas - 1
             Creditos.Val_Desemb   = Creditos.Monto
            /* Creditos.Monto        = Creditos.Sdo_Capital*/
             Creditos.Sdo_Proyecta = Creditos.Sdo_Capital
             Creditos.Capital_Acum = Creditos.Monto - Creditos.Sdo_Capital           
             Creditos.Sdo_CapPag   = Creditos.Monto - Creditos.Sdo_Capital         
             Creditos.Sdo_IntMor   = 0
             Creditos.Val_Atraso   = 0
             Creditos.Cuo_Atraso   = 0
             Creditos.Dias_Atraso  = 0
             Creditos.Int_LiqAcum  = 0
             Creditos.Sdo_IntPag   = Creditos.Int_Anticipado.  

      IF Creditos.Cuo_Pagadas GT 0 THEN
         Creditos.Cuo_Pagadas = Creditos.Cuo_Pagadas - 1.

    /*  ASSIGN PlanPagos.Id_PdoMes   = 4   /*Cumplidos PP-Anterior*/
             W_FecControl          = PlanPagos.Fec_Vcto.*/

      FOR EACH PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                           AND PlanPagos.Nit          EQ Creditos.Nit        
                           AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                           AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito:
          IF PlanPagos.Id_PdoMes EQ 0 THEN
             DELETE PlanPagos.
          ELSE IF PlanPagos.Id_PdoMes EQ 1 THEN
             ASSIGN PlanPagos.Id_PdoMes = 4            /*Queda no vigente*/
                    W_IntLiqPdo = PlanPagos.Int_LiqPdo.
          ELSE IF PlanPagos.Fec_Vcto GE DATE(3,1,2006) THEN 
             PlanPagos.Id_PdoMes = 4.            /*Queda no vigente*/
          ELSE DELETE PlanPagos.
      END.

      IF W_NCuotas LT 0 THEN
         W_NCuotas = 0.
      
      IF Creditos.FOR_Pago EQ 2 THEN DO: 
         ASSIGN Creditos.Fec_Pago = DATE(4,30,2006)
                W_FecTran         = DATE(3,30,2006).
         IF Creditos.Per_Pago EQ 3 THEN
            W_FecTran = DATE(4,15,2006).
      END.
      ELSE DO:
         IF DAY(Creditos.Fec_Desemb) EQ 31 THEN
            Creditos.Fec_Desemb = Creditos.Fec_Desemb - 1.

         IF DAY(Creditos.Fec_Desemb) LT 23 THEN
            ASSIGN Creditos.Fec_Pago = DATE(5,DAY(Creditos.Fec_Desemb),2006)
                   W_FecTran         = DATE(4,DAY(Creditos.Fec_Desemb),2006).
         ELSE 
            ASSIGN Creditos.Fec_Pago = DATE(4,DAY(Creditos.Fec_Desemb),2006)
                   W_FecTran         = DATE(3,DAY(Creditos.Fec_Desemb),2006).

         IF Creditos.Per_Pago EQ 3 AND DAY(Creditos.Fec_Desemb) LE 15 THEN
            ASSIGN Creditos.Fec_Pago = DATE(4,DAY(Creditos.Fec_Desemb) + 15,2006)
                   W_FecTran         = DATE(4,DAY(Creditos.Fec_Desemb),2006).
         ELSE IF Creditos.Per_Pago EQ 3 THEN 
            ASSIGN Creditos.Fec_Pago = DATE(4,DAY(Creditos.Fec_Desemb),2006)
                   W_FecTran         = DATE(4,DAY(Creditos.Fec_Desemb)- 15,2006).
      END.

      CREATE PlanPagos.                                                                            
      ASSIGN PlanPagos.Agencia     = Creditos.Agencia
             PlanPagos.Cod_Credito = Creditos.Cod_Credito
             PlanPagos.Num_Credito = Creditos.Num_Credito
             PlanPagos.Pagare      = Creditos.Pagare     
             PlanPagos.Nit         = Creditos.Nit        
             PlanPagos.Tip_Credito = Creditos.Tip_Credito
             PlanPagos.Id_PdoMes = 2            /*El 1o.Ya transcurrido */                         
             PlanPagos.Nro_Cuota = W_NCuotas                                                               
             PlanPagos.Plazo     = Creditos.Plazo                                                  
             PlanPagos.Cuota     = Creditos.Cuota                                                  
             PlanPagos.Tasa      = Creditos.Tasa
             PlanPagos.Fec_Inic  = W_FecTran - W_DiasPdo
             PlanPagos.Fec_Vcto  = W_FecTran
             PlanPagos.Cargos_Pdo        = 0                                                       
             PlanPagos.Cargos_Acum       = Creditos.Costas + Creditos.Honorarios + Creditos.Polizas
             PlanPagos.Pagos_OtrosPdo    = 0                                                       
             PlanPagos.Pagos_OtrosAcum   = 0                                                       
             PlanPagos.Int_MoraPdo       = 0                                                       
             PlanPagos.Int_MoraAcum      = Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar        
             PlanPagos.Pagos_MoraPdo     = 0                                                       
             PlanPagos.Pagos_MoraAcum    = 0
             Cont[2] = Cont[2] + 1.
                                                                                                   
      ASSIGN PlanPagos.Int_LiqPdo        = W_IntLiqPdo                                                   
             PlanPagos.Int_LiqAcum       = 0                                                       
             PlanPagos.Pagos_IntPdo      = 0                                                       
             PlanPagos.Pagos_IntAcum     = Creditos.Int_Anticipado                                 
             PlanPagos.Capital_Pdo       = 0                                                       
             PlanPagos.Capital_Acum      = Creditos.Monto - Creditos.Sdo_Capital                   
             PlanPagos.Pagos_CapitalPdo  = 0                                                       
             PlanPagos.Pagos_CapitalAcum = Creditos.Monto - Creditos.Sdo_Capital                   
             PlanPagos.Fec_ProxPago      = Creditos.Fec_Pago                                       
             PlanPagos.Monto_Actual      = Creditos.Monto                                          
             PlanPagos.Cuo_Pagas         = Creditos.Cuo_Pagadas.      

      CREATE PlanPagos.                                                                             
      ASSIGN PlanPagos.Agencia     = Creditos.Agencia     
             PlanPagos.Cod_Credito = Creditos.Cod_Credito 
             PlanPagos.Num_Credito = Creditos.Num_Credito 
             PlanPagos.Pagare      = Creditos.Pagare      
             PlanPagos.Nit         = Creditos.Nit         
             PlanPagos.Tip_Credito = Creditos.Tip_Credito 
             PlanPagos.Id_PdoMes = 1            /*El que transcurre */                          
             PlanPagos.Nro_Cuota = W_NCuotas + 1                                                       
             PlanPagos.Plazo     = Creditos.Plazo                                                   
             PlanPagos.Cuota     = Creditos.Cuota                                                   
             PlanPagos.Tasa      = Creditos.Tasa  
             PlanPagos.Fec_Inic  = W_FecTran
             PlanPagos.Fec_Vcto  = Creditos.Fec_Pago
             PlanPagos.Cargos_Pdo        = 0                                                        
             PlanPagos.Cargos_Acum       = Creditos.Costas + Creditos.Honorarios + Creditos.Polizas 
             PlanPagos.Pagos_OtrosPdo    = 0                                                        
             PlanPagos.Pagos_OtrosAcum   = 0                                                        
             PlanPagos.Int_MoraPdo       = 0                                                        
             PlanPagos.Int_MoraAcum      = Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar         
             PlanPagos.Pagos_MoraPdo     = 0                                                        
             PlanPagos.Pagos_MoraAcum    = 0
             Cont[2] = Cont[2] + 1.
                                                                                                    
      ASSIGN PlanPagos.Int_LiqPdo        = W_IntLiqPdo                                                   
             PlanPagos.Int_LiqAcum       = 0                                                        
             PlanPagos.Pagos_IntPdo      = 0                                                        
             PlanPagos.Pagos_IntAcum     = Creditos.Int_Anticipado                                  
             PlanPagos.Capital_Pdo       = 0                                                        
             PlanPagos.Capital_Acum      = Creditos.Monto - Creditos.Sdo_Capital                    
             PlanPagos.Pagos_CapitalPdo  = 0                                                        
             PlanPagos.Pagos_CapitalAcum = Creditos.Monto - Creditos.Sdo_Capital                    
             PlanPagos.Fec_ProxPago      = Creditos.Fec_Pago                                        
             PlanPagos.Monto_Actual      = Creditos.Monto                                           
             PlanPagos.Cuo_Pagas         = Creditos.Cuo_Pagadas
             W_FecTran = Creditos.Fec_Pago.                                    
                                                                                                    
      DO I = W_NCuotas + 2 TO Creditos.Plazo:
         RUN Halla_FecVcto.R (INPUT  Creditos.Fec_Pago,W_DiasPdo,W_FecTran,   
                              OUTPUT W_FecVcto).
         CREATE PlanPagos.
         ASSIGN PlanPagos.Agencia     = Creditos.Agencia     
                PlanPagos.Cod_Credito = Creditos.Cod_Credito 
                PlanPagos.Num_Credito = Creditos.Num_Credito 
                PlanPagos.Pagare      = Creditos.Pagare      
                PlanPagos.Nit         = Creditos.Nit         
                PlanPagos.Tip_Credito = Creditos.Tip_Credito 
                PlanPagos.Id_PdoMes = 0            /*los futuros */                        
                PlanPagos.Nro_Cuota = I                                                                
                PlanPagos.Plazo     = Creditos.Plazo                                                      
                PlanPagos.Cuota     = Creditos.Cuota                                                      
                PlanPagos.Tasa      = Creditos.Tasa    
                PlanPagos.Fec_Inic  = W_FecTran
                PlanPagos.Fec_Vcto  = W_FecVcto
                W_FecTran           = W_FecVcto         /*Para hallar la Pròxima*/
                PlanPagos.Cargos_Pdo        = 0                                                        
                PlanPagos.Cargos_Acum       = 0 
                PlanPagos.Pagos_OtrosPdo    = 0                                                        
                PlanPagos.Pagos_OtrosAcum   = 0                                                        
                PlanPagos.Int_MoraPdo       = 0                                                        
                PlanPagos.Int_MoraAcum      = 0         
                PlanPagos.Pagos_MoraPdo     = 0                                                        
                PlanPagos.Pagos_MoraAcum    = 0
                Cont[2] = Cont[2] + 1.
                                                                                                       
         ASSIGN PlanPagos.Int_LiqPdo        = 0
                PlanPagos.Int_LiqAcum       = 0                                                        
                PlanPagos.Pagos_IntPdo      = 0                                                        
                PlanPagos.Pagos_IntAcum     = 0                                 
                PlanPagos.Capital_Pdo       = 0                                                        
                PlanPagos.Capital_Acum      = 0                                                        
                PlanPagos.Pagos_CapitalPdo  = 0                                                        
                PlanPagos.Pagos_CapitalAcum = 0                                                        
                /*PlanPagos.Fec_ProxPago  =  */
                PlanPagos.Monto_Actual      = Creditos.Monto                                     
                PlanPagos.Cuo_Pagas         = 0.                                                                                                                            
      END.
  END.

  MESSAGE "Totales :" Cont[1] Cont[2]
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
      
 
