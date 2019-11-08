/*-----------------------------
Programa  Halla_PagasFalt.P ... Halla Cuo-faltan con Sdo-Capital para hallar las Pagadas 
-----------------------------------------------------------------------------------*/
   DEFI INPUT PARAM W_RowidCr AS ROWID.
   DEFI INPUT PARAM W_RowidPP AS ROWID.
   DEFI INPUT PARAM W_TasaUS  LIKE Creditos.Tasa.

   DEFI VAR W_Tasa    LIKE Creditos.Tasa.
   DEFI VAR KSdo      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KInt      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KCap      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KCuo      LIKE Creditos.Cuota.              
   DEFI VAR NN           AS INTEG FORM "99999".
   DEFI VAR W_PdoLiq     AS INTEG FORM "99" INIT 12.
   DEFI VAR PdoTrans     AS INTEG FORM "99999".
   DEFI VAR CuoFalt      AS INTEG FORM "99999".
   DEFI VAR W_SiOKCuoPag AS LOG INIT FALSE.
   DEFI VAR PTrans1      LIKE PlanPagos.Nro_Cuota.

   FIND Creditos  WHERE ROWID(Creditos)  EQ W_RowidCr NO-ERROR.
   IF AVAIL(Creditos) THEN
      FIND PlanPagos WHERE ROWID(PlanPagos) EQ W_RowidPP NO-ERROR.

   IF NOT AVAIL(Creditos) OR NOT AVAIL(PlanPagos) THEN DO:
      MESSAGE "Programa Halla_PagasFalt.P, no halló el Crédito o no Halló el PlanPagos" SKIP
              "Programa Retorna Error."
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
  
   IF Creditos.Per_Pago EQ 1 THEN
      W_PdoLiq = 52.
   ELSE IF Creditos.Per_Pago EQ 2 THEN
      W_PdoLiq = 36.
   ELSE IF Creditos.Per_Pago EQ 3 THEN
      W_PdoLiq = 24.

   IF Creditos.Tasa GT W_TasaUS THEN   
       W_Tasa  = W_TasaUS / (W_PdoLiq * 100).
    ELSE
       W_Tasa  = Creditos.Tasa / (W_PdoLiq * 100). 

   IF Creditos.Sistema         EQ 2 
   OR Creditos.Plazo           EQ 1
   OR (Creditos.Sdo_Capital GE Creditos.Monto) THEN DO:
      ASSIGN Creditos.Cuo_Pagadas = 0.

      FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia       
                       AND PlanPagos.Nit          EQ Creditos.Nit            
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito    
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito  
                       AND PlanPagos.Id_PdoMes    LE 2    /*Puede ser ya cumplida o Transc.*/
                       AND PlanPagos.Nro_Cuota    EQ 1 NO-LOCK NO-ERROR.     /*Debe desde la 1a.*/

      ASSIGN Creditos.Fec_Pago = PlanPagos.Fec_Vcto WHEN AVAIL(PlanPagos).     
   END.
   ELSE IF (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
            Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
            Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado) LE Creditos.Cuota THEN DO:
      ASSIGN Creditos.Cuo_Pagadas = Creditos.Plazo - 1.

      FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia       
                       AND PlanPagos.Nit          EQ Creditos.Nit            
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito    
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito  
                       AND PlanPagos.Id_PdoMes    LE 2
                       AND PlanPagos.Nro_Cuota    EQ Creditos.Plazo NO-LOCK NO-ERROR.  /*Debe la Última*/

      ASSIGN Creditos.Fec_Pago = PlanPagos.Fec_Vcto WHEN AVAIL(PlanPagos).     
   END.
   ELSE DO:
      ASSIGN KSdo     = Creditos.Sdo_Capital - (Creditos.Cuota * .06)     /*Condición nueva: - (Creditos.Cuota * .06)*/      
             KCuo     = Creditos.Cuota                                                /*Margen Nov.24/06*/
             PdoTrans = PlanPagos.Nro_Cuota
             W_SiOKCuoPag = FALSE
             CuoFalt      = Creditos.Plazo.

      /*Para todos halla Faltantes*/           
      IF NOT W_SiOKCuoPag THEN DO:
         DO NN = 1 TO Creditos.Plazo:
            FIND LAST PlanPagos WHERE PlanPagos.Agencia   EQ Creditos.Agencia    
                               AND PlanPagos.Nit          EQ Creditos.Nit                                    
                               AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                            
                               AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                            
                               AND PlanPagos.Id_PdoMes    LE 2   /*Cumplid,Transc.y Futuras*/            
                               AND PlanPagos.Nro_Cuota    EQ NN NO-LOCK NO-ERROR.   /*Con Cuota GE 0*/  
            IF AVAIL(PlanPagos) AND PlanPagos.Cuota GE Creditos.Cuota THEN
               ASSIGN KCuo = PlanPagos.Cuota.
            ELSE 
               KCuo = Creditos.Cuota.

            ASSIGN KInt = ROUND(KSdo * W_Tasa,0)
                   KCap = ROUND(KCuo - KInt,0)
                   KSdo = KSdo - KCap.

            IF KSdo LE 0 THEN DO:  
               CuoFalt = NN.       
               LEAVE.
            END.

            CuoFalt = NN.
         END.         
                  
         ASSIGN Creditos.Cuo_pagadas = Creditos.Plazo - CuoFalt.   /*Plazo menos faltantes son pagadas*/
      END.
   END.

   /*Para hallar Fec-Vcto del Proximo Pago*/
   FIND LAST PlanPagos WHERE PlanPagos.Agencia            EQ Creditos.Agencia    
                               AND PlanPagos.Nit          EQ Creditos.Nit                                    
                               AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                            
                               AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                            
                               AND PlanPagos.Id_PdoMes    LE 2   /*Cumplid,Transc.y Futuras*/            
                               AND PlanPagos.Nro_Cuota    EQ Creditos.Cuo_pagadas + 1 NO-ERROR.
   IF AVAIL(PlanPagos) THEN
      ASSIGN Creditos.Fec_Pago      = PlanPagos.Fec_Vcto
             PlanPagos.Fec_ProxPago = Creditos.Fec_Pago
             PlanPagos.Cuo_Pagas    = Creditos.Cuo_Pagadas.

   IF AVAIL(PlanPagos) AND PlanPagos.Id_PdoMes NE 1 THEN DO:
      FIND PlanPagos WHERE ROWID(PlanPagos) EQ W_RowidPP NO-ERROR.
      ASSIGN PlanPagos.Fec_ProxPago = Creditos.Fec_Pago
             PlanPagos.Cuo_Pagas    = Creditos.Cuo_pagadas.
   END.

   ASSIGN Creditos.Cuo_Atraso  = 0
          Creditos.Val_Atraso  = 0
          Creditos.Dias_Atraso = 0
          KSdo                 = Creditos.Monto
          PTrans1              = PlanPagos.Nro_Cuota.

   IF Creditos.Cuo_Pagadas + 1 LT PlanPagos.Nro_Cuota THEN DO:
      ASSIGN Creditos.Cuo_Atraso  = PlanPagos.Nro_Cuota - (Creditos.Cuo_pagadas + 1)
             Creditos.Dias_Atraso = TODAY - Creditos.Fec_Pago
             Creditos.Val_Atraso  = Creditos.Sdo_Capital - Creditos.Sdo_Proyect 
             PTrans1              = PlanPagos.Nro_Cuota.

      IF Creditos.Cuo_Atraso  LT 0 THEN
         Creditos.Cuo_Atraso  = 0.
      IF Creditos.Dias_Atraso LT 0 THEN
         Creditos.Dias_Atraso = 0.
      IF Creditos.Val_Atraso  LT 0 THEN
         Creditos.Val_Atraso  = 0.

      /*RUN Halla_Proyec.
      IF KSdo LE 0 THEN
         KSdo = 0.   Comentariado Nov.25/06 no debe recalcular proyectado

      FIND PlanPagos WHERE ROWID(PlanPagos) EQ W_RowidPP NO-ERROR.
      ASSIGN Creditos.Sdo_Proyect   = KSdo
             Creditos.Capital_Acum  = Creditos.Monto - Creditos.Sdo_Proyect
             PlanPagos.Capital_Acum = Creditos.Monto - Creditos.Sdo_Proyect
             Creditos.Val_Atraso    = Creditos.Sdo_Capital - Creditos.Sdo_Proyect.*/            
   END.
   ELSE IF Creditos.Sdo_Proyect LT Creditos.Sdo_Capital AND PlanPagos.Nro_Cuota LE Creditos.Plazo THEN DO:
          /*Debe quedar sin Cap.Vdo, Porque está al Día o Adelantado*/  
         IF PlanPagos.Cuota GE Creditos.Cuota THEN
            ASSIGN KCuo = PlanPagos.Cuota.
         ELSE 
            KCuo = Creditos.Cuota.

         ASSIGN Creditos.Sdo_Proyect   = Creditos.Sdo_Capital       /*Nov.25/06 Nuevo Proyectado al Día */
                Creditos.Capital_Acum  = Creditos.Monto - Creditos.Sdo_Proyect
                PlanPagos.Capital_acum = Creditos.Monto - Creditos.Sdo_Proyect
                PlanPagos.Fec_ProxPago = Creditos.Fec_Pago
                PlanPagos.Cuo_Pagas    = Creditos.Cuo_Pagadas.   

         IF Creditos.Cuo_Pagadas GE PlanPagos.Nro_Cuota THEN 
            ASSIGN KInt = ROUND(Creditos.Sdo_Capital * W_Tasa,0)                                                          
                   KCap = ROUND(KCuo - KInt,0)
                   Creditos.Sdo_Proyect   = Creditos.Sdo_Capital + KCap      /*Nuevo Proyectado Superior*/
                   Creditos.Capital_Acum  = Creditos.Monto - Creditos.Sdo_Proyect                  
                   PlanPagos.Capital_acum = Creditos.Monto - Creditos.Sdo_Proyect                  
                   PlanPagos.Fec_ProxPago = Creditos.Fec_Pago                                      
                   PlanPagos.Cuo_Pagas    = Creditos.Cuo_Pagadas. 
   END.
   
  /*----------------------*/
  PROCEDURE Halla_Proyec:             /*Con tasa Credito*/
    ASSIGN KSdo    = Creditos.Monto
           W_Tasa  = Creditos.Tasa / (W_PdoLiq * 100).

    DO NN = 1 TO PTrans1 - 1:                                                                     
       FIND LAST PlanPagos WHERE PlanPagos.Agencia   EQ Creditos.Agencia                             
                          AND PlanPagos.Nit          EQ Creditos.Nit                                 
                          AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                         
                          AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                         
                          AND PlanPagos.Id_PdoMes    EQ 2   /*Cumplid,*/             
                          AND PlanPagos.Nro_Cuota    EQ NN NO-LOCK NO-ERROR.   /*Con Cuota GE 0*/    
       IF AVAIL(PlanPagos) AND PlanPagos.Cuota GE Creditos.Cuota THEN                                
          ASSIGN KCuo = PlanPagos.Cuota.                                                             
       ELSE                                                                                          
          KCuo = Creditos.Cuota.                                                                     
                                                                                                     
       ASSIGN KInt = ROUND(KSdo * W_Tasa,0)                                                          
              KCap = ROUND(KCuo - KInt,0)                                                            
              KSdo = KSdo - KCap.                                                                    
                                                                                                     
       IF KSdo LE 0 THEN                                                                     
          LEAVE.                                                                                     
    END.                                                                                                                                                                                                 
 END PROCE.
   

