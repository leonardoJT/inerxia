   DEFI VAR W_Tasa    LIKE Creditos.Tasa.
   DEFI VAR KSdo      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KInt      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KCap      LIKE Creditos.Sdo_Capital INIT 0. 
   DEFI VAR KCuo      LIKE Creditos.Cuota.              
   DEFI VAR NN           AS INTEG FORM "99999".
   DEFI VAR W_PdoLiq     AS INTEG FORM "99" INIT 12.
   DEFI VAR PdoTrans     AS INTEG FORM "99999".
   DEFI VAR DiaPer       AS INTEG FORM "99999".
   DEFI VAR CuoFalt      AS INTEG FORM "99999".
   DEFI VAR W_SiOKCuoPag AS LOG INIT FALSE.
   DEFI VAR PTrans1      LIKE PlanPagos.Nro_Cuota.
   DEFI VAR W_TasaUS     LIKE Creditos.Tasa.
   DEFI VAR Cont AS INTEG FORM "999999".
   DEFI VAR ErrTransc AS LOG INIT FALSE.
   DEFI VAR W_RowIdPPT AS ROWID.
   DEFI VAR W_RowIdCre AS ROWID.
   DEFI VAR FPOk       LIKE PlanPagos.Fec_Vcto.


   W_TasaUS = 20.56.
        
OUTPUT TO C:\InfRed\Incons_QuedanFuturCred271106.Txt.
FOR EACH creditos WHERE estado EQ 2 AND sistema EQ 1 AND sdo_capit GT 0 
                BREAK BY Creditos.Agencia BY Creditos.Nit:
    FIND LAST PlanPagos WHERE PlanPagos.Agencia   EQ Creditos.Agencia       
                       AND PlanPagos.Nit          EQ Creditos.Nit            
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito    
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito  
                       AND PlanPagos.Id_PdoMes    EQ 1 NO-LOCK NO-ERROR.
    ASSIGN W_PdoLiq = 12
           DiaPer   = 30.
           ErrTransc = FALSE.
    IF Creditos.Per_Pago EQ 1 THEN
       ASSIGN DiaPer   = 7
              W_PdoLiq = 52.
    ELSE IF Creditos.Per_Pago EQ 2 THEN
       ASSIGN DiaPer   = 10
              W_PdoLiq = 36.
    ELSE IF Creditos.Per_Pago EQ 3 THEN
       ASSIGN DiaPer   = 15
              W_PdoLiq = 24.

    IF PlanPagos.Fec_Vcto GT (TODAY + DiaPer) THEN
       ErrTransc = TRUE.

    IF Creditos.Tasa GT W_TasaUS THEN   
       W_Tasa  = W_TasaUS / (W_PdoLiq * 100).
    ELSE
       W_Tasa  = Creditos.Tasa / (W_PdoLiq * 100). 

    IF Creditos.Sistema         EQ 2 
    OR Creditos.Plazo           EQ 1
    OR PlanPagos.Nro_Cuota      GT Creditos.Plazo
    OR (Creditos.Sdo_Capital    GE Creditos.Monto) THEN.
    ELSE DO:
       ASSIGN KSdo     = Creditos.Sdo_Capital - (Creditos.Cuota * .06)
              KCuo     = Creditos.Cuota
              PdoTrans = PlanPagos.Nro_Cuota
              CuoFalt  = Creditos.Plazo.

       DO NN = 1 TO Creditos.Plazo:
          FIND LAST PlanPagos WHERE PlanPagos.Agencia     EQ Creditos.Agencia    
                               AND PlanPagos.Nit          EQ Creditos.Nit                                    
                               AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                            
                               AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                            
                               AND PlanPagos.Id_PdoMes    LE 2   /*Cumplid,Transc.y Futuras*/            
                               AND PlanPagos.Nro_Cuota    EQ NN NO-LOCK NO-ERROR.    
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

       FIND LAST PlanPagos WHERE PlanPagos.Agencia        EQ Creditos.Agencia    
                               AND PlanPagos.Nit          EQ Creditos.Nit                                    
                               AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                            
                               AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                            
                               AND PlanPagos.Id_PdoMes    LE 2   /*Cumplid,Transc.y Futuras*/            
                               AND PlanPagos.Nro_Cuota    EQ (Creditos.Plazo - CuoFalt) + 1 NO-LOCK NO-ERROR.
       IF NOT AVAIL(PlanPagos) THEN
          FIND LAST PlanPagos WHERE PlanPagos.Agencia   EQ Creditos.Agencia       
                       AND PlanPagos.Nit                EQ Creditos.Nit            
                       AND PlanPagos.Cod_Credito        EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito        EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes          EQ 1 NO-LOCK NO-ERROR. 

       IF Creditos.Fec_Pago    LT PlanPagos.Fec_Vcto
       OR Creditos.Cuo_Pagadas LT (Creditos.Plazo - CuoFalt) 
       OR ErrTransc THEN DO:
          DISPLAY Creditos.Agencia            LABEL "Ag."
                  Creditos.Nit                LABEL "Ced./Nit"
                  Creditos.Cod_Credito        LABEL "Pdto"
                  Creditos.Num_Credito        LABEL "Num-Crédito"
                  Creditos.Cuo_Pagadas        LABEL "CPagasAct"
                 (Creditos.Plazo - CuoFalt)   LABEL "CPagasR.OK"
                  Creditos.Fec_Pago           LABEL "FPX-PagoAct"
                  PlanPagos.Fec_Vcto          LABEL "FV-Recal.OK"
                  ErrTransc                   LABEL "ErrPP" WHEN ErrTransc 
                  "ErrVF"                     LABEL "ErrVF" WHEN (Creditos.Val_Atraso GT 0 AND Creditos.Fec_Pago GE TODAY)
                  Creditos.Sdo_Capital        LABEL "Sdo de Capital" FORM "->>>>>>,>>>,>>9"
                  Creditos.Cuota              LABEL "Vlr.de Cuota"   FORM "->>>>>>,>>>,>>9"
                  Creditos.Val_Atraso         LABEL "ValK-Vencido"   FORM "->>>>>>,>>>,>>9"
                  Creditos.Cuo_Atraso         LABEL "CuoVdas"
                  Creditos.For_Pago           LABEL "FP"  FORM "9"
                  Creditos.Per_Pago           LABEL "PP"  FORM "99"
                  Creditos.Fec_UltPag         LABEL "F-UltPago"
             WITH DOWN WIDTH 200 FRAME FCred NO-BOX NO-LABELS USE-TEXT STREAM-IO.

           ASSIGN cont = cont + 1
                    FPOk = PlanPagos.Fec_Vcto.

          /* FIND LAST PlanPagos WHERE PlanPagos.Agencia                   EQ Creditos.Agencia       
                                          AND PlanPagos.Nit                EQ Creditos.Nit            
                                          AND PlanPagos.Cod_Credito        EQ Creditos.Cod_Credito
                                          AND PlanPagos.Num_Credito        EQ Creditos.Num_Credito
                                          AND PlanPagos.Id_PdoMes          EQ 1 NO-ERROR.

           IF NOT ErrTransc AND PlanPagos.Nro_Cuota LE Creditos.Plazo
           AND Creditos.Fec_Pago GT TODAY THEN DO:                                                                             
                    IF PlanPagos.Cuota GE Creditos.Cuota THEN                                                          
                       ASSIGN KCuo = PlanPagos.Cuota.                                                                  
                    ELSE                                                                                               
                       KCuo = Creditos.Cuota.                                                                          
                                                                                                                     
                    ASSIGN Creditos.Sdo_Proyect   = Creditos.Sdo_Capital       /*Nov.25/06 Nuevo Proyectado al Día */  
                           Creditos.Capital_Acum  = Creditos.Monto - Creditos.Sdo_Proyect                              
                           PlanPagos.Capital_acum = Creditos.Monto - Creditos.Sdo_Proyect 
                           Creditos.Fec_Pago      = FPOk
                           Creditos.Cuo_Pagadas   = (Creditos.Plazo - CuoFalt)
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

                    ASSIGN Creditos.Val_Atraso  = 0
                           Creditos.Dias_Atraso = 0
                           Creditos.Cuo_Atraso  = 0.                
           END.
           ELSE IF Creditos.Cuo_Pagadas LE (Creditos.Plazo - CuoFalt) THEN DO:
                 ASSIGN W_RowIdPPT = ROWID(PlanPagos)
                        W_RowIdCre = ROWID(Creditos).

                 RUN Halla_PagasFalt.R (INPUT W_RowIdCre,W_RowIdPPT,W_TasaUs).
           END.*/
         
       END.

       IF LAST-OF(Creditos.Agencia) THEN 
          MESSAGE "Total Van X Agenc.:" cont
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.
OUTPUT CLOSE.
