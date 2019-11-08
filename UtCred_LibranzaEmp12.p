/*UtCred_LibranzaEmp12.P*/

  DEFINE VARIABLE PlazoW LIKE Solicitud.Plazo.
  DEFINE VARIABLE TotPtW LIKE Solicitud.Monto.
  DEFINE VARIABLE CuotaW LIKE Solicitud.Cuota.
  DEFINE VARIABLE TasaW  LIKE Solicitud.Tasa.
  DEFINE VARIABLE TinteW LIKE Solicitud.Monto.
  DEFI   VAR W_CuoPla    AS INTEG FORM "9".
  DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
  DEFINE VAR Wimp_Coop   AS INTEGER INITIAL 0. 
  DEFI   VAR Fec_Corte   AS DATE.

  {Incluido/Variable.I "SHARED"}

  {Incluido/VARCON.I   "SHARED"} 

  DEFI TEMP-TABLE TPlanP LIKE PlanPagos. 

  Fec_Corte = DATE(07,27,2005).

MESSAGE "Recuerde que debe haber actualizado este .P con fecha de Control" SKIP
        " se procesa con : " Fec_Corte
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
SESSION:SET-WAIT-STATE("General").

OUTPUT TO C:\InfRed\CredEmp12_AntUti.Txt.

DISPLAY "        COOP.BELEN  -  CREDITOS EMPRESA 12" SKIP
        "REINICIADOS A QNAL, Fecha : " + STRING(TODAY,"99/99/9999") FORMAT "X(50)".

DO TRANSACTION ON ERROR UNDO: 

FOR EACH Clientes WHERE Cod_Empresa EQ 12 NO-LOCK BY Clientes.Nit:
  FOR EACH Creditos WHERE Creditos.Nit         EQ Clientes.Nit AND 
                          Creditos.FOR_pago    EQ 2            AND
                          Creditos.per_pago    EQ 4            AND
                          Creditos.Sdo_capital GT 0:
     ASSIGN TotPtW   = Creditos.Sdo_Capital
            CuotaW   = ROUND(Creditos.Cuota / 2,0)
            W_CuoPla = 2
            PlazoW   = 0
            TasaW    = Creditos.Tasa.
            
     RUN NVEF IN W_ManFin (INPUT TasaW / (24 * 100),INPUT 24,OUTPUT TasaW).       
            
     TasaW = TasaW * 100.                    
        
     RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw, INPUT-OUTPUT PlazoW, INPUT-OUTPUT CuotaW,            
                     INPUT-OUTPUT TInteW, INPUT-OUTPUT TasaW, INPUT 0,                              
                     INPUT 0,INPUT 3, INPUT W_CuoPla,                                      
                     INPUT 1,                                                    
                     INPUT 1).                                                       
     IF PlazoW LE 0 THEN DO:                                                                        
        MESSAGE "El Valor del Plazo debe ser mayor a cero. Rectifique!" SKIP 
                "Ced/Nit : " Creditos.Nit skip
                "Creditos.Num_Credito : " Creditos.Num_Credito VIEW-AS ALERT-BOX ERROR.  
                                                         
        NEXT.                                                                               
     END.      

     FIND LAST PlanPagos WHERE PlanPagos.Agencia   EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1 NO-ERROR.
     IF NOT AVAILABLE(PlanPagos) THEN DO:
        MESSAGE "La Tabla PlanPagos para el Pdo-Actual No Existe..." SKIP
              "Ced/Nit : " Creditos.Nit skip
                "Creditos.Num_Credito : " Creditos.Num_Credito SKIP
              "                                     Refinanciaciòn cancelada..."
              VIEW-AS ALERT-BOX ERROR.
        NEXT.
     END. 

     DISPLAY Creditos.Agencia      LABEL "Ag."
             Creditos.Nit          LABEL "Ced./Nit"
             Creditos.Cod_Credito  LABEL "Pdcto"
             Creditos.Num_Credito  LABEL "Num_Credito"
             Creditos.Cuota        LABEL "Cuota-Mes"
             ROUND(Creditos.Cuota / 2,0) LABEL "Cuota-Qnal"            FORM ">>>>,>>>,>>9"
             Creditos.Plazo              LABEL "Pla-Mes"
             (Creditos.Plazo - Creditos.Cuo_Pagadas) LABEL "X-PagM"    FORM "9999"
             PlazoW                                  LABEL "N.Pla-Qna" FORM "9999"
             Creditos.Fec_Pago                       LABEL "Px-Pago Ant"
             Creditos.Sdo_Capital                    LABEL "Sdo-Capital"
             Creditos.Int_Anticipado                 LABEL "Int-Anticip"
             Creditos.INT_Corrientes + Creditos.Int_DifCobro 
                                                     LABEL "ICtes+CtesDC" FORM ">>>>,>>>,>>9"
             Creditos.INT_MorCobrar + Creditos.Int_MoraDifCob 
                                                     LABEL "IMora+MoraDC" FORM ">>>>,>>>,>>9"
         WITH DOWN WIDTH 200 FRAME F1 NO-BOX NO-LABEL USE-TEXT STREAM-IO.


     ASSIGN Creditos.Per_Pago     = 3
            Creditos.Cuota        = ROUND(Creditos.Cuota / 2,0)
            Creditos.Plazo        = PlazoW
            Creditos.Cuo_Pagadas  = 0 
            Creditos.Monto        = Creditos.Sdo_Capital
            Creditos.Sdo_Proyecta = Creditos.Sdo_Capital
            Creditos.Capital_Acum = 0
            /*Creditos.Int_LiqAcum  = Creditos.INT_Corrientes + Creditos.Int_DifCobro */
            Creditos.Int_LiqAcum  = 0
            Creditos.Sdo_CapPag   = 0
            Creditos.Sdo_IntPag   = Creditos.Int_Anticipado
            Creditos.Sdo_IntMor   = 0
            Creditos.Val_Atraso   = 0
            Creditos.Cuo_Atraso   = 0
            Creditos.Dias_Atraso  = 0.                     

     RUN ActualizaPP.
  END.
END.
END.

OUTPUT CLOSE.

SESSION:SET-WAIT-STATE("").

PROCEDURE ActualizaPP:
  DEFI VAR W_ContCuota AS INTEG FORM "9999" INIT 0.
  DEFI VAR K           AS INTEG FORM "9999" INIT 0.
  DEFI VAR W_DiasTrans AS INTEG FORM "9999" INIT 0.
  DEFI VAR Fec_Control LIKE Creditos.Fec_Pago.

  ASSIGN PlanPagos.Id_PdoMes = 4.  /*Lo marca ya cumplido y no vigente*/
  CREATE TPlanP.
  BUFFER-COPY PlanPagos TO TPlanP.
  
  CREATE PlanPagos.
  BUFFER-COPY TPlanP TO PlanPagos.
  ASSIGN PlanPagos.Id_PdoMes = 1            /*El nuevo que transcurre */
         PlanPagos.Nro_Cuota = 1
         PlanPagos.Plazo     = Creditos.Plazo
         PlanPagos.Cuota     = Creditos.Cuota
         PlanPagos.Tasa      = Creditos.Tasa
         PlanPagos.Cargos_Pdo        = 0          
         PlanPagos.Cargos_Acum       = Creditos.Costas + Creditos.Honorarios + Creditos.Polizas
         PlanPagos.Pagos_OtrosPdo    = 0
         PlanPagos.Pagos_OtrosAcum   = 0
         PlanPagos.Int_MoraPdo       = 0 
         PlanPagos.Int_MoraAcum      = Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar.

  ASSIGN PlanPagos.Pagos_MoraPdo     = 0
         PlanPagos.Pagos_MoraAcum    = 0                            /*Ojo 13 dias*/                          
         PlanPagos.Int_LiqPdo        = ROUND(((Creditos.Sdo_Capital * Creditos.Tasa) / 36000) * 13,0)  
         /*PlanPagos.Int_LiqAcum       = Creditos.INT_Corrientes + Creditos.Int_DifCobro */
         PlanPagos.Int_LiqAcum       = 0
         PlanPagos.Pagos_IntPdo      = 0
         PlanPagos.Pagos_IntAcum     = Creditos.Int_Anticipado
         PlanPagos.Capital_Pdo       = 0
         PlanPagos.Capital_Acum      = 0                               
         PlanPagos.Pagos_CapitalPdo  = 0 
         PlanPagos.Pagos_CapitalAcum = 0                                                
         PlanPagos.Monto_Actual      = Creditos.Sdo_Capital 
         PlanPagos.Cuo_Pagas         = 0
         W_ContCuota                 = 1.

  IF DAY(Fec_Corte) LT 15 THEN
     ASSIGN Creditos.Fec_Pago      = DATE(MONTH(Fec_Corte),15,YEAR(Fec_Corte))
            PlanPagos.Fec_ProxPago = Creditos.Fec_Pago
            PlanPagos.Fec_Vcto     = Creditos.Fec_Pago
            PlanPagos.Fec_Inic     = DATE(MONTH(Fec_Corte),1,YEAR(Fec_Corte)).
  ELSE 
     ASSIGN Creditos.Fec_Pago      = DATE(MONTH(Fec_Corte),30,YEAR(Fec_Corte))
            PlanPagos.Fec_ProxPago = Creditos.Fec_Pago
            PlanPagos.Fec_Vcto     = Creditos.Fec_Pago
            PlanPagos.Fec_Inic     = DATE(MONTH(Fec_Corte),16,YEAR(Fec_Corte)).

  ASSIGN Fec_Control = Creditos.Fec_Pago.
  Fec_Control = Fec_Control + 15.
  IF DAY(Fec_Control) NE 15 OR DAY(Fec_Control) NE 30 THEN DO:       
     IF DAY(Fec_Control) GE 13 AND DAY(Fec_Control) LE 18 THEN              
        Fec_Control = DATE(MONTH(Fec_Control),15,YEAR(Fec_Control)).        
     ELSE IF DAY(Fec_Control) LE 3 AND MONTH(Fec_Control) EQ 3 THEN         
        Fec_Control = DATE(2,28,YEAR(Fec_Control)).                         
     ELSE IF MONTH(Fec_Control) NE 2 THEN                                   
        Fec_Control = DATE(MONTH(Fec_Control),30,YEAR(Fec_Control)).        
  END.                                                                      

  FOR EACH PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                       AND PlanPagos.Nit          EQ Creditos.Nit        
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes    EQ 0   /*Futuros que Faltan*/
                           BY PlanPagos.Nro_Cuota: 
      IF W_ContCuota GT Creditos.Plazo THEN                     
         DELETE PlanPagos.
      ELSE DO:
         ASSIGN PlanPagos.Plazo        = Creditos.Plazo
                PlanPagos.Cuota        = Creditos.Cuota
                PlanPagos.Monto_Actual = Creditos.Sdo_Capital
                W_ContCuota            = W_ContCuota + 1
                PlanPagos.Nro_Cuota    = W_ContCuota
                PlanPagos.Fec_Inic     = Fec_Control - 14
                PlanPagos.Fec_Vcto     = Fec_Control.

         Fec_Control = Fec_Control + 15.
         IF DAY(Fec_Control) NE 15 OR DAY(Fec_Control) NE 30 THEN DO:
            IF DAY(Fec_Control) GE 13 AND DAY(Fec_Control) LE 18 THEN
               Fec_Control = DATE(MONTH(Fec_Control),15,YEAR(Fec_Control)).
            ELSE IF DAY(Fec_Control) LE 3 AND MONTH(Fec_Control) EQ 3 THEN
               Fec_Control = DATE(2,28,YEAR(Fec_Control)).
            ELSE IF MONTH(Fec_Control) NE 2 THEN
               Fec_Control = DATE(MONTH(Fec_Control),30,YEAR(Fec_Control)).
         END.
      END.
  END.
  
  IF W_ContCuota GE Creditos.Plazo THEN
     RETURN.
  
  FIND LAST PlanPagos WHERE PlanPagos.Agencia     EQ Creditos.Agencia    
                       AND PlanPagos.Nit          EQ Creditos.Nit        
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes    EQ 0
                       AND PlanPagos.Nro_Cuota    GT 0  NO-LOCK NO-ERROR.
  IF AVAIL(PlanPagos) THEN DO:
     CREATE TPlanP.
     BUFFER-COPY PlanPagos TO TPlanP.
  END.
   
  IF W_ContCuota LT Creditos.Plazo THEN DO K = W_ContCuota + 1 TO Creditos.Plazo:
     CREATE PlanPagos.
     BUFFER-COPY TPlanP TO PlanPagos.
     ASSIGN PlanPagos.Plazo        = Creditos.Plazo      
            PlanPagos.Cuota        = Creditos.Cuota      
            PlanPagos.Monto_Actual = Creditos.Sdo_Capital
            PlanPagos.Tasa         = Creditos.Tasa
            PlanPagos.Cuo_Pagas    = 0
            PlanPagos.Id_PdoMes    = 0
            PlanPagos.Nro_Cuota    = K
            PlanPagos.Cargos_Pdo        = 0
            PlanPagos.Cargos_Acum       = 0
            PlanPagos.Pagos_OtrosPdo    = 0
            PlanPagos.Pagos_OtrosAcum   = 0
            PlanPagos.Int_MoraPdo       = 0
            PlanPagos.Int_MoraAcum      = 0
            PlanPagos.Pagos_MoraPdo     = 0
            PlanPagos.Pagos_MoraAcum    = 0
            PlanPagos.Int_LiqPdo        = 0
            PlanPagos.Int_LiqAcum       = 0
            PlanPagos.Pagos_IntPdo      = 0
            PlanPagos.Pagos_IntAcum     = 0
            PlanPagos.Capital_Pdo       = 0
            PlanPagos.Capital_Acum      = 0
            PlanPagos.Pagos_CapitalPdo  = 0
            PlanPagos.Pagos_CapitalAcum = 0 
            PlanPagos.Fec_Inic          = Fec_Control - 14
            PlanPagos.Fec_Vcto          = Fec_Control.  

     Fec_Control = Fec_Control + 15.                                   
     IF DAY(Fec_Control) NE 15 OR DAY(Fec_Control) NE 30 THEN DO:      
        IF DAY(Fec_Control) GE 13 AND DAY(Fec_Control) LE 18 THEN      
           Fec_Control = DATE(MONTH(Fec_Control),15,YEAR(Fec_Control)).
        ELSE IF DAY(Fec_Control) LE 3 AND MONTH(Fec_Control) EQ 3 THEN 
           Fec_Control = DATE(2,28,YEAR(Fec_Control)).                 
        ELSE IF MONTH(Fec_Control) NE 2 THEN                           
           Fec_Control = DATE(MONTH(Fec_Control),30,YEAR(Fec_Control)).
     END.                                                              
  END.      

  FOR EACH PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                       AND PlanPagos.Nit          EQ Creditos.Nit        
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes    EQ 2: /*Los marca ya cumplidos y no vigentes*/
      PlanPagos.Id_PdoMes = 4.
  END.

END PROCE.
