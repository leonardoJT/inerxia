 
  DISABLE TRIGGERS FOR LOAD OF creditos.
  
  FOR EACH Creditos WHERE  month(fec_desembolso) = 11 and
                           year(fec_desembolso) = 2008 AND estado = 2:
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
          MESSAGE "Inconsistencia para generar planpagos"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         NEXT.   
      END.
      DISP nit fec_desembolso monto sdo_capital.
      /*FOR EACH  planpagos WHERE planpagos.agencia = creditos.agencia         AND
                                planpagos.nit     = creditos.nit             AND
                                planpagos.cod_credito = creditos.cod_credito AND
                                planpagos.num_credito = creditos.num_credito: 
          
      END.*/
  END.

