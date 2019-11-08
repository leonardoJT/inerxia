DEF BUFFER c FOR creditos.

OUTPUT TO c:\migrar\ERROR-planpagos.csv.
EXPORT DELIMITER ";"
"nit            "
"num_credito    "
"cod_credito    "
"cuota          "
"Fec_Desembolso "
"Int_Anticipado "
"Int_Corrientes "
"Int_DifCobro   "
"Int_MoraDifCob "
"Int_MorCobrar  "
"Monto          "
"Plazo          "
"Polizas        ".


DEF VAR j AS INTEGER.
    FOR EACH c no-LOCK WHERE c.estado = 2  AND 
      ( c.Cuota       LE 0
      OR c.Fec_Desembolso EQ ? OR STRING(c.Fec_Desembolso) LE " "
      OR c.Int_Anticipado LT 0
      OR c.Int_Corrientes LT 0
      OR c.Int_DifCobro   LT 0 
      OR c.Int_MoraDifCob LT 0
      OR c.Int_MorCobrar  LT 0 
      OR c.Monto          LE 0
      OR c.Plazo          LE 0
      OR c.Polizas        LT 0 
      OR (c.Int_Anticipado GT 0 AND (c.INT_Corrientes + c.Int_DifCobro GT 0))).

/*          j = j + 1.
          DISP*/
        EXPORT DELIMITER ";"
              c.nit
              c.num_credito
              c.cod_credito
              c.cuota
              c.Fec_Desembolso
              c.Int_Anticipado
              c.Int_Corrientes
              c.Int_DifCobro  
              c.Int_MoraDifCob
              c.Int_MorCobrar 
              c.Monto         
              c.Plazo         
              c.Polizas       

              /*
              c.Costas      
              c.Cuota       
              c.Fec_Desembolso
              c.Int_Anticipado
              c.Int_Corrientes
              c.Int_DifCobro  
              c.Int_MoraDifCob
              c.Int_MorCobrar 
              c.Monto         
              c.Plazo         
              c.Polizas       


                WITH 1 COL*/
    .

    END.
/*    DISP j.*/
