OUTPUT TO C:\INFO_FODUN\plazo-error.csv.
FOR EACH creditos WHERE plazo > 100 NO-LOCK:
        DISPLAY agencia 
                nit
                COD_CREDITO
                num_credito 
                Creditos.Fec_Desembolso  
                plazo   
                
            WITH WIDTH 200 NO-LABEL .
    
END.
OUTPUT CLOSE.
  
