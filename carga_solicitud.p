DEFI VAR Consec AS INTEG FORM "99999999".
consec = 9900000.
FOR EACH creditos WHERE fec_desemb LT DATE(10,1,2006) AND sdo_capit GT 0:
    ASSIGN consec = consec + 1
           creditos.num_solici = consec.
           
    CREATE solicitud.
    ASSIGN  Solicitud.Agencia         = Creditos.Agencia       
            Solicitud.Cod_Credito     = Creditos.Cod_Credito   
            Solicitud.Cuota           = Creditos.Cuota         
            Solicitud.Estado          = 3        
            Solicitud.Fec_Aprobacion  = Creditos.Fec_Desemb 
            Solicitud.Fec_Solicitud   = Creditos.Fec_Desemb 
            Solicitud.Nit             = Creditos.Nit            
            Solicitud.Num_Solicitud   = Creditos.Num_Solicitud  
            Solicitud.Pagare          = Creditos.Pagare         
            Solicitud.Plazo           = Creditos.Plazo          
            Solicitud.Sistema         = Creditos.Sistema        
            Solicitud.Tip_Credito     = Creditos.Tip_Credito.    
END.
