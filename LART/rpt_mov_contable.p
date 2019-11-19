FOR EACH mov_contable WHERE Mov_Contable.agencia = 1 AND Mov_Contable.Cuenta  BEGINS("1110")  NO-LOCK:
    
    DISPLAY Mov_Contable.Cuenta 
            Mov_Contable.Comprobante
            Mov_Contable.Nit
            Mov_Contable.Comentario FORMAT "XXXXXXXXXXXXXXXXXXXXXXXXX"
            Mov_Contable.Doc_Referencia 
            Mov_Contable.Num_Documento 
            Mov_Contable.Enlace
            Mov_Contable.Db
            Mov_Contable.Cr 
            
        WITH WIDTH 200 .
    
END.
