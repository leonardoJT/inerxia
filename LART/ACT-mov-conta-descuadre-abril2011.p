/* DISABLE TRIGGER OF mov_contable. */ 
DEFINE VAR wsalida AS CHAR FORMAT "x(300)".
FOR EACH mov_contable WHERE Mov_Contable.agencia EQ 4 
                            AND Mov_Contable.Comprobante EQ 20 
                            AND Mov_Contable.Num_Documento = 129
                            AND Mov_Contable.Fec_Contable EQ date("30/04/2011") :
    IF Mov_Contable.Db EQ  40000000 THEN DO:
        DISPLAY Mov_Contable.agencia
                Mov_Contable.Fec_Contable 
                Mov_Contable.Comprobante 
                Mov_Contable.Num_Documento 
                Mov_Contable.Cuenta
                Mov_Contable.Db FORMAT "->>,>>>,>>9.99" 
                Mov_Contable.Cr FORMAT "->>,>>>,>>9.99"
            WITH WIDTH 500. 
          /* ASSIGN Mov_Contable.db = 40000000.    */ 
    END.
    
END.
