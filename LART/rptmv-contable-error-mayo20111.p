DEFINE VAR wsalida AS CHAR FORMAT "x(300)".
OUTPUT TO c:\INFO_fodun\doc-descuadrados-mayo.csv.
FOR EACH mov_contable WHERE Mov_Contable.agencia EQ 4 
                            AND Mov_Contable.Comprobante EQ 20 
                            AND Mov_Contable.Num_Documento = 191
                            AND Mov_Contable.Fec_Contable EQ date("31/05/2011") NO-LOCK:
    ASSIGN wsalida = string(Mov_Contable.agencia) + ";" +
                     string(Mov_Contable.Fec_Contable) + ";" +
                     string(Mov_Contable.Comprobante) + ";" +
                     string(Mov_Contable.Num_Documento) + ";" +  
                     Mov_Contable.Cuenta + ";" +
                     string(Mov_Contable.Db, "->>,>>>,>>9.99") + ";" + 
                     string(Mov_Contable.Cr, "->>,>>>,>>9.99"). 


    DISPLAY wsalida NO-LABEL WITH WIDTH 500.
    
END.


OUTPUT CLOSE.
