DEFINE VAR wsalida AS CHAR FORMAT "X(255)"   NO-UNDO.
DEFINE VAR wtitulo AS CHAR FORMAT "X(255)"   NO-UNDO.
wtitulo = "Agencia ; nit ; Creditos.Cod_Credito ; Num_Credito ; Fec_Desembolso ; Fec_PagAnti " .

OUTPUT TO c:\INFO_fodun\fec_penXagencia.csv.
DISPLAY wtitulo NO-LABEL WITH   WIDTH  500.
FOR EACH Creditos WHERE Creditos.Fec_Pago = ? AND estado = 2   NO-LOCK BY agencia :
    IF Creditos.Fec_Pago NE ? THEN  DO: NEXT.
    END.
    wsalida = string(Creditos.Agencia) + ";" + 
              Creditos.nit + ";" + string(Creditos.Cod_Credito) +  ";" + 
              string(Creditos.Num_Credito) + ";" +  
              string(Creditos.Fec_Desembolso) +  ";" +  
              string(Creditos.Fec_PagAnti).
    DISPLAY wsalida NO-LABEL WITH   WIDTH  500.

END.
OUTPUT CLOSE.

