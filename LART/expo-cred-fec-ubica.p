DEFINE VAR wsalida AS CHAR FORMAT "X(200)".
OUTPUT TO c:\INFO_fodun\fecmay302011.csv.
FOR EACH creditos WHERE estado = 2 AND fec_pago NE ? NO-LOCK BY agencia :
    ASSIGN wsalida = string(agencia)  + ";" + 
                    nit + ";" + string(cod_credito) + ";" +  
                    string(num_credito) + ";" + string(fec_pago)
        .
    DISPLAY wsalida NO-LABEL WITH WIDTH 300.
    
END.
OUTPUT CLOSE.
