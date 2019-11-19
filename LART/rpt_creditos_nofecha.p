DEFINE VAR wsalida AS CHAR FORMAT "X(300)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)".
DEFINE VAR wfec1 AS CHAR FORMAT "X(10)".
DEFINE VAR wfec2 AS CHAR FORMAT "X(10)".
ASSIGN wtitulo = "Agencia;Nit;Linea;Numero;Fec_1pago;Fec_ubica".
OUTPUT TO C:\info_fodun\Ajuste_creditos-viany\revisar-creditos.csv.
DISPLAY wtitulo NO-LABEL WITH WIDTH 500.
FOR EACH Creditos WHERE Creditos.Agencia EQ 1 AND  
                        (Creditos.Cod_Credito EQ 108  OR Creditos.Cod_Credito EQ 113)   
     NO-LOCK:
    IF (Creditos.Estado = 2) AND (Creditos.Fec_PagAnti NE Creditos.Fec_Pago)  THEN DO:
        IF Creditos.Fec_PagAnti EQ ? THEN DO: 
            ASSIGN wfec1 = "No Existe ".
        END.
        ELSE wfec1 = string(Creditos.Fec_PagAnti).

        IF Creditos.Fec_Pago EQ ? THEN DO: 
           ASSIGN wfec2 = "No Existe ".
        END.
        ELSE wfec2 = string(Creditos.Fec_Pago).

        ASSIGN wsalida = string(Creditos.agencia) + ";" +
                         Creditos.Nit + ";" + 
                         string(Creditos.Cod_Credito) + ";" +
                         string(Creditos.Num_Credito) + ";" + 
                         wfec1 + ";" +
                         wfec2.
        DISPLAY wsalida NO-LABEL WITH WIDTH 500.
    END.
END.
OUTPUT CLOSE.
