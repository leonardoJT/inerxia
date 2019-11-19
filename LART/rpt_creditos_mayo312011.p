DEFINE VAR wsalida AS CHAR FORMAT "X(300)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(300)".
DEFINE VAR wfec1 AS CHAR FORMAT "X(10)".
DEFINE VAR wfec2 AS CHAR FORMAT "X(10)".
ASSIGN wtitulo = "Agencia;Nit;Linea;Numero;fec-desem;monto;cuota;plazo;tasa;Fec_1pago;Fec_ubica;cuo-pagas;dia-atraso;int-corri;int-morcobrar;sado-capital;sdo-intMor;sdo-Intpag;val-atraso".
OUTPUT TO C:\info_fodun\creditosjun012011-bta.csv.
DISPLAY wtitulo NO-LABEL WITH WIDTH 500.
FOR EACH Creditos WHERE Creditos.Agencia EQ 1 AND  Creditos.Estado EQ 2
                           
     NO-LOCK BY agencia BY cod_credito:
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
                         STRING(Creditos.Fec_Desembolso) + ";" +
                         STRING(Creditos.Monto, "99999999999.99") + ";" + 
                         STRING(Creditos.Cuota, "99999999999.99") + ";" +
                         STRING(Creditos.Plazo, "9999") + ";" +
                         STRING(Creditos.Tasa, "999.99999") + ";" +
                         wfec1 + ";" +
                         wfec2 + ";" + 
                         STRING(Creditos.Cuo_Pagadas, "99999") + ";" + 
                         string(Creditos.Dias_Atraso, "99999") + ";" + 
                         STRING(Creditos.Int_Corrientes, "-99999999999.99") + ";" + 
                         STRING(Creditos.Int_MorCobrar, "-99999999999.99") + ";" + 
                         STRING(Creditos.Sdo_Capital, "-99999999999.99") + ";" + 
                         STRING(Creditos.Sdo_IntMor, "-99999999999.99") + ";" + 
                         STRING(Creditos.Sdo_IntPag, "-99999999999.99") + ";" + 
                         STRING(Creditos.Val_Atraso, "-99999999999.99")
            .
        DISPLAY wsalida NO-LABEL WITH WIDTH 500.
END.    
OUTPUT CLOSE.
