DEFINE VAR wsalida AS CHAR  FORMAT "X(200)".
DEFINE VAR wtitulo AS CHAR FORMAT "X(200)".
ASSIGN wtitulo = "Agencia;Nit;Cod_ahorro;Cue_Ahorros;Cuota;Sdo_Disponible;Estado".
OUTPUT TO c:\INFO_fodun\cuota_aportesmay202011.csv.
DISPLAY wtitulo VIEW-AS TEXT WITH WIDTH 300 NO-LABEL.
FOR EACH ahorros  NO-LOCK :
    ASSIGN  wsalida = string(Agencia) + ";" +
            Nit + ";" + 
            string(Cod_ahorro) + ";" +
            Cue_Ahorros + ";" + string(Cuota) + ";" +
            STRING(Ahorros.Sdo_Disponible) + ";" +  
            STRING(Estado).
    DISPLAY wsalida
        VIEW-AS TEXT WITH WIDTH 300 NO-LABEL
          .
END.
OUTPUT CLOSE.
