OUTPUT TO "c:\info_juriscoop\AhorrosManejo.csv".
PUT "agencia;nit;credito;fecha;cpte;cod_operacion;describ;valor" SKIP.
FOR EACH mov_creditos WHERE cpte = 10 AND cod_operacion = 010302001 NO-LOCK:
    PUT mov_creditos.agencia     ";"
        mov_creditos.nit         ";"
        mov_creditos.num_credito ";"
        mov_creditos.fecha       ";"
        mov_creditos.Cpte            ";"
        mov_creditos.cod_operacion ";"
        mov_creditos.Descrip         ";"
        mov_creditos.val_cheque + mov_creditos.val_efectivo FORMAT "->>>,>>>,>>>,>>9.99"
    SKIP.
END.
MESSAGE "Listado: " "c:\info_juriscoop\AhorrosManejo.csv"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
