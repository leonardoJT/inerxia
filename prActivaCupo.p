OUTPUT TO "c:\info_juriscoop\CupoRotaCanceMarzo.csv".
PUT "agencia;nit;credito;fec.cancetotal;pagototal;pago_minimo;val_recaudo" SKIP.
FOR EACH facturacion WHERE estado = 1 NO-LOCK:
    FIND FIRST creditos WHERE
        creditos.nit = facturacion.nit AND
        creditos.num_credito = facturacion.num_credito AND
        creditos.estado = 3 
    NO-ERROR.
    IF AVAILABLE creditos THEN DO:
       PUT creditos.agencia     ";"
           creditos.nit         ";"
           creditos.num_credito ";"
           fec_cancetotal       ";"
           pagototal            ";"
           pago_minimo          ";"
           val_recaudo          ";"
           SKIP.
       UPDATE creditos.estado = 2.
       UPDATE creditos.fec_cancetotal = ?.
    END.
END.
MESSAGE "Listado: " "c:\info_juriscoop\CupoRotaCanceMarzo.csv"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
