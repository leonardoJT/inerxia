DEFINE VAR intCorriente AS DECIMAL.

FOR EACH creditos WHERE creditos.agencia = 1
                    AND creditos.cod_credito = 62
                    /*AND (creditos.cod_credito = 17 OR
                         creditos.cod_credito = 103 OR
                         creditos.cod_credito = 118 OR
                         creditos.cod_credito = 128 OR
                         creditos.cod_credito = 133 OR
                         creditos.cod_credito = 185)*/
                    AND creditos.INT_corriente <> 0 NO-LOCK:
    intCorriente = intCorriente + creditos.INT_corriente.
END.

MESSAGE intCorriente
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
