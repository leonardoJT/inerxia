DEFINE VAR cont AS INTEGER.
    
FOR EACH creditos WHERE agencia = 3 AND cod_credito = 113 AND sdo_capital > 0:
    cont = cont + 1.

    creditos.INT_corriente = creditos.INT_corriente + 1.

    IF cont = 3 THEN
        LEAVE.
END.

MESSAGE cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
