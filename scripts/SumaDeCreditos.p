DEFINE VAR vTotal1 AS DECIMAL.
DEFINE VAR vTotal2 AS DECIMAL.
DEFINE VAR codigo AS INTEGER.
DEFINE VAR vAgencia AS INTEGER.

codigo = 185.
vAgencia = 4.

FOR EACH creditos WHERE creditos.cod_credito = codigo
                    AND creditos.agencia = vAgencia
                    AND creditos.sdo_capital > 0
                    AND creditos.FOR_pago = 1 NO-LOCK:
    vTotal1 = vTotal1 + creditos.sdo_capital.
END.

FOR EACH creditos WHERE creditos.cod_credito = codigo
                    AND creditos.agencia = vAgencia
                    AND creditos.sdo_capital > 0
                    AND creditos.FOR_pago = 2 NO-LOCK:
    vTotal2 = vTotal2 + creditos.sdo_capital.
END.

MESSAGE STRING(vTotal1,"$>>>,>>>,>>>,>>9.99") SKIP
        STRING(vTotal2,"$>>>,>>>,>>>,>>9.99")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
