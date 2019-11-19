DEFINE VAR saldo AS DECIMAL.

FOR EACH agencias NO-LOCK:
    saldo = 0.

    FOR EACH ahorros WHERE ahorros.agencia = agencias.agencia
                       AND cod_ahorro = 9 NO-LOCK:
        saldo = saldo + ahorros.sdo_disponible + ahorros.sdo_canje.
    END.

    MESSAGE agencias.agencia string(saldo,"$>>>,>>>,>>>,>>9.99")
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
