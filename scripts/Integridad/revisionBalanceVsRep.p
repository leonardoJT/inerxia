DEFINE VAR saldo AS DECIMAL.

FOR EACH rep_creditos WHERE agencia = 3
                       AND cod_credito = 17
                       AND fecCorte = 01/31/2017 NO-LOCK:
    saldo = saldo + rep_creditos.INT_morCobrar.
    

    /*DISPLAY rep_ahorros  WITH WIDTH 300 1 COL.*/
END.

/*
saldoK = 0.

FOR EACH ahorros WHERE agencia = 3
                   AND cod_ahorro = 8
                   AND sdo_disponible < 0:
    saldoK = saldoK + ahorros.sdo_disponible.

    UPDATE ahorros  WITH WIDTH 300 1 COL.
END.
*/
 
MESSAGE saldo
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
