DEFINE VAR sumaAh AS DECIMAL.
DEFINE VAR sumaCont AS DECIMAL.
DEFINE VAR cont AS INTEGER.

DEFINE VAR fechaCorte AS DATE INITIAL 02/28/2017.
DEFINE VAR vAgencia AS INTEGER INITIAL 4.


FOR EACH rep_ahorros WHERE fecCorte = fechaCorte
                       AND rep_ahorros.agencia = vAgencia
                       AND rep_ahorros.cod_ahorro = 4 NO-LOCK:
    sumaAh = sumaAh + rep_ahorros.sdo_disponible + rep_ahorros.sdo_Canje.
END.

FOR EACH sal_cuenta WHERE sal_cuenta.ano = YEAR(fechaCorte)
                      AND sal_cuenta.agencia = vAgencia
                      AND sal_cuenta.cuenta = "21050501" NO-LOCK:
    sumaCont = sumaCont + sal_cuenta.sal_inicial.

    DO cont = 1 TO MONTH(fechaCorte):
        sumaCont = sumaCont + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

MESSAGE sumaAh sumaCont sumaAh - sumaCont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
