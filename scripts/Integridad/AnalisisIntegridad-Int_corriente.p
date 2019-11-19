DEFINE VAR sumaInt AS DECIMAL.
DEFINE VAR sumaCont AS DECIMAL.
DEFINE VAR cont AS INTEGER.

DEFINE VAR fechaCorte AS DATE INITIAL 03/31/2019.
DEFINE VAR vAgencia AS INTEGER INITIAL 1.


FOR EACH rep_creditos WHERE fecCorte = fechaCorte
                       AND rep_creditos.agencia = vAgencia
                       AND rep_creditos.cod_credito = 123 NO-LOCK:
    sumaInt = sumaInt + rep_creditos.INT_corriente.
END.

FOR EACH sal_cuenta WHERE sal_cuenta.ano = YEAR(fechaCorte)
                      AND sal_cuenta.agencia = vAgencia
                      AND (sal_cuenta.cuenta = "1443050125" OR
                           sal_cuenta.cuenta = "1443100125" OR
                           sal_cuenta.cuenta = "1443150125" OR
                           sal_cuenta.cuenta = "1443200125" OR
                           sal_cuenta.cuenta = "1443250125") NO-LOCK:
    sumaCont = sumaCont + sal_cuenta.sal_inicial.

    DO cont = 1 TO MONTH(fechaCorte):
        sumaCont = sumaCont + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

MESSAGE sumaInt sumaCont sumaInt - sumaCont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
