DEFINE VAR suma1 AS DECIMAL.
DEFINE VAR suma2 AS DECIMAL.

DEFINE VAR cont AS INTEGER.
DEFINE VAR vagencia AS INTEGER INITIAL 1.

FOR EACH creditos WHERE INT_corriente + INT_morCobrar <> 0
                    AND (cod_credito = 32 OR cod_credito = 186)
                    AND agencia = vagencia
                    /*AND fecCorte = 08/31/2014*/ NO-LOCK:
    suma1 = suma1 + INT_corriente + INT_morCobrar.
END.

FOR EACH sal_cuenta WHERE (cuenta = "1655180104" OR
                           cuenta = "1655220104" OR
                           cuenta = "1655240104" OR
                           cuenta = "1655260104" OR
                           cuenta = "1655280104" OR
                           cuenta = "1655180204" OR
                           cuenta = "1655180224")
                      AND ano = 2014
                      AND agencia = vagencia NO-LOCK:
    suma2 = suma2 + sal_inicial.

    DO cont = 1 TO 9:
        suma2 = suma2 + db[cont] - cr[cont].
    END.
END.

MESSAGE suma1 suma2 suma1 - suma2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
