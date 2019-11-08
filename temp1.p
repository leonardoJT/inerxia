DEFINE VAR cont AS INTEGER.
DEFINE VAR saldo AS DECIMAL.
DEFINE VAR vAgencia AS INTEGER.

FOR EACH anexos WHERE cuenta = "16909502" AND agencia = 1
    AND ano = 2017 NO-LOCK:

    saldo = anexos.sdo_inicial.

    DO cont = 1 TO 8:
        saldo = saldo + anexos.db[cont] - anexos.cr[cont].
    END.

    IF saldo < 0 THEN
        DISPLAY agencia cen_costo nit saldo.
END.
