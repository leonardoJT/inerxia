DEFINE VAR cont AS INTEGER.

DEFINE TEMP-TABLE tt
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD saldo AS DECIMAL.



FOR EACH anexos WHERE ano = 2015 AND cuenta = "24959550" NO-LOCK:
    FIND FIRST tt WHERE tt.agencia = anexos.agencia AND tt.nit = anexos.nit NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.agencia = anexos.agencia.
        tt.nit = anexos.nit.
    END.

    tt.saldo = tt.saldo + anexos.sdo_inicial.

    DO cont = 1 TO 8:
        tt.saldo = tt.saldo + anexos.cr[cont] - anexos.db[cont].
    END.
END.

OUTPUT TO d:\Leonardo\anexos.csv.
FOR EACH tt WHERE tt.saldo <> 0 NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.

MESSAGE "Proceso finalizado"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
