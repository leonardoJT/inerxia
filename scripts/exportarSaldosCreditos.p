OUTPUT TO d:\Leonardo\creditos62.csv.

FOR EACH creditos WHERE cod_credito = 62
    AND agencia = 1
    AND estado = 2 NO-LOCK:
    EXPORT DELIMITER ";"
        agencia
        nit
        sdo_capital
        INT_corriente
        INT_morCobrar
        INT_difCobro
        Int_MoraDifCob.
END.

OUTPUT CLOSE.
