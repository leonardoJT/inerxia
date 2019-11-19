OUTPUT TO c:\INFO_fodun\leonardo\creditosEndYear.csv.
FOR EACH creditos WHERE sdo_capital > 0 NO-LOCK:
    EXPORT DELIMITER ";"
        nit
        cod_credito
        sdo_capital
        INT_corriente
        INT_morCobrar
        provision.
END.
OUTPUT CLOSE.
