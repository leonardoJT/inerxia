DEFINE VAR cont AS INTEGER.
DEFINE VAR sdo AS DECIMAL.

OUTPUT TO C:\INFO_Fodun\Leonardo\Anexos.txt.
FOR EACH anexos WHERE anexos.cuenta = "16909502"
                  AND ano = 2012 NO-LOCK BREAK BY nit
                                               BY agencia:
    IF FIRST-OF(anexos.agencia) THEN
        sdo = 0.

    sdo = sdo + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        sdo = sdo + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.agencia) AND sdo <> 0 THEN
        DISPLAY agencia nit sdo FORMAT "$->>>,>>>,>>9.99".
END.
OUTPUT CLOSE.
