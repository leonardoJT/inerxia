DEFINE BUFFER ttanexos FOR anexos.
DEFINE VAR sdoFinal AS DECIMAL.
DEFINE VAR cont AS INTEGER.

    
FOR EACH anexos13 WHERE anexos13.cuenta = "16909502" AND anexos13.agencia = 2 AND anexos13.ano = 2011 NO-LOCK:
    sdoFinal = anexos13.sdo_inicial.

    DO cont = 1 TO 12:
        sdoFinal = sdoFinal + anexos13.db[cont] - anexos13.cr[cont].
    END.

    FOR EACH ttAnexos WHERE ttAnexos.cuenta = anexos13.cuenta
                          AND ttAnexos.Agencia = anexos13.agencia
                          AND ttAnexos.nit = anexos13.nit
                          AND ttAnexos.cen_costos = anexos13.cen_costos
                          AND ttAnexos.ano = 2011 NO-LOCK:
        DISPLAY anexos13.nit sdoFinal FORMAT "->>>,>>>,>>>,>>9.99" ttAnexos.sdo_inicial FORMAT "->>>,>>>,>>>,>>9.99".
    END.

    MESSAGE sdoFinal
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
