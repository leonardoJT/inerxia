DEFINE VAR cont AS INTEGER.

DEFINE TEMP-TABLE ttAnexos LIKE anexos.

FOR EACH anexos WHERE cuenta = "16250501"
                  AND agencia = 2
                  AND ano = 2011 NO-LOCK:
    CREATE ttAnexos.
    BUFFER-COPY anexos TO ttAnexos.
    ttAnexos.sdo_Final = anexos.sdo_inicial.

    DO cont = 1 TO 12:
        ttAnexos.sdo_Final = ttAnexos.sdo_Final + anexos.db[cont] - anexos.cr[cont].
    END.
END.

OUTPUT TO d:\Leonardo\Anexos.txt.
FOR EACH ttAnexos WHERE ttAnexos.sdo_Final <> 0 NO-LOCK:
    FIND FIRST anexos WHERE anexos.nit = ttAnexos.nit
                        AND anexos.ano = ttAnexos.ano + 1
                        AND anexos.cuenta = ttAnexos.cuenta
                        AND anexos.agencia = ttAnexos.agencia
                        AND anexos.sdo_inicial = ttAnexos.sdo_Final NO-LOCK NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        DISPLAY ttAnexos.nit ttAnexos.sdo_Final FORMAT "$->>>,>>>,>>>,>>9".
        /*DISPLAY ttanexos WITH 1 COL.*/

        CREATE anexos.
        BUFFER-COPY ttAnexos TO anexos.
        anexos.ano = 2012.
        anexos.sdo_inicial = ttAnexos.sdo_final.
        anexos.sdo_final = 0.

        DO cont = 1 TO 12:
            anexos.db[cont] = 0.
            anexos.cr[cont] = 0.
        END.

        CREATE anexos13.
        BUFFER-COPY ttAnexos TO anexos13.
        anexos13.ano = 2012.
        anexos13.sdo_inicial = ttAnexos.sdo_final.
        anexos13.sdo_final = 0.

        DO cont = 1 TO 12:
            anexos13.db[cont] = 0.
            anexos13.cr[cont] = 0.
        END.

        CREATE anexos.
        BUFFER-COPY ttAnexos TO anexos.
        anexos.ano = 2013.
        anexos.sdo_inicial = ttAnexos.sdo_final.
        anexos.sdo_final = 0.

        DO cont = 1 TO 12:
            anexos.db[cont] = 0.
            anexos.cr[cont] = 0.
        END.
    END.
END.
OUTPUT CLOSE.
