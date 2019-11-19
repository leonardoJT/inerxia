DEFINE TEMP-TABLE ttanexos LIKE anexos 
    FIELD sdoFinal AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

DEFINE VAR cont AS INTEGER.

FOR EACH anexos13 WHERE anexos13.ano = 2011
                    AND SUBSTRING(anexos13.cuenta,1,4) = "1690" NO-LOCK:
    CREATE ttAnexos.
    BUFFER-COPY anexos13 TO ttAnexos.

    ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.sdo_inicial.

    DO cont = 1 TO 12:
        ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.db[cont] - ttAnexos.cr[cont].
    END.
END.

FOR EACH ttAnexos NO-LOCK:
    FIND FIRST anexos13 WHERE anexos13.agencia = ttAnexos.agencia
                          AND anexos13.cuenta = ttAnexos.cuenta
                          AND anexos13.nit = ttAnexos.nit
                          AND anexos13.cen_costos = ttAnexos.cen_costos
                          AND anexos13.ano = 2012 NO-ERROR.
    IF AVAILABLE anexos13 THEN DO:
        IF anexos13.sdo_inicial <> ttAnexos.sdoFinal THEN DO:
            DISPLAY anexos13.agencia anexos13.cuenta ttAnexos.nit ttAnexos.cen_Costos ttAnexos.sdoFinal anexos13.sdo_inicial SKIP WITH WIDTH 200.
            anexos13.sdo_inicial = ttAnexos.sdoFinal.
        END.
    END.
    ELSE DO:
        CREATE anexos13.
        anexos13.agencia = ttAnexos.agencia.
        anexos13.cuenta = ttAnexos.cuenta.
        anexos13.nit = ttAnexos.nit.
        anexos13.cen_costos = ttAnexos.cen_costos.
        anexos13.ano = 2012.
        anexos13.sdo_inicial = ttAnexos.sdoFinal.
    END.

    FIND FIRST anexos WHERE anexos.agencia = ttAnexos.agencia
                        AND anexos.cuenta = ttAnexos.cuenta
                        AND anexos.nit = ttAnexos.nit
                        AND anexos.cen_costos = ttAnexos.cen_costos
                        AND anexos.ano = 2012 NO-ERROR.
    IF AVAILABLE anexos THEN DO:
        IF anexos.sdo_inicial <> ttAnexos.sdoFinal THEN DO:
            DISPLAY anexos.agencia anexos.cuenta Anexos.nit anexos.cen_costos 0 + ttAnexos.sdoFinal FORMAT "->>>,>>>,>>>,>>9.99" anexos.sdo_inicial SKIP WITH WIDTH 200.
            anexos.sdo_inicial = ttAnexos.sdoFinal.
        END.
    END.
    ELSE DO:
        CREATE anexos.
        anexos.agencia = ttAnexos.agencia.
        anexos.cuenta = ttAnexos.cuenta.
        anexos.nit = ttAnexos.nit.
        anexos.cen_costos = ttAnexos.cen_costos.
        anexos.ano = 2012.
        anexos.sdo_inicial = ttAnexos.sdoFinal.
    END.
END.
