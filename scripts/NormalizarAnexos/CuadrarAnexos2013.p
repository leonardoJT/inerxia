DEFINE TEMP-TABLE ttanexos LIKE anexos 
    FIELD sdoFinal AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

DEFINE TEMP-TABLE ttsaldos
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD saldo AS DECIMAL
    FIELD cen_costos AS INTEGER.

DEFINE VAR cont AS INTEGER.
DEFINE VAR vCuenta AS CHARACTER INITIAL "16909502".
DEFINE VAR vAgencia AS INTEGER INITIAL 1.

FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.

/* 1. Organizamos los anexos con el saldo inicial */
FOR EACH anexos13 WHERE anexos13.ano = 2011
                    AND anexos13.cuenta = vCuenta
                    AND anexos13.agencia = vAgencia:
    DELETE anexos13.
END.

FOR EACH anexos WHERE anexos.ano = 2011
                  AND anexos.cuenta = vCuenta
                  AND anexos.agencia = vAgencia:
    anexos.sdo_inicial = 0.
END.

EMPTY TEMP-TABLE ttsaldos.

INPUT FROM d:\leonardo\saldos.csv.
REPEAT:
    CREATE ttsaldos.
    IMPORT DELIMITER ";" ttsaldos.

    IF ttsaldos.saldo <> 0 THEN DO:
        FIND FIRST anexos WHERE anexos.ano = 2011
                            AND anexos.cuenta = vCuenta
                            AND anexos.agencia = vAgencia
                            AND anexos.nit = ttsaldos.nit NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            ASSIGN anexos.nit = ttsaldos.nit
                   anexos.cen_costos = ttsaldos.cen_costos
                   anexos.cuenta = vCuenta
                   anexos.agencia = vAgencia
                   anexos.ano = 2011.
        END.

        anexos.sdo_inicial = anexos.sdo_inicial + ttsaldos.saldo.

        FIND FIRST clientes WHERE clientes.nit = ttsaldos.nit NO-LOCK NO-ERROR.
        IF NOT AVAILABLE clientes THEN DO:
            CREATE clientes.
            ASSIGN clientes.nit = ttsaldos.nit
                   clientes.nombre = ttsaldos.nombre.
        END.
    END.
END.

FOR EACH anexos WHERE anexos.ano = 2011
                  AND anexos.cuenta = vCuenta
                  AND anexos.agencia = vAgencia:
    CREATE anexos13.
    BUFFER-COPY anexos TO anexos13.
END.

/* ----------------------------- */

/* 2. Cuadramos los anexos para el 2011 */
EMPTY TEMP-TABLE ttAnexos.

FOR EACH anexos WHERE anexos.ano = 2011
                  AND anexos.cuenta = vCuenta
                  AND anexos.agencia = vAgencia NO-LOCK:
    CREATE ttAnexos.
    BUFFER-COPY anexos TO ttAnexos.

    ttAnexos.sdoFinal = ttAnexos.sdo_inicial.

    DO cont = 1 TO 12:
        IF cuentas.naturaleza = "DB" THEN
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.db[cont] - ttAnexos.cr[cont].
        ELSE
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.cr[cont] - ttAnexos.db[cont].
    END.
END.

FOR EACH anexos WHERE anexos.cuenta = vCuenta
                  AND anexos.ano = 2012
                  AND anexos.agencia = vAgencia:
    anexos.sdo_inicial = 0.
END.

FOR EACH anexos13 WHERE anexos13.cuenta = vCuenta
                    AND anexos13.ano = 2012
                    AND anexos13.agencia = vAgencia:
    anexos13.sdo_inicial = 0.
END.

FOR EACH ttAnexos WHERE ttAnexos.sdoFinal <> 0 NO-LOCK:
    FIND FIRST anexos13 WHERE anexos13.agencia = ttAnexos.agencia
                          AND anexos13.cuenta = ttAnexos.cuenta
                          AND anexos13.nit = ttAnexos.nit
                          AND anexos13.cen_costos = ttAnexos.cen_costos
                          AND anexos13.ano = 2012 NO-ERROR.
    IF AVAILABLE anexos13 THEN
        anexos13.sdo_inicial = ttAnexos.sdoFinal.
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
    IF AVAILABLE anexos THEN
        anexos.sdo_inicial = ttAnexos.sdoFinal.
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

EMPTY TEMP-TABLE ttAnexos.

FOR EACH anexos WHERE anexos.ano = 2012
                  AND anexos.cuenta = vCuenta
                  AND anexos.agencia = vAgencia NO-LOCK:
    CREATE ttAnexos.
    BUFFER-COPY anexos TO ttAnexos.

    ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.sdo_inicial.

    DO cont = 1 TO 12:
        IF cuentas.naturaleza = "DB" THEN
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.db[cont] - ttAnexos.cr[cont].
        ELSE
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.cr[cont] - ttAnexos.db[cont].
    END.
END.

FOR EACH anexos WHERE anexos.cuenta = vCuenta
                  AND anexos.ano = 2013
                  AND anexos.agencia = vAgencia:
    anexos.sdo_inicial = 0.
END.

FOR EACH anexos13 WHERE anexos13.cuenta = vCuenta
                    AND anexos13.ano = 2013
                    AND anexos13.agencia = vAgencia:
    anexos13.sdo_inicial = 0.
END.

FOR EACH ttAnexos WHERE sdoFinal <> 0 NO-LOCK:
    FIND FIRST anexos WHERE anexos.agencia = ttAnexos.agencia
                        AND anexos.cuenta = ttAnexos.cuenta
                        AND anexos.nit = ttAnexos.nit
                        AND anexos.cen_costos = ttAnexos.cen_costos
                        AND anexos.ano = 2013 NO-ERROR.
    IF AVAILABLE anexos THEN
        anexos.sdo_inicial = ttAnexos.sdoFinal.
    ELSE DO:
        CREATE anexos.
        anexos.agencia = ttAnexos.agencia.
        anexos.cuenta = ttAnexos.cuenta.
        anexos.nit = ttAnexos.nit.
        anexos.cen_costos = ttAnexos.cen_costos.
        anexos.ano = 2013.
        anexos.sdo_inicial = ttAnexos.sdoFinal.
    END.

    FIND FIRST anexos13 WHERE anexos13.agencia = ttAnexos.agencia
                          AND anexos13.cuenta = ttAnexos.cuenta
                          AND anexos13.nit = ttAnexos.nit
                          AND anexos13.cen_costos = ttAnexos.cen_costos
                          AND anexos13.ano = 2013 NO-ERROR.
    IF AVAILABLE anexos13 THEN
        anexos13.sdo_inicial = ttAnexos.sdoFinal.
    ELSE DO:
        CREATE anexos13.
        anexos13.agencia = ttAnexos.agencia.
        anexos13.cuenta = ttAnexos.cuenta.
        anexos13.nit = ttAnexos.nit.
        anexos13.cen_costos = ttAnexos.cen_costos.
        anexos13.ano = 2013.
        anexos13.sdo_inicial = ttAnexos.sdoFinal.
    END.
END.

EMPTY TEMP-TABLE ttAnexos.

FOR EACH anexos WHERE anexos.ano = 2013
                  AND anexos.cuenta = vCuenta
                  AND anexos.agencia = vAgencia NO-LOCK:
    CREATE ttAnexos.
    BUFFER-COPY anexos TO ttAnexos.

    ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.sdo_inicial.

    DO cont = 1 TO 12:
        IF cuentas.naturaleza = "DB" THEN
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.db[cont] - ttAnexos.cr[cont].
        ELSE
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.cr[cont] - ttAnexos.db[cont].
    END.
END.

/* *** */

FOR EACH anexos WHERE anexos.cuenta = vCuenta
                  AND anexos.ano = 2014
                  AND anexos.agencia = vAgencia:
    anexos.sdo_inicial = 0.
END.

FOR EACH anexos13 WHERE anexos13.cuenta = vCuenta
                    AND anexos13.ano = 2014
                    AND anexos13.agencia = vAgencia:
    anexos13.sdo_inicial = 0.
END.

FOR EACH ttAnexos WHERE sdoFinal <> 0 NO-LOCK:
    FIND FIRST anexos WHERE anexos.agencia = ttAnexos.agencia
                        AND anexos.cuenta = ttAnexos.cuenta
                        AND anexos.nit = ttAnexos.nit
                        AND anexos.cen_costos = ttAnexos.cen_costos
                        AND anexos.ano = 2014 NO-ERROR.
    IF AVAILABLE anexos THEN
        anexos.sdo_inicial = ttAnexos.sdoFinal.
    ELSE DO:
        CREATE anexos.
        anexos.agencia = ttAnexos.agencia.
        anexos.cuenta = ttAnexos.cuenta.
        anexos.nit = ttAnexos.nit.
        anexos.cen_costos = ttAnexos.cen_costos.
        anexos.ano = 2014.
        anexos.sdo_inicial = ttAnexos.sdoFinal.
    END.

    FIND FIRST anexos13 WHERE anexos13.agencia = ttAnexos.agencia
                          AND anexos13.cuenta = ttAnexos.cuenta
                          AND anexos13.nit = ttAnexos.nit
                          AND anexos13.cen_costos = ttAnexos.cen_costos
                          AND anexos13.ano = 2014 NO-ERROR.
    IF AVAILABLE anexos13 THEN
        anexos13.sdo_inicial = ttAnexos.sdoFinal.
    ELSE DO:
        CREATE anexos13.
        anexos13.agencia = ttAnexos.agencia.
        anexos13.cuenta = ttAnexos.cuenta.
        anexos13.nit = ttAnexos.nit.
        anexos13.cen_costos = ttAnexos.cen_costos.
        anexos13.ano = 2014.
        anexos13.sdo_inicial = ttAnexos.sdoFinal.
    END.
END.

EMPTY TEMP-TABLE ttAnexos.

FOR EACH anexos WHERE anexos.ano = 2014
                  AND anexos.cuenta = vCuenta
                  AND anexos.agencia = vAgencia NO-LOCK:
    CREATE ttAnexos.
    BUFFER-COPY anexos TO ttAnexos.

    ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.sdo_inicial.

    DO cont = 1 TO 12:
        IF cuentas.naturaleza = "DB" THEN
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.db[cont] - ttAnexos.cr[cont].
        ELSE
            ttAnexos.sdoFinal = ttAnexos.sdoFinal + ttAnexos.cr[cont] - ttAnexos.db[cont].
    END.
END.


/* *** */
FOR EACH anexos WHERE anexos.cuenta = vCuenta
                  AND anexos.ano = 2015
                  AND anexos.agencia = vAgencia:
    anexos.sdo_inicial = 0.
END.

FOR EACH ttAnexos WHERE sdoFinal <> 0 NO-LOCK:
    FIND FIRST anexos WHERE anexos.agencia = ttAnexos.agencia
                        AND anexos.cuenta = ttAnexos.cuenta
                        AND anexos.nit = ttAnexos.nit
                        AND anexos.cen_costos = ttAnexos.cen_costos
                        AND anexos.ano = 2015 NO-ERROR.
    IF AVAILABLE anexos THEN
        anexos.sdo_inicial = ttAnexos.sdoFinal.
    ELSE DO:
        CREATE anexos.
        anexos.agencia = ttAnexos.agencia.
        anexos.cuenta = ttAnexos.cuenta.
        anexos.nit = ttAnexos.nit.
        anexos.cen_costos = ttAnexos.cen_costos.
        anexos.ano = 2015.
        anexos.sdo_inicial = ttAnexos.sdoFinal.
    END.
END.
