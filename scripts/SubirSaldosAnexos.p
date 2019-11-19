DEFINE VAR cont AS INTEGER.
DEFINE VAR saldoFinal AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER INITIAL "16909501".
DEFINE VAR vAgencia AS INTEGER INITIAL 2.

DEFINE TEMP-TABLE anexosini
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD saldo AS DECIMAL.

/* Subimos el archivo */
INPUT FROM d:\Leonardo\AnexosIni.csv.
REPEAT :
    CREATE anexosIni.
    IMPORT DELIMITER ";" anexosIni.
END.

/* Borramos los db y cr de enero y febrero de 2012 */
FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,LENGTH(vCuenta)) = vCuenta AND anexos.ano = 2011 AND anexos.agencia = vAgencia:
    anexos.sdo_inicial = 0.
    anexos.db[1] = 0.
    anexos.cr[1] = 0.
    anexos.db[2] = 0.
    anexos.cr[2] = 0.
END.

FOR EACH anexos13 WHERE SUBSTRING(anexos13.cuenta,1,LENGTH(vCuenta)) = vCuenta AND anexos13.ano = 2011 AND anexos13.agencia = vAgencia:
    anexos13.sdo_inicial = 0.
    anexos13.db[1] = 0.
    anexos13.cr[1] = 0.
    anexos13.db[2] = 0.
    anexos13.cr[2] = 0.
END.

FOR EACH anexosIni NO-LOCK:
    /* Subimos como saldo inicial de 2011 el saldo al 28 de febrero */
    FIND FIRST anexos WHERE anexos.agencia = anexosIni.agencia
                        AND anexos.nit = anexosIni.nit
                        AND anexos.cuenta = anexosIni.cuenta
                        AND anexos.ano = 2011 NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        CREATE anexos.
        ASSIGN anexos.nit = anexosIni.nit
               anexos.agencia = anexosIni.agencia
               anexos.cen_costos = 999
               anexos.cuenta = anexosIni.cuenta
               anexos.ano = 2011.
    END.

    anexos.sdo_inicial = anexosIni.saldo.

    FIND FIRST anexos13 WHERE anexos13.agencia = anexosIni.agencia
                          AND anexos13.nit = anexosIni.nit
                          AND anexos13.cuenta = anexosIni.cuenta
                          AND anexos13.ano = 2011 NO-ERROR.
    IF NOT AVAILABLE anexos13 THEN DO:
        CREATE anexos13.
        ASSIGN anexos13.nit = anexosIni.nit
               anexos13.agencia = anexosIni.agencia
               anexos13.cen_costos = 999
               anexos13.cuenta = anexosIni.cuenta
               anexos13.ano = 2011.
    END.

    anexos13.sdo_inicial = anexosIni.saldo.

    /* Pasamos el saldo final como saldo inicial de 2012 */
    saldoFinal = anexos13.sdo_inicial.
    
    DO cont = 1 TO 12:
        saldoFinal = saldoFinal + anexos13.db[cont] - anexos13.cr[cont].
    END.

    FIND FIRST anexos WHERE anexos.agencia = anexosIni.agencia
                        AND anexos.nit = anexosIni.nit
                        AND anexos.cuenta = anexosIni.cuenta
                        AND anexos.ano = 2012 NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        CREATE anexos.
        ASSIGN anexos.nit = anexosIni.nit
               anexos.agencia = anexosIni.agencia
               anexos.cen_costos = 999
               anexos.cuenta = anexosIni.cuenta
               anexos.ano = 2012.
    END.

    anexos.sdo_inicial = saldoFinal.

    FIND FIRST anexos13 WHERE anexos13.agencia = anexosIni.agencia
                          AND anexos13.nit = anexosIni.nit
                          AND anexos13.cuenta = anexosIni.cuenta
                          AND anexos13.ano = 2012 NO-ERROR.
    IF NOT AVAILABLE anexos13 THEN DO:
        CREATE anexos13.
        ASSIGN anexos13.nit = anexosIni.nit
               anexos13.agencia = anexosIni.agencia
               anexos13.cen_costos = 999
               anexos13.cuenta = anexosIni.cuenta
               anexos13.ano = 2012.
    END.

    anexos13.sdo_inicial = saldoFinal.

    /* Pasamos el saldo final como saldo inicial de 2013 */
    saldoFinal = anexos13.sdo_inicial.
    
    DO cont = 1 TO 12:
        saldoFinal = saldoFinal + anexos13.db[cont] - anexos13.cr[cont].
    END.

    FIND FIRST anexos WHERE anexos.agencia = anexosIni.agencia
                        AND anexos.nit = anexosIni.nit
                        AND anexos.cuenta = anexosIni.cuenta
                        AND anexos.ano = 2013 NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        CREATE anexos.
        ASSIGN anexos.nit = anexosIni.nit
               anexos.agencia = anexosIni.agencia
               anexos.cen_costos = 999
               anexos.cuenta = anexosIni.cuenta
               anexos.ano = 2013.
    END.

    anexos.sdo_inicial = saldoFinal.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
