DISABLE TRIGGERS FOR LOAD OF anexos.

DEFINE VAR ag AS INTEGER INITIAL 2.

DEFINE TEMP-TABLE nits
    FIELD cuenta AS CHARACTER
    FIELD cuenta1 AS CHARACTER
    FIELD nit AS CHARACTER FORMAT "X(12)"
    FIELD nombre AS CHARACTER
    FIELD saldo AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9.99".

INPUT FROM "C:\Info_Fodun\Leonardo\temp.txt".

REPEAT :
    CREATE nits.
    IMPORT DELIMITER ";" nits.
END.

FOR EACH nits WHERE saldo <> 0:
    FIND FIRST anexos WHERE anexos.agencia = ag
                        AND anexos.nit = nits.nit
                        AND anexos.cuenta = nits.cuenta1
                        AND anexos.ano = 2011
                        AND anexos.cen_costos = 999 NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        /*DISPLAY nits.nit nits.saldo.*/
        /*CREATE anexos.
        ASSIGN anexos.sdo_inicial = nits.saldo
               anexos.agencia = ag
               anexos.nit = nits.nit
               anexos.cuenta = nits.cuenta
               anexos.ano = 2011
               anexos.cen_costos = 999.*/
    END.
    ELSE DO:
        DISPLAY anexos.nit anexos.cuenta anexos.sdo_inicial nits.saldo WITH WIDTH 200.
        /*anexos.cuenta = "16252501".*/
        /*anexos.sdo_inicial = nits.saldo.*/
    END.

    /*FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE clientes THEN DO:
        CREATE clientes.
        ASSIGN clientes.nit = nits.nit
               clientes.nombre = nits.nombre.
    END.*/
END.

/*
FOR EACH sal_cuenta WHERE sal_cuenta.agencia = ag
                      AND sal_cuenta.ano = 2011
                      AND (SUBSTRING(sal_cuenta.cuenta,1,2) = "13" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "16500502" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "16609502" OR
                           SUBSTRING(sal_cuenta.cuenta,1,2) = "26" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "27959501")
                      AND sal_cuenta.cuenta <> "13100102"
                      AND sal_cuenta.sal_inicial <> 0 NO-LOCK:
    FIND FIRST anexos WHERE anexos.agencia = sal_cuenta.agencia
                        AND anexos.nit = "800112808-7"
                        AND anexos.cuenta = sal_cuenta.cuenta
                        AND anexos.ano = 2011
                        AND anexos.cen_costos = sal_cuenta.cen_costos NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        CREATE anexos.
        ASSIGN anexos.sdo_inicial = sal_cuenta.sal_inicial
               anexos.agencia = sal_cuenta.agencia
               anexos.nit = "800112808-7"
               anexos.cuenta = sal_cuenta.cuenta
               anexos.ano = sal_cuenta.ano
               anexos.cen_costos = sal_cuenta.cen_costos.
    END.
    ELSE DO:
        anexos.sdo_inicial = sal_cuenta.sal_inicial.
    END.
END.

FOR EACH sal_cuenta WHERE sal_cuenta.agencia = ag
                      AND sal_cuenta.ano = 2011
                      AND (SUBSTRING(sal_cuenta.cuenta,1,4) = "2442" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2445" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2447" OR
                           SUBSTRING(sal_cuenta.cuenta,1,2) = "25")
                      AND sal_cuenta.cuenta <> "25150501"
                      AND sal_cuenta.sal_inicial <> 0 NO-LOCK:
    FIND FIRST anexos WHERE anexos.agencia = sal_cuenta.agencia
                        AND anexos.nit = "800197268-4"
                        AND anexos.cuenta = sal_cuenta.cuenta
                        AND anexos.ano = 2011
                        AND anexos.cen_costos = sal_cuenta.cen_costos NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        CREATE anexos.
        ASSIGN anexos.sdo_inicial = sal_cuenta.sal_inicial
               anexos.agencia = sal_cuenta.agencia
               anexos.nit = "800197268-4"
               anexos.cuenta = sal_cuenta.cuenta
               anexos.ano = sal_cuenta.ano
               anexos.cen_costos = sal_cuenta.cen_costos.
    END.
    ELSE DO:
        anexos.sdo_inicial = sal_cuenta.sal_inicial.
    END.
END.
*/
