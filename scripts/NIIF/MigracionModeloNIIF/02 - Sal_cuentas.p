DISABLE TRIGGERS FOR LOAD OF sal_cuenta.

DEFINE TEMP-TABLE ttsalCuenta LIKE sal_cuenta.
DEFINE TEMP-TABLE tempSalCuenta LIKE sal_cuenta.

DEFINE TEMP-TABLE homologacion
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreNIIF AS CHARACTER
    FIELD cuentaSES AS CHARACTER
    FIELD nombreSES AS CHARACTER.

INPUT FROM d:\Leonardo\CatalogoNIIF_vs_SES.csv.
REPEAT:
    CREATE homologacion.
    IMPORT DELIMITER ";" homologacion.
END.
INPUT CLOSE.

DEFINE VAR time1 AS INTEGER.
DEFINE VAR time2 AS INTEGER.

time1 = TIME.

/* Pasamos todos los registros a una temporal */
FOR EACH sal_cuenta_NIIF WHERE sal_cuenta_NIIF.ano = 2017 NO-LOCK:
    FIND FIRST ttSalCuenta WHERE ttSalCuenta.agencia = sal_cuenta_NIIF.agencia
                             AND ttSalCuenta.cen_costos = sal_cuenta_NIIF.cen_costos
                             AND ttSalCuenta.cuenta = sal_cuenta_NIIF.cuenta
                             AND ttSalCuenta.ano = sal_cuenta_NIIF.ano NO-ERROR.
    IF NOT AVAILABLE ttSalCuenta THEN DO:
        CREATE ttSalCuenta.
        ttSalCuenta.agencia = sal_cuenta_NIIF.agencia.
        ttSalCuenta.Ano = sal_cuenta_NIIF.ano.
        ttSalCuenta.cen_costos = sal_cuenta_NIIF.Cen_Costos.
        ttSalCuenta.cuenta = sal_cuenta_NIIF.Cuenta.
        ttSalCuenta.sal_inicial = sal_cuenta_NIIF.sal_inicial.
    END.
    ELSE
        ttSalCuenta.sal_inicial = ttSalCuenta.sal_inicial + sal_cuenta_NIIF.sal_inicial.
END.

FOR EACH sal_cuenta WHERE sal_cuenta.ano = 2017 NO-LOCK:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = sal_cuenta.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN DO:
        FIND FIRST ttSalCuenta WHERE ttSalCuenta.agencia = sal_cuenta.agencia
                                 AND ttSalCuenta.cen_costos = sal_cuenta.cen_costos
                                 AND ttSalCuenta.cuenta = homologacion.cuentaNIIF
                                 AND ttSalCuenta.ano = sal_cuenta.ano NO-ERROR.
        IF NOT AVAILABLE ttSalCuenta THEN DO:
            CREATE ttSalCuenta.
            BUFFER-COPY sal_cuenta TO ttSalCuenta.
            ttSalCuenta.cuenta = homologacion.cuentaNIIF.
        END.
        ELSE DO:
            ttSalCuenta.db[1] = ttSalCuenta.db[1] + sal_cuenta.db[1].
            ttSalCuenta.db[2] = ttSalCuenta.db[2] + sal_cuenta.db[2].
            ttSalCuenta.db[3] = ttSalCuenta.db[3] + sal_cuenta.db[3].
            ttSalCuenta.db[4] = ttSalCuenta.db[4] + sal_cuenta.db[4].
            ttSalCuenta.db[5] = ttSalCuenta.db[5] + sal_cuenta.db[5].
            ttSalCuenta.db[6] = ttSalCuenta.db[6] + sal_cuenta.db[6].
            ttSalCuenta.db[7] = ttSalCuenta.db[7] + sal_cuenta.db[7].
            ttSalCuenta.db[8] = ttSalCuenta.db[8] + sal_cuenta.db[8].
            ttSalCuenta.db[9] = ttSalCuenta.db[9] + sal_cuenta.db[9].
            ttSalCuenta.db[10] = ttSalCuenta.db[10] + sal_cuenta.db[10].
            ttSalCuenta.cr[1] = ttSalCuenta.cr[1] + sal_cuenta.cr[1].
            ttSalCuenta.cr[2] = ttSalCuenta.cr[2] + sal_cuenta.cr[2].
            ttSalCuenta.cr[3] = ttSalCuenta.cr[3] + sal_cuenta.cr[3].
            ttSalCuenta.cr[4] = ttSalCuenta.cr[4] + sal_cuenta.cr[4].
            ttSalCuenta.cr[5] = ttSalCuenta.cr[5] + sal_cuenta.cr[5].
            ttSalCuenta.cr[6] = ttSalCuenta.cr[6] + sal_cuenta.cr[6].
            ttSalCuenta.cr[7] = ttSalCuenta.cr[7] + sal_cuenta.cr[7].
            ttSalCuenta.cr[8] = ttSalCuenta.cr[8] + sal_cuenta.cr[8].
            ttSalCuenta.cr[9] = ttSalCuenta.cr[9] + sal_cuenta.cr[9].
            ttSalCuenta.cr[10] = ttSalCuenta.cr[10] + sal_cuenta.cr[10].
        END.
    END.
END.

/* Borramos los registros de la tabla sal_cuenta */
FOR EACH sal_cuenta WHERE sal_cuenta.ano = 2017:
    DELETE sal_cuenta.
END.

/* Poblamos de nuevo la tabla sal_cuenta a partir de la temporal */
FOR EACH ttSalCuenta NO-LOCK:
    CREATE sal_cuenta.
    BUFFER-COPY ttSalCuenta TO sal_cuenta.
END.

time2 = TIME.

MESSAGE "Fin" STRING(time2 - time1,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
