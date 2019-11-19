DISABLE TRIGGERS FOR LOAD OF anexos.

DEFINE TEMP-TABLE ttAnexos LIKE anexos.
DEFINE TEMP-TABLE tempAnexos LIKE anexos.

DEFINE TEMP-TABLE homologacion
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreNIIF AS CHARACTER
    FIELD cuentaSES AS CHARACTER
    FIELD nombreSES AS CHARACTER.

DEFINE VAR vCuenta AS CHARACTER.

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
FOR EACH anexos WHERE anexos.ano = 2017 NO-LOCK:
    FIND FIRST homologacion WHERE homologacion.cuentaSES = anexos.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE homologacion THEN DO:
        FIND FIRST ttAnexos WHERE ttanexos.agencia = anexos.agencia
                              AND ttanexos.cen_costos = anexos.cen_costos
                              AND ttanexos.cuenta = homologacion.cuentaNIIF
                              AND ttAnexos.nit = anexos.nit
                              AND ttanexos.ano = anexos.ano NO-ERROR.
        IF NOT AVAILABLE ttAnexos THEN DO:
            EMPTY TEMP-TABLE tempAnexos.
            CREATE tempAnexos.
            BUFFER-COPY anexos TO tempAnexos.
            tempAnexos.cuenta = homologacion.cuentaNIIF.

            CREATE ttAnexos.
            BUFFER-COPY tempAnexos TO ttAnexos.
        END.
        ELSE DO:
            ttAnexos.sdo_inicial = ttAnexos.sdo_inicial + anexos.sdo_inicial.
            ttAnexos.db[1] = ttAnexos.db[1] + anexos.db[1].
            ttAnexos.db[2] = ttAnexos.db[2] + anexos.db[2].
            ttAnexos.db[3] = ttAnexos.db[3] + anexos.db[3].
            ttAnexos.db[4] = ttAnexos.db[4] + anexos.db[4].
            ttAnexos.db[5] = ttAnexos.db[5] + anexos.db[5].
            ttAnexos.db[6] = ttAnexos.db[6] + anexos.db[6].
            ttAnexos.db[7] = ttAnexos.db[7] + anexos.db[7].
            ttAnexos.db[8] = ttAnexos.db[8] + anexos.db[8].
            ttAnexos.db[9] = ttAnexos.db[9] + anexos.db[9].
            ttAnexos.db[10] = ttAnexos.db[10] + anexos.db[10].
            ttAnexos.cr[1] = ttAnexos.cr[1] + anexos.cr[1].
            ttAnexos.cr[2] = ttAnexos.cr[2] + anexos.cr[2].
            ttAnexos.cr[3] = ttAnexos.cr[3] + anexos.cr[3].
            ttAnexos.cr[4] = ttAnexos.cr[4] + anexos.cr[4].
            ttAnexos.cr[5] = ttAnexos.cr[5] + anexos.cr[5].
            ttAnexos.cr[6] = ttAnexos.cr[6] + anexos.cr[6].
            ttAnexos.cr[7] = ttAnexos.cr[7] + anexos.cr[7].
            ttAnexos.cr[8] = ttAnexos.cr[8] + anexos.cr[8].
            ttAnexos.cr[9] = ttAnexos.cr[9] + anexos.cr[9].
            ttAnexos.cr[10] = ttAnexos.cr[10] + anexos.cr[10].
        END.
    END.
END.

/* Borramos los registros de la tabla anexos */
FOR EACH anexos WHERE anexos.ano = 2017:
    DELETE anexos.
END.

/* Poblamos de nuevo la tabla sal_cuenta a partir de la temporal */
FOR EACH ttAnexos NO-LOCK:
    CREATE anexos.
    BUFFER-COPY ttAnexos TO anexos.
END.

time2 = TIME.

MESSAGE "Fin" STRING(time2 - time1,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
