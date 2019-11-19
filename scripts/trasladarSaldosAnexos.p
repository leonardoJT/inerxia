DEFINE VAR vAno AS INTEGER INITIAL 2017.
DEFINE VAR vCuenta AS CHARACTER INITIAL "27100502".
DEFINE VAR vSaldo AS DECIMAL.
DEFINE VAR cont AS INTEGER.

DEFINE BUFFER bfrAnexos FOR anexos.

FOR EACH anexos WHERE anexos.ano = vAno + 1
                  AND anexos.cuenta = vCuenta:
    anexos.sdo_inicial = 0.
END.

FOR EACH bfrAnexos WHERE bfrAnexos.ano = vAno
                     AND bfrAnexos.cuenta = vCuenta NO-LOCK:
    vSaldo = bfrAnexos.sdo_inicial.

    DO cont = 1 TO 12:
        vSaldo = vSaldo + bfrAnexos.cr[cont] - bfrAnexos.db[cont].
    END.

    DISPLAY bfrAnexos.nit vSaldo FORMAT "->>>,>>>,>>>,>>9".

    IF vSaldo <> 0 THEN DO:
        FIND FIRST anexos WHERE anexos.nit = bfrAnexos.nit
                            AND anexos.cuenta = bfrAnexos.cuenta
                            AND anexos.agencia = bfrAnexos.agencia
                            AND anexos.cen_costos = bfrAnexos.cen_costos
                            AND anexos.ano = vAno + 1 NO-ERROR.
        IF AVAILABLE anexos THEN
            anexos.sdo_inicial = vSaldo.
    END.
END.

/*
DEFINE VAR vAno AS INTEGER INITIAL 2016.
DEFINE VAR vCuenta AS CHARACTER INITIAL "27101001".
DEFINE VAR nuevaCuenta AS CHARACTER.
DEFINE VAR vSaldo AS DECIMAL.
DEFINE VAR cont AS INTEGER.

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

FIND FIRST homologacion WHERE homologacion.cuentaSES = vCuenta NO-LOCK NO-ERROR.
IF AVAILABLE homologacion THEN
    nuevaCuenta = homologacion.cuentaNIIF.
ELSE DO:
    MESSAGE "Falló"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    LEAVE.
END.


DEFINE BUFFER bfrAnexos FOR anexos.

FOR EACH anexos WHERE anexos.ano = vAno + 1
                  AND anexos.cuenta = nuevaCuenta:
    anexos.sdo_inicial = 0.
END.

FOR EACH bfrAnexos WHERE bfrAnexos.ano = vAno
                     AND bfrAnexos.cuenta = vCuenta NO-LOCK:
    vSaldo = bfrAnexos.sdo_inicial.

    DO cont = 1 TO 12:
        vSaldo = vSaldo + bfrAnexos.cr[cont] - bfrAnexos.db[cont].
    END.

    DISPLAY bfrAnexos.nit vSaldo FORMAT "->>>,>>>,>>>,>>9".

    IF vSaldo <> 0 THEN DO:
        FIND FIRST anexos WHERE anexos.nit = bfrAnexos.nit
                            AND anexos.cuenta = nuevaCuenta
                            AND anexos.agencia = bfrAnexos.agencia
                            AND anexos.cen_costos = bfrAnexos.cen_costos
                            AND anexos.ano = vAno + 1 NO-ERROR.
        IF AVAILABLE anexos THEN
            anexos.sdo_inicial = vSaldo.
    END.
END.

*/
