DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE TEMP-TABLE homologacion
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreNIIF AS CHARACTER
    FIELD cuentaSES AS CHARACTER
    FIELD nombreSES AS CHARACTER
    INDEX idx cuentaSES.

DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR vfecha AS DATE INITIAL 01/10/2017.
DEFINE VAR cont AS INTEGER.

INPUT FROM d:\Leonardo\CatalogoNIIF_vs_SES.csv.
REPEAT:
    CREATE homologacion.
    IMPORT DELIMITER ";" homologacion.
END.
INPUT CLOSE.

DEFINE VAR time1 AS INTEGER.
DEFINE VAR time2 AS INTEGER.

time1 = TIME.

/* Homologamos todo el movimiento contable */
DO cont = 10 TO 1 BY -1:
    FOR EACH agencias NO-LOCK BY agencias.agencia:
        FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                                AND mov_contable.fec_contable = vFecha:
            IF mov_contable.fec_grabacion <= 10/08/2017 THEN DO:
                FIND FIRST homologacion WHERE homologacion.cuentaSES = mov_contable.cuenta NO-LOCK NO-ERROR.
                IF AVAILABLE homologacion THEN
                    mov_contable.cuenta = homologacion.cuentaNIIF.
                ELSE
                    DELETE mov_contable.
            END.
        END.
    END.

    vFecha = vFecha - 1.
END.

time2 = TIME.

MESSAGE vFecha STRING(time2 - time1,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
