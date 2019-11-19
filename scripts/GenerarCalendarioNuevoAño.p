DEFINE TEMP-TABLE tcal LIKE calendario.
DEFINE VAR cont AS INTEGER.
DEFINE var fecha AS DATE INITIAL 01/01/2013.

FIND FIRST calendario WHERE agencia = 4 AND ano = 2012 NO-LOCK NO-ERROR.
/*DISPLAY calendario WITH 1 COL.*/

CREATE tcal.
BUFFER-COPY calendario TO tcal.

tcal.estado = 1.
tcal.ano = 2014.

/*DISPLAY tcal WITH 1 COL.*/

DO cont = 1 TO 365:
    CREATE calendario.
    BUFFER-COPY tcal TO calendario.

    calendario.dia = DAY(fecha).
    calendario.mes = MONTH(fecha).
    calendario.ano = YEAR(fecha).

    fecha = fecha + 1.
END.

