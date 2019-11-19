/*Actualiza fechas de vencimiento*/

DEFINE VARIABLE vi AS INTEGER     NO-UNDO.

DEF VAR ano AS CHAR.
DEF VAR mes AS CHAR.
DEF VAR dia AS CHAR.

DEFINE TEMP-TABLE TA
    FIELDS nit          LIKE ahorros.nit
    FIELDS cue_ahorros  LIKE ahorros.cue_ahorros
    FIELDS fecha        AS CHARACTER
    INDEX Idx NIT cue_ahorros.


FUNCTION Fec RETURNS DATE (INPUT fecha AS CHAR):   
    ano = SUBSTRING(fecha,1,4,"CHARACTER").
    mes = SUBSTRING(fecha,5,2,"CHARACTER").
    dia = SUBSTRING(fecha,7,2,"CHARACTER").
    RETURN DATE(int(mes),int(dia),int(ano)).
END.


INPUT FROM "c:\migrar\AhorrosAllFecTerminacion.csv".

REPEAT:
    CREATE TA.
    IMPORT DELIMITER ";" TA.
END.

FOR EACH TA WHERE nit NE "" AND cue_ahorros NE "" NO-LOCK:
    IF INTEGER(TA.fecha) EQ 0 THEN NEXT.
    FIND FIRST ahorros WHERE ahorros.nit EQ TA.nit AND ahorros.cue_ahorros EQ TA.cue_ahorros EXCLUSIVE-LOCK NO-ERROR.
    UPDATE ahorros.fec_vencimiento = fec(fecha).
END.

MESSAGE vi
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
