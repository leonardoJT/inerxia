/*
    actualiza datos cdat
*/

DEF VAR ano AS CHAR.
DEF VAR mes AS CHAR.
DEF VAR dia AS CHAR.
DEFINE VARIABLE vi AS INTEGER     NO-UNDO.

FUNCTION fecChToDa RETURNS DATE (INPUT fecha AS CHAR):   
    ano = SUBSTRING(fecha,1,4,"CHARACTER").
    mes = SUBSTRING(fecha,5,2,"CHARACTER").
    dia = SUBSTRING(fecha,7,2,"CHARACTER").
    RETURN DATE(int(mes),int(dia),int(ano)).
END.


DEFINE TEMP-TABLE TA
    FIELDS cue_ahorros          LIKE ahorros.cue_ahorros
    FIELDS fec_apertura         AS CHAR
    FIELDS tasa                 LIKE ahorros.tasa
    FIELDS Fec_UltLiquidacion   AS CHAR 
    FIELDS Dias_Causar          LIKE Ahorros.Dias_Causar 
    FIELDS Int_Causado          LIKE Ahorros.Int_Causado 
    FIELDS sdo_actual           LIKE ahorros.sdo_disponible
    FIELDS Fec_Vencimiento      AS CHAR
    INDEX idx cue_ahorros
    INDEX cue_ahorros IS UNIQUE cue_ahorros.


INPUT FROM "c:\info_fodun\cdat.csv".
REPEAT:
    CREATE TA.
    IMPORT DELIMITER ";" TA.
    ASSIGN vi = vi + 1.
END.
INPUT CLOSE.
MESSAGE "Cargados" vi
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


OUTPUT TO "C:\info_fodun\cdat_noactualizado.csv".
ASSIGN vi = 0.
FOR EACH TA NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.cue_ahorros EQ TA.cue_ahorros AND 
                            (ahorros.cod_ahorro EQ 5 OR ahorros.cod_ahorro EQ 6) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        UPDATE ahorros.fec_apertura         = fecChToDa(TA.fec_apertura)
               Ahorros.Fec_UltLiquidacion   = fecChToDa(TA.Fec_UltLiquidacion)
               Ahorros.Fec_Vencimiento      = fecChToDa(TA.Fec_Vencimiento)
               Ahorros.Fec_ProLiquidacion   = ahorros.Fec_Vencimiento - 1.
    END.
    ELSE DO:
        EXPORT DELIMITER ";" TA.
    END.
END.
OUTPUT CLOSE.
