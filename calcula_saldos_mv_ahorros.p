DEFINE VARIABLE vdaFecIni AS DATE        NO-UNDO.
DEFINE VARIABLE vdaFecFin AS DATE        NO-UNDO.
DEFINE VARIABLE totap     AS DECIMAL  INIT 0   NO-UNDO.
DEFINE VARIABLE vtipoo    AS INTEGER NO-UNDO.
DEFINE VARIABLE vdeValorCr LIKE   mov_ahorros.Val_Efectivo NO-UNDO.
DEFINE VARIABLE vdeValorDb LIKE   mov_ahorros.Val_Efectivo NO-UNDO.
DEFINE VARIABLE vdeValorNe LIKE   mov_ahorros.Val_Efectivo NO-UNDO.

/* ---------------- TABLA TEMPORAL    */
DEFINE TEMP-TABLE trevaloriza
    FIELD tnit LIKE ahorros.nit
    FIELD tage LIKE ahorros.agencia
    FIELD salant LIKE mov_ahorros.Val_Efectivo COLUMN-LABEL "Saldo Anterior"  FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD saldoi LIKE mov_ahorros.Val_Efectivo COLUMN-LABEL "Saldo Inicial"   FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD saldb  LIKE mov_ahorros.Val_Efectivo COLUMN-LABEL "Debitos"         FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD salCr  LIKE mov_ahorros.Val_Efectivo COLUMN-LABEL "Creditos"        FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD salnue LIKE mov_ahorros.Val_Efectivo COLUMN-LABEL "Saldo Nuevo"     FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD salult LIKE Ahorros.Sdo_UltLiquidacion COLUMN-LABEL "Saldo UltLiquidacion"   FORMAT "->>>,>>>,>>>,>>9.99".

INPUT FROM c:\INFO_juriscoop\prueba1.csv.

REPEAT:
    CREATE trevaloriza.
    IMPORT DELIMITER ";"  trevaloriza.
END.

/* ----------------------  Inicio Cargue Movimiento  */ 
FOR EACH trevaloriza :
    FOR EACH mov_ahorros WHERE  
             nit = tnit AND Mov_Ahorros.Cod_Ahorro = 5 USE-INDEX XIE4Mov_ahorros
             NO-LOCK BREAK BY nit :
        FIND FIRST Operacion WHERE  Operacion.Cod_Operacion = Mov_Ahorros.Cod_Operacion NO-LOCK NO-ERROR.
        IF AVAILABLE Operacion
            THEN DO:
                IF Operacion.Tipo_Operacion EQ 1
                THEN ASSIGN vtipoo = 1. 
                ELSE ASSIGN vtipoo = 2.
        END.
        ASSIGN 
            vdeValorCr = 0
            vdeValorDb = 0.
        CASE vtipoo:
            WHEN 1 THEN ASSIGN vdeValorCr = (mov_ahorros.Val_Efectivo + mov_ahorros.Val_Cheque).
            WHEN 2 THEN ASSIGN vdeValorDb = (mov_ahorros.Val_Efectivo + mov_ahorros.Val_Cheque).
            OTHERWISE
            ASSIGN vdeValorCr = (mov_ahorros.Val_Efectivo + mov_ahorros.Val_Cheque).
        END CASE.
        IF FIRST-OF(Mov_Ahorros.nit)  THEN ASSIGN trevaloriza.saldoi = Mov_Ahorros.Sdo_Disponible.
        ASSIGN vdeValorNe = vdeValorDb - vdeValorCr
            trevaloriza.saldb = trevaloriza.saldb + vdeValorDb
            trevaloriza.salcr = trevaloriza.salcr + vdeValorCr
            trevaloriza.tage   = mov_ahorros.agencia.
    END.
END.

OUTPUT TO "c:\INFO_juriscoop\sal_ahorrosu.txt".
EXPORT DELIMITER ";"
"Nit"         
"Agencia" 
"Ahorros.Sdo_Disponible"
"MV_Ahorros.Sdo_Disponible_sldoini" 
"Debitos"       
"Creditos"       
"Saldo UltLiquidacion"
"Saldo Nuevo".   

FOR EACH trevaloriza :
    FIND FIRST Ahorros WHERE Ahorros.nit = tnit AND Ahorros.Cod_Ahorro = 5 
             EXCLUSIVE-LOCK  NO-ERROR.
    IF AVAILABLE Ahorros 
        THEN  ASSIGN trevaloriza.salant = Ahorros.Sdo_Disponible
                     trevaloriza.salult = Ahorros.Sdo_UltLiquidacion.

/* Para Seleccionar el Ultimo Movimiento y Sacar el saldo Actual*/ 
    FIND LAST Ahorros WHERE Ahorros.nit = tnit AND Ahorros.Cod_Ahorro = 5 
             NO-LOCK  NO-ERROR.
    IF AVAILABLE Ahorros 
        THEN  ASSIGN trevaloriza.salant = Ahorros.Sdo_Disponible.

/*----------------------------------------------------------------------------------------- */ 
     ASSIGN trevaloriza.salnue = trevaloriza.salult - trevaloriza.saldb + trevaloriza.salCr.
     EXPORT DELIMITER ";"
     tnit 
     tage 
     salant
     saldoi
     saldb 
     salCr 
     salult
     salnue
     SKIP.
    UPDATE Ahorros.Sdo_Disponible = salnue.
END.

MESSAGE "Actualizada la revalorización de aportes" SKIP
        "Creado archivo..." SKIP
        "c:\INFO_juriscoop\sal_ahorrosu.txt"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
