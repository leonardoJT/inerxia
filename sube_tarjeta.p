DISABLE TRIGGERS FOR LOAD OF creditos.
DEFINE VAR xcre LIKE creditos.num_credito.
DEFINE VAR xtot LIKE creditos.sdo_capital.
DEFINE TEMP-TABLE t
   FIELD age      LIKE plastico.agencia
   FIELD nro      LIKE plastico.num_plastico.

   

INPUT FROM c:\tarjetas.csv.
REPEAT:
    CREATE t.
    IMPORT DELIMITER ";" t.
END.
INPUT CLOSE.

FOR EACH t:
    FIND FIRST plastico WHERE  Num_Plastico EQ t.nro AND
                               Agencia      EQ t.age NO-ERROR.
    IF NOT AVAILABLE(plastico) THEN DO:
       CREATE plastico .
       ASSIGN plastico.num_plastico = t.nro
              plastico.agencia      = t.age 
              plastico.cue_ahorros   = "0".
    END.
    ELSE DISPLAY "ya existe " age nro.
END.
