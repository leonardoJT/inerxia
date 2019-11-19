/*
    Levantar y poner restriccion a cuentas para solo manejo de taquilla.
*/


DEFINE TEMP-TABLE TC
    FIELDS  cuenta          LIKE cuentas.cuenta.

INPUT FROM "c:\info_fodun\CuentasLevantarTaquilla.csv".
REPEAT:
    CREATE TC.
    IMPORT DELIMITER ";" TC.
END.
INPUT CLOSE.

/*Levantar restriccion*/
OUTPUT TO "c:\info_fodun\CuentasTaquilla.csv".
FOR EACH TC WHERE TC.cuenta NE "" NO-LOCK BREAK BY TC.cuenta:
    IF FIRST-OF(TC.cuenta) THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta EQ TC.cuenta EXCLUSIVE-LOCK NO-ERROR.
        UPDATE Cuentas.id_NoMvto = FALSE.
        EXPORT DELIMITER ";" TC.cuenta.
    END.
END.
OUTPUT CLOSE.

/*Colocar restriccion*/
