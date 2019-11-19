DEFINE TEMP-TABLE cifin
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER
    FIELD agencia AS INTEGER
    FIELD estado AS INTEGER
    FIELD saldo AS DECIMAL
    FIELD agenciaSfg AS INTEGER
    FIELD estadoSfg AS INTEGER
    FIELD saldoSfg AS DECIMAL
    FIELD vResult AS CHARACTER.

INPUT FROM d:\Leonardo\cifin.csv.
REPEAT :
    CREATE cifin.

    IMPORT DELIMITER ";" cifin NO-ERROR.

    /*IF ERROR-STATUS:ERROR THEN
        DELETE cifin.*/
END.
INPUT CLOSE.

OUTPUT TO d:\Leonardo\cifin2.csv.
FOR EACH cifin NO-LOCK:
    IF cifin.estado = 1 THEN DO:
        FIND FIRST creditos WHERE creditos.nit = cifin.nit
                              AND creditos.num_credito = cifin.num_credito NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN DO:
            cifin.agenciaSfg = creditos.agencia.
        
            IF creditos.estado = 2 THEN
                cifin.estadoSfg = 1.

            IF creditos.estado = 3 THEN
                cifin.estadoSfg = 7.

            cifin.saldo = creditos.sdo_capital.
        END.
        ELSE
            cifin.vResult = "NoExiste/Cancelado".
    END.
    ELSE
        cifin.vResult = "CANCELADO".

    EXPORT DELIMITER ";" cifin.
END.
OUTPUT CLOSE.
