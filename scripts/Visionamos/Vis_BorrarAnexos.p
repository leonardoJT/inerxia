DEFINE TEMP-TABLE tt1
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD sdo_inicial AS DECIMAL
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    FIELD sdo_final AS DECIMAL
    FIELD id AS INTEGER.

DEFINE TEMP-TABLE tt2
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD sdo_inicial AS DECIMAL
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL
    FIELD sdo_final AS DECIMAL
    FIELD id AS INTEGER
    INDEX idx1 id.

DEFINE TEMP-TABLE borrar
    FIELD id AS INTEGER.

INPUT FROM d:\Leonardo\anexos.csv.
REPEAT:
    CREATE tt1.
    IMPORT DELIMITER ";" tt1.

    CREATE tt2.
    IMPORT DELIMITER ";" tt2.
END.
INPUT CLOSE.

FOR EACH tt1 NO-LOCK BY id:
    FIND FIRST tt2 WHERE tt2.id = tt1.id NO-ERROR.
    IF AVAILABLE tt2 THEN
        DELETE tt2.

    FOR EACH tt2 WHERE tt2.agencia = tt1.agencia
                   AND tt2.cuenta = tt1.cuenta
                   AND tt2.nit = tt1.nit
                   AND tt2.id <> tt1.id:
        CREATE borrar.
        borrar.id = tt2.id.
        DELETE tt2.
    END.
END.

OUTPUT TO d:\Leonardo\borrar.txt.
FOR EACH borrar NO-LOCK:
    PUT STRING(borrar.id) + "," SKIP.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
