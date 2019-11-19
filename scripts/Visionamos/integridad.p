DEFINE TEMP-TABLE nov
    FIELD nro_producto AS INTEGER
    FIELD saldo AS DECIMAL.

DEFINE TEMP-TABLE movs
    FIELD nro_producto AS INTEGER
    FIELD desembolsos AS DECIMAL
    FIELD pagos AS DECIMAL.

DEFINE TEMP-TABLE dic
    FIELD nro_producto AS INTEGER
    FIELD saldo AS DECIMAL.

DEFINE TEMP-TABLE difs
    FIELD nro_producto AS INTEGER
    FIELD descuadre AS INTEGER.

INPUT FROM d:\Leonardo\noviembre.csv.
REPEAT :
    CREATE nov.
    IMPORT DELIMITER ";" nov.
END.
INPUT CLOSE.

INPUT FROM d:\Leonardo\movs.csv.
REPEAT :
    CREATE movs.
    IMPORT DELIMITER ";" movs.
END.
INPUT CLOSE.

INPUT FROM d:\Leonardo\diciembre.csv.
REPEAT :
    CREATE dic.
    IMPORT DELIMITER ";" dic.
END.
INPUT CLOSE.

FOR EACH nov NO-LOCK:
    FIND FIRST difs WHERE difs.nro_producto = nov.nro_producto NO-ERROR.
    IF NOT AVAILABLE difs THEN DO:
        CREATE difs.
        difs.nro_producto = nov.nro_producto.
        difs.descuadre = nov.saldo.
    END.

    FIND FIRST movs WHERE movs.nro_producto = nov.nro_producto NO-ERROR.
    IF AVAILABLE movs THEN DO:
        difs.descuadre = difs.descuadre + movs.desembolsos - movs.pagos.
        DELETE movs.
    END.

    FIND FIRST dic WHERE dic.nro_producto = nov.nro_producto NO-ERROR.
    IF AVAILABLE dic THEN DO:
        difs.descuadre = difs.descuadre - dic.saldo.
        DELETE dic.
    END.

    DELETE nov.
END.

FOR EACH dic NO-LOCK:
    FIND FIRST difs WHERE difs.nro_producto = dic.nro_producto NO-ERROR.
    IF NOT AVAILABLE difs THEN DO:
        CREATE difs.
        difs.nro_producto = dic.nro_producto.
        difs.descuadre = dic.saldo.
    END.

    FIND FIRST movs WHERE movs.nro_producto = dic.nro_producto NO-ERROR.
    IF AVAILABLE movs THEN DO:
        difs.descuadre = difs.descuadre - movs.desembolsos + movs.pagos.
        DELETE movs.
    END.

    FIND FIRST nov WHERE nov.nro_producto = dic.nro_producto NO-ERROR.
    IF AVAILABLE nov THEN DO:
        difs.descuadre = difs.descuadre - nov.saldo.
        DELETE nov.
    END.

    DELETE dic.
END.


OUTPUT TO d:\Leonardo\descuadre.csv.
FOR EACH difs WHERE descuadre <> 0 NO-LOCK:
    EXPORT DELIMITER ";" difs.
END.
OUTPUT CLOSE.
