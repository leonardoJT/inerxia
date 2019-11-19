DEFINE TEMP-TABLE puc
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreNIIF AS CHARACTER.

INPUT FROM d:\Leonardo\niif\pucNIIFparaCUB.csv.
REPEAT :
    CREATE puc.
    IMPORT DELIMITER ";" puc.
END.
INPUT CLOSE.

FOR EACH puc WHERE cuentaNIIF = "":
    DELETE puc.
END.

DEFINE TEMP-TABLE pucCub
    FIELD cuenta AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD cuentaNIIF AS CHARACTER
    FIELD nombreNIIF AS CHARACTER.

INPUT FROM d:\Leonardo\niif\pucCUB.csv.
REPEAT :
    CREATE pucCUB.
    IMPORT DELIMITER ";" pucCUB.
END.
INPUT CLOSE.

FOR EACH pucCUB:
    FIND FIRST puc WHERE puc.cuenta = pucCUB.cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE puc THEN DO:
        pucCUB.cuentaNIIF = puc.cuentaNIIF.
        pucCUB.nombreNIIF = pucCUB.nombre.
    END.
END.

OUTPUT TO d:\Leonardo\NIIF\pucHomologadoCUB.csv.
EXPORT DELIMITER ";" "Cuenta_PUC" "Nombre_PUC" "Cuenta_NIIF" "Nombre_NIIF".
FOR EACH pucCUB NO-LOCK:
    EXPORT DELIMITER ";" pucCUB.
END.
OUTPUT CLOSE.
