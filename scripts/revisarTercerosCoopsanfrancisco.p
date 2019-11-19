DEFINE VAR registro AS CHARACTER.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    INDEX idxNit nit.

INPUT FROM d:\Leonardo\terceros.txt.
REPEAT :
    IMPORT registro.

    FIND FIRST tt WHERE tt.nit = registro NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt THEN DO:
        CREATE tt.
        tt.nit = registro.
    END.
END.
INPUT CLOSE.

OUTPUT TO d:\Leonardo\terceros_out.txt.
FOR EACH tt NO-LOCK:
    EXPORT tt.
END.
OUTPUT CLOSE.
