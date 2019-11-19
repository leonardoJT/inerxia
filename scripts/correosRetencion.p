DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD email AS CHARACTER.

FOR EACH rep_ahorros WHERE fecCorte = 12/31/2016 NO-LOCK BREAK BY nit:
    IF FIRST-OF(nit) THEN DO:
        CREATE tt.
        tt.nit = rep_ahorros.nit.
    END.
END.

FOR EACH rep_creditos WHERE fecCorte = 12/31/2016 NO-LOCK BREAK BY nit:
    IF FIRST-OF(nit) THEN DO:
        FIND FIRST tt WHERE tt.nit = rep_creditos.nit NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            tt.nit = rep_creditos.nit.
        END.
    END.
END.

FOR EACH tt:
    FIND FIRST clientes WHERE clientes.nit = tt.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        tt.email = clientes.email.
END.

OUTPUT TO d:\Leonardo\CorreosCertificados.csv.
FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.

