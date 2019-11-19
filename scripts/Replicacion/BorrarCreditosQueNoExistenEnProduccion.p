DISABLE TRIGGERS FOR LOAD OF bd_web.creditos.

DEFINE VAR cont AS INTEGER.

FOR EACH bd_web.creditos:
    FIND FIRST bdcentral.creditos WHERE bdcentral.creditos.nit = bd_web.creditos.nit
                                    AND bdcentral.creditos.num_credito = bd_web.creditos.num_credito NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bdcentral.creditos THEN DO:
        DELETE bd_web.creditos.
        cont = cont + 1.
    END.
END.

MESSAGE "Fin" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
