DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF anexos13.

DEFINE VAR cont AS INTEGER.
DEFINE VAR vCuenta AS CHARACTER INITIAL "11200522".

FOR EACH anexos WHERE anexos.cuenta = vCuenta:
    DELETE anexos.
    cont = cont + 1.
END.

FOR EACH anexos13 WHERE anexos13.cuenta = vCuenta:
    DELETE anexos13.
END.

MESSAGE cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
