TRIGGER PROCEDURE FOR WRITE OF Varios
    NEW inserted OLD deleted.

{INCLUIDO\variable.i "shared"}

DEF VAR W_Rpta AS LOGICAL.

FIND FIRST Cfg_varios WHERE Cfg_Varios.Tipo = Inserted.Tipo NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Cfg_Varios) THEN DO:
    RUN MostrarMensaje IN W_Manija (INPUT 64, OUTPUT W_Rpta).
    RETURN ERROR.
END.

{valida/W_Varios.v}

