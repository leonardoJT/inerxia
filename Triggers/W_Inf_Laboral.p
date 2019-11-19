TRIGGER PROCEDURE FOR WRITE OF Inf_Laboral
NEW inserted OLD deleted.

    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Inf_Laboral' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.


{valida/W_Inf_Laboral.v}
