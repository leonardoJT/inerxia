TRIGGER PROCEDURE FOR WRITE OF Inf_Familiar
NEW inserted OLD deleted.

    FIND FIRST Terceros WHERE (
        inserted.Nit_Familiar = Terceros.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Terceros THEN DO:
      MESSAGE "No puede Actualizar 'Inf_Familiar' Porque 'Terceros' NO EXISTE".
      RETURN ERROR.
    END.


    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Inf_Familiar' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.


{valida/W_Inf_Familiar.v}
