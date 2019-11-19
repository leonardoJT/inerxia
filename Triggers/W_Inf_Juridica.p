TRIGGER PROCEDURE FOR WRITE OF Inf_Juridica
NEW inserted OLD deleted.


    FIND FIRST Terceros WHERE (
        inserted.Nit_RepLegal = Terceros.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Terceros THEN DO:
      MESSAGE "No puede Actualizar 'Inf_Juridica' Porque 'Terceros' NO EXISTE".
      RETURN ERROR.
    END.


    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Inf_Juridica' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.


{valida/W_Inf_Juridica.v}
