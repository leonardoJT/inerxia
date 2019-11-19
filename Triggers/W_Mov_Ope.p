TRIGGER PROCEDURE FOR WRITE OF Mov_Ope
NEW inserted OLD deleted.


    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Mov_Ope' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.

{valida/W_Mov_Ope.v}
