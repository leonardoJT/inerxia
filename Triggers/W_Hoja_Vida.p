TRIGGER PROCEDURE FOR WRITE OF Hoja_Vida
NEW inserted OLD deleted.


    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Hoja_Vida' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.



{valida/W_Hoja_Vida.v}
