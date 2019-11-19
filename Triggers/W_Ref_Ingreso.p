TRIGGER PROCEDURE FOR WRITE OF Ref_Ingreso
NEW inserted OLD deleted.


    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Ref_Ingreso' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.

{valida/W_Ref_Ingreso.v}
