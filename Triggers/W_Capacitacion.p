TRIGGER PROCEDURE FOR WRITE OF Capacitacion
NEW inserted OLD deleted.

    FIND FIRST Clientes WHERE (
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Capacitacion' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.

{valida/W_Capacitacion.v}
