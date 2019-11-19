TRIGGER PROCEDURE FOR WRITE OF Ciiu
NEW inserted OLD deleted.


  IF
    deleted.Codigo_CIIU <> inserted.Codigo_CIIU
  THEN DO:
    FOR FIRST Clientes WHERE (
        Clientes.Codigo_CIIU = deleted.Codigo_CIIU) NO-LOCK:
      MESSAGE "No puede Actualizar 'Ciiu' Porque 'Clientes' EXISTE".
      RETURN ERROR.
    END.
  END.


{valida/W_Ciiu.v}
