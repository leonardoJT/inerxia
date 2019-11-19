TRIGGER PROCEDURE FOR DELETE OF Ciiu.

  

  FOR FIRST Clientes WHERE (
      
      Clientes.Codigo_CIIU = Ciiu.Codigo_CIIU) NO-LOCK:
    MESSAGE "No puede Borrar 'Ciiu' Porque 'Clientes' EXISTE".
    RETURN ERROR.
  END.





