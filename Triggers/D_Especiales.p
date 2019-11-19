TRIGGER PROCEDURE FOR DELETE OF Especiales.


  FOR FIRST Mov_Especiales WHERE (
      Mov_Especiales.Oficina = Especiales.Oficina and
      Mov_Especiales.Cod_Producto = Especiales.Cod_Producto and
      Mov_Especiales.Nit = Especiales.Nit and
      Mov_Especiales.Secuencia = Especiales.Secuencia) NO-LOCK:
    MESSAGE "No puede Borrar 'Especiales' Porque 'Mov_Especiales' EXISTE".
    RETURN ERROR.
  END.
