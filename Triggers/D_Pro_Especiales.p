TRIGGER PROCEDURE FOR DELETE OF Pro_Especiales.
FOR FIRST Especiales WHERE (
      Especiales.Oficina = Pro_Especiales.Oficina and
      Especiales.Cod_Producto = Pro_Especiales.Cod_Producto) NO-LOCK:
    MESSAGE "No puede Borrar 'Pro_Especiales' Porque 'Especiales' EXISTE".
    RETURN ERROR.
  END.

