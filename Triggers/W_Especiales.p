TRIGGER PROCEDURE FOR WRITE OF Especiales
NEW inserted OLD deleted.


  IF
    deleted.agencia <> inserted.agencia or 
    deleted.Cod_Producto <> inserted.Cod_Producto or 
    deleted.Nit <> inserted.Nit
  THEN DO:
    FOR FIRST Mov_Especiales WHERE (
        Mov_Especiales.agencia = deleted.agencia and
        Mov_Especiales.Cod_Producto = deleted.Cod_Producto and
        Mov_Especiales.Nit = deleted.Nit) NO-LOCK:
      MESSAGE "No puede Actualizar 'Especiales' Porque 'Mov_Especiales' EXISTE".
      RETURN ERROR.
    END.
  END.


    FIND FIRST Pro_Especiales WHERE (
        inserted.agencia = Pro_Especiales.agencia and
        inserted.Cod_Producto = Pro_Especiales.Cod_Producto) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Pro_Especiales THEN DO:
      MESSAGE "No puede Actualizar 'Especiales' Porque 'Pro_Especiales' NO EXISTE".
      RETURN ERROR.
    END.



{valida/W_Especiales.v}
