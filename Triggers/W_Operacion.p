TRIGGER PROCEDURE FOR WRITE OF Operacion
NEW inserted OLD deleted.
  IF
    deleted.Cod_Operacion <> inserted.Cod_Operacion
  THEN DO:
    FOR FIRST Mov_Especiales WHERE (
        Mov_Especiales.Cod_Operacion = deleted.Cod_Operacion) NO-LOCK:
      MESSAGE "No puede Actualizar 'Operacion' Porque 'Mov_Especiales' EXISTE".
      RETURN ERROR.
    END.
  END.
  
  IF
    deleted.Cod_Operacion <> inserted.Cod_Operacion
  THEN DO:
    FOR FIRST Taquilla WHERE (
        Taquilla.Cod_Operacion = deleted.Cod_Operacion) NO-LOCK:
      MESSAGE "No puede Actualizar 'Operacion' Porque 'Taquilla' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Cod_Operacion <> inserted.Cod_Operacion
  THEN DO:
    FOR FIRST Mov_Creditos WHERE (
        Mov_Creditos.Cod_Operacion = deleted.Cod_Operacion) NO-LOCK:
      MESSAGE "No puede Actualizar 'Operacion' Porque 'Mov_Creditos' EXISTE".
      RETURN ERROR.
    END.
  END.

  IF
    deleted.Cod_Operacion <> inserted.Cod_Operacion
  THEN DO:
    FOR FIRST Res_Operacion WHERE (
        Res_Operacion.Cod_Operacion = deleted.Cod_Operacion) NO-LOCK:
      MESSAGE "No puede Actualizar 'Operacion' Porque 'Ope_RstGrp' EXISTE".
      RETURN ERROR.
    END.
  END.



  IF
    deleted.Cod_Operacion <> inserted.Cod_Operacion
  THEN DO:
    FOR FIRST Mov_Ahorros WHERE (
        Mov_Ahorros.Cod_Operacion = deleted.Cod_Operacion) NO-LOCK:
      MESSAGE "No puede Actualizar 'Operacion' Porque 'Mov_Ahorros' EXISTE".
      RETURN ERROR.
    END.
  END.

    IF inserted.Cod_Deducible EQ ""
    OR inserted.Cod_Deducible EQ ?
    OR inserted.Cod_Deducible EQ STRING(0) THEN
       RETURN.

    FIND FIRST Deducible WHERE (
        inserted.Cod_Deducible = Deducible.Cod_Deducible) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Deducible THEN DO:
      MESSAGE "No puede Actualizar 'Operacion' Porque 'Deducible' NO EXISTE".
      RETURN ERROR.
    END.




    IF inserted.Comision EQ ""
    OR inserted.Comision EQ ?
    OR inserted.Comision EQ STRING(0) THEN
       RETURN.

    FIND FIRST Deducible WHERE (
        inserted.Comision = Deducible.Cod_Deducible) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Deducible THEN DO:
      MESSAGE "No puede Actualizar 'Operacion' Porque 'Deducible' NO EXISTE".
      RETURN ERROR.
    END.






{valida/W_Operacion.v}
