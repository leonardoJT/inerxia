TRIGGER PROCEDURE FOR DELETE OF Operacion.

  FOR FIRST Mov_Especiales WHERE (
      Mov_Especiales.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK:
    MESSAGE "No puede Borrar 'Operacion' Porque 'Mov_Especiales' EXISTE".
    RETURN ERROR.
  END.

 FOR FIRST Taquilla WHERE (
      Taquilla.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK:
    MESSAGE "No puede Borrar 'Operacion' Porque 'Taquilla' EXISTE".
    RETURN ERROR.
  END.

 FOR FIRST Mov_Creditos WHERE (
      Mov_Creditos.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK:
    MESSAGE "No puede Borrar 'Operacion' Porque 'Mov_Creditos' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Ope_RstGrp WHERE (
      Ope_RstGrp.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK:
    MESSAGE "No puede Borrar 'Operacion' Porque 'Ope_RstGrp' EXISTE".
    RETURN ERROR.
  END.

 FOR FIRST Ope_RstOfi WHERE (
      Ope_RstOfi.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK:
    MESSAGE "No puede Borrar 'Operacion' Porque 'Ope_RstOfi' EXISTE".
    RETURN ERROR.
  END.

   FOR FIRST Ope_RstUsu WHERE (
      Ope_RstUsu.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK:
    MESSAGE "No puede Borrar 'Operacion' Porque 'Ope_RstUsu' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Mov_Ahorros WHERE (
      Mov_Ahorros.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK:
    MESSAGE "No puede Borrar 'Operacion' Porque 'Mov_Ahorros' EXISTE".
    RETURN ERROR.
  END.

