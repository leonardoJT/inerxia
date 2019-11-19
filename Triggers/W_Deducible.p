TRIGGER PROCEDURE FOR WRITE OF Deducible
NEW inserted OLD deleted.


  IF
    deleted.Cod_Deducible <> inserted.Cod_Deducible
  THEN DO:
    FOR FIRST Operacion WHERE (
        Operacion.Cod_Deducible = deleted.Cod_Deducible) NO-LOCK:
      MESSAGE "No puede Actualizar 'Deducible' Porque 'Operacion' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Cod_Deducible <> inserted.Cod_Deducible
  THEN DO:
    FOR FIRST Operacion WHERE (
        Operacion.Comision = deleted.Cod_Deducible) NO-LOCK:
      MESSAGE "No puede Actualizar 'Deducible' Porque 'Operacion' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Cod_Deducible <> inserted.Cod_Deducible
  THEN DO:
    FOR FIRST Bancos WHERE (
        Bancos.Cod_RemNego = deleted.Cod_Deducible) NO-LOCK:
      MESSAGE "No puede Actualizar 'Deducible' Porque 'Bancos' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Cod_Deducible <> inserted.Cod_Deducible
  THEN DO:
    FOR FIRST Bancos WHERE (
        Bancos.Cod_RemCobro = deleted.Cod_Deducible) NO-LOCK:
      MESSAGE "No puede Actualizar 'Deducible' Porque 'Bancos' EXISTE".
      RETURN ERROR.
    END.
  END.


{valida/W_Deducible.v}
