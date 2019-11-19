TRIGGER PROCEDURE FOR DELETE OF Deducible.
  FOR FIRST Operacion WHERE (
      Operacion.Cod_Deducible = Deducible.Cod_Deducible) NO-LOCK:
    MESSAGE "No puede Borrar 'Deducible' Porque 'Operacion' EXISTE".
    RETURN ERROR.
  END.
  
  FOR FIRST Operacion WHERE (
      Operacion.Comision = Deducible.Cod_Deducible) NO-LOCK:
    MESSAGE "No puede Borrar 'Deducible' Porque 'Operacion' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Bancos WHERE (
      Bancos.Cod_RemNego = Deducible.Cod_Deducible) NO-LOCK:
    MESSAGE "No puede Borrar 'Deducible' Porque 'Bancos' EXISTE".
    RETURN ERROR.
  END.

 FOR FIRST Bancos WHERE (
      Bancos.Cod_RemCobro = Deducible.Cod_Deducible) NO-LOCK:
    MESSAGE "No puede Borrar 'Deducible' Porque 'Bancos' EXISTE".
    RETURN ERROR.
  END.


