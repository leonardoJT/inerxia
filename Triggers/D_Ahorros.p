TRIGGER PROCEDURE FOR DELETE OF Ahorros.

  FOR FIRST Mov_Ahorros WHERE (
      Mov_Ahorros.Agencia = Ahorros.Agencia and
      Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro and
      Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorros) NO-LOCK:
    MESSAGE "No puede Borrar 'Ahorros' Porque 'Mov_Ahorros' EXISTE".
    RETURN ERROR.
  END.

 FOR FIRST Che_Blo WHERE (
      Che_Blo.Agencia = Ahorros.Agencia and
      Che_Blo.Cod_Producto = Ahorros.Cod_Ahorro and
      Che_Blo.Cue_Ahorros = Ahorros.Cue_Ahorros) NO-LOCK:
    MESSAGE "No puede Borrar 'Ahorros' Porque 'Che_Blo' EXISTE".
    RETURN ERROR.
  END.

  FOR FIRST Lib_Chequera WHERE (
      Lib_Chequera.Agencia = Ahorros.Agencia and
      Lib_Chequera.Cod_Producto = Ahorros.Cod_Ahorro and
      Lib_Chequera.Cue_Ahorros = Ahorros.Cue_Ahorros) NO-LOCK:
    MESSAGE "No puede Borrar 'Ahorros' Porque 'Lib_Chequera' EXISTE".
    RETURN ERROR.
  END.

  
