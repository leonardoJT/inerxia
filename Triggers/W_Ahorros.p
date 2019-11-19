TRIGGER PROCEDURE FOR WRITE OF Ahorros
/*
NEW inserted OLD deleted.

  IF
    /*deleted.Agencia <> inserted.Agencia or */
    deleted.Cod_Ahorro <> inserted.Cod_Ahorro or 
    deleted.Cue_Ahorros <> inserted.Cue_Ahorros
  THEN DO:
    FOR FIRST Mov_Ahorros WHERE (
        /*Mov_Ahorros.Agencia = deleted.Agencia and*/
        Mov_Ahorros.Cod_Ahorro = deleted.Cod_Ahorro and
        Mov_Ahorros.Cue_Ahorros = deleted.Cue_Ahorros) NO-LOCK:
      MESSAGE "No puede Actualizar 'Ahorros' Porque 'Mov_Ahorros' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    /*deleted.Agencia <> inserted.Agencia or */
    deleted.Cod_Ahorro <> inserted.Cod_Ahorro or 
    deleted.Cue_Ahorros <> inserted.Cue_Ahorros
  THEN DO:
    FOR FIRST Che_Blo WHERE (
        /*Che_Blo.Agencia = deleted.Agencia and*/
        Che_Blo.Cod_Producto = deleted.Cod_Ahorro and
        Che_Blo.Cue_Ahorros = deleted.Cue_Ahorros) NO-LOCK:
      MESSAGE "No puede Actualizar 'Ahorros' Porque 'Che_Blo' EXISTE".
      RETURN ERROR.
    END.
  END.

    IF
  
    /*deleted.Agencia <> inserted.Agencia or */
    deleted.Cod_Ahorro <> inserted.Cod_Ahorro or 
    deleted.Cue_Ahorros <> inserted.Cue_Ahorros
  THEN DO:
    FOR FIRST Lib_Chequera WHERE (
  
        /*Lib_Chequera.Agencia = deleted.Agencia and*/
        Lib_Chequera.Cod_Producto = deleted.Cod_Ahorro and
        Lib_Chequera.Cue_Ahorros = deleted.Cue_Ahorros) NO-LOCK:
      MESSAGE "No puede Actualizar 'Ahorros' Porque 'Lib_Chequera' EXISTE".
      RETURN ERROR.
    END.
  END.


      FIND FIRST Clientes WHERE (
  
        inserted.Nit = Clientes.Nit) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Clientes THEN DO:
      MESSAGE "No puede Actualizar 'Ahorros' Porque 'Clientes' NO EXISTE".
      RETURN ERROR.
    END.

    FIND FIRST Pro_Ahorros WHERE (
  
        inserted.Cod_Ahorro = Pro_Ahorros.Cod_Ahorro) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Pro_Ahorros THEN DO:
      MESSAGE "No puede Actualizar 'Ahorros' Porque 'Pro_Ahorros' NO EXISTE".
      RETURN ERROR.
    END.



{valida/W_Ahorros.v}*/
