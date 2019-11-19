TRIGGER PROCEDURE FOR WRITE OF Mov_Ahorros
/*
NEW inserted OLD deleted.
    FIND FIRST Operacion WHERE (
     
        inserted.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Operacion THEN DO:
      MESSAGE "No puede Actualizar 'Mov_Ahorros' Porque 'Operacion' NO EXISTE".
      RETURN ERROR.
    END.



{valida/W_Mov_Ahorros.v}
  */
