TRIGGER PROCEDURE FOR WRITE OF Res_Operacion
NEW inserted OLD deleted.


    FIND FIRST Operacion WHERE (
        inserted.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Operacion THEN DO:
      MESSAGE "No puede Actualizar 'Res_Operacion' Porque 'Operacion' NO EXISTE".
      RETURN ERROR.
    END.

