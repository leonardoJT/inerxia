TRIGGER PROCEDURE FOR WRITE OF Ope_RstUsu
NEW inserted OLD deleted.


    FIND FIRST Operacion WHERE (
        inserted.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Operacion THEN DO:
      MESSAGE "No puede Actualizar 'Ope_RstUsu' Porque 'Operacion' NO EXISTE".
      RETURN ERROR.
    END.


{valida/W_Ope_RstUsu.v}
