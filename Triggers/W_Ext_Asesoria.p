TRIGGER PROCEDURE FOR WRITE OF Ext_Asesoria
NEW inserted OLD deleted.

    FIND FIRST Asesoria WHERE (
        inserted.Num_Asesoria = Asesoria.Num_Asesoria) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Asesoria THEN DO:
      MESSAGE "No puede Actualizar 'Ext_Asesoria' Porque 'Asesoria' NO EXISTE".
      RETURN ERROR.
    END.


{valida/W_Ext_Asesoria.v}
