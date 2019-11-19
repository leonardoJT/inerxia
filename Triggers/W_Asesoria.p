TRIGGER PROCEDURE FOR WRITE OF Asesoria
NEW inserted OLD deleted.
IF
    deleted.Num_Asesoria <> inserted.Num_Asesoria
THEN DO:
    FOR FIRST Solicitud WHERE (
      Solicitud.Num_Asesoria = deleted.Num_Asesoria) NO-LOCK:
      MESSAGE "No puede Actualizar 'Asesoria' Porque 'Solicitud' EXISTE".
      RETURN ERROR.
    END.
END.



{valida/W_Asesoria.v}
