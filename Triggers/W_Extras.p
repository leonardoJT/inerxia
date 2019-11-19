TRIGGER PROCEDURE FOR WRITE OF Extras
NEW inserted OLD deleted.


    FIND FIRST Solicitud WHERE (
        inserted.Num_Solicitud = Solicitud.Num_Solicitud) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Solicitud THEN DO:
      MESSAGE "No puede Actualizar 'Extras' Porque 'Solicitud' NO EXISTE".
      RETURN ERROR.
    END.



{valida/W_Extras.v}
