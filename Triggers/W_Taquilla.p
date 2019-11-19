TRIGGER PROCEDURE FOR WRITE OF Taquilla
NEW inserted OLD deleted.
    FIND FIRST Operacion WHERE (inserted.Cod_Operacion = Operacion.Cod_Operacion) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Operacion THEN DO:
      MESSAGE "No puede Actualizar 'Taquilla' Porque 'Operacion'  NO EXISTE" inserted.Cod_Operacion.
      RETURN ERROR.
    END.


 
