TRIGGER PROCEDURE FOR DELETE OF Asesoria.

  FOR FIRST Solicitud WHERE (
    
      Solicitud.Num_Asesoria = Asesoria.Num_Asesoria) NO-LOCK:
    MESSAGE "No puede Borrar 'Asesoria' Porque 'Solicitud' EXISTE".
    RETURN ERROR.
  END.


