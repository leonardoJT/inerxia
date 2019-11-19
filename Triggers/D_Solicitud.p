TRIGGER PROCEDURE FOR DELETE OF Solicitud.
FOR FIRST Garantias WHERE (
      Garantias.Num_Solicitud = Solicitud.Num_Solicitud) NO-LOCK:
    MESSAGE "No puede Borrar 'Solicitud' Porque 'Garantias' EXISTE".
    RETURN ERROR.
  END.




  FOR FIRST Creditos WHERE (
      Creditos.Num_Solicitud = Solicitud.Num_Solicitud) AND
        Creditos.Nit = Solicitud.Nit NO-LOCK:
    MESSAGE "No puede Borrar 'Solicitud' Porque 'Creditos' EXISTE".
    RETURN ERROR.
  END.



