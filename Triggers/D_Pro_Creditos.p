TRIGGER PROCEDURE FOR DELETE OF Pro_Creditos.

  FOR FIRST Asesoria WHERE (
      Asesoria.Cod_Producto = Pro_Creditos.Cod_Credito) NO-LOCK:
    MESSAGE "No puede Borrar 'Pro_Creditos' Porque 'Asesoria' EXISTE".
    RETURN ERROR.
  END.

 FOR FIRST Solicitud WHERE (
      Solicitud.Cod_Credito = Pro_Creditos.Cod_Credito) NO-LOCK:
    MESSAGE "No puede Borrar 'Pro_Creditos' Porque 'Solicitud' EXISTE".
    RETURN ERROR.
  END.

 FOR FIRST Creditos WHERE (
     Creditos.Cod_Credito = Pro_Creditos.Cod_Credito) NO-LOCK:
    MESSAGE "No puede Borrar 'Pro_Creditos' Porque 'Creditos' EXISTE".
    RETURN ERROR.
  END.

