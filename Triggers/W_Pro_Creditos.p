TRIGGER PROCEDURE FOR WRITE OF Pro_Creditos
NEW inserted OLD deleted.
/*
   IF
    deleted.Cod_Credito <> inserted.Cod_Credito
  THEN DO:
    FOR FIRST Solicitud WHERE (
       Solicitud.Cod_Credito = deleted.Cod_Credito) NO-LOCK:
      MESSAGE "No puede Actualizar 'Pro_Creditos' Porque 'Solicitud' EXISTE".
      RETURN ERROR.
    END.
  END.


  IF
    deleted.Cod_credito <> inserted.Cod_credito
  THEN DO:
    FOR FIRST Creditos WHERE (
        Creditos.Cod_Credito = deleted.Cod_Credito) NO-LOCK:
      MESSAGE "No puede Actualizar 'Pro_Creditos' Porque 'Creditos' EXISTE".
      RETURN ERROR.
    END.
  END.

  */

{valida/W_Pro_Creditos.v}
