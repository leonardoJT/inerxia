TRIGGER PROCEDURE FOR WRITE OF Pro_Ahorros

NEW inserted OLD deleted.

 
  IF
  deleted.Cod_Ahorro <> inserted.Cod_ahorro
  THEN DO:
    FOR FIRST Ahorros WHERE (
        Ahorros.Cod_Ahorro = deleted.Cod_Ahorro) NO-LOCK:
      MESSAGE "No puede Actualizar 'Pro_Ahorros' Porque 'Ahorros' EXISTE".
      RETURN ERROR.
    END.
  END.

{valida/W_Pro_Ahorros.v}
